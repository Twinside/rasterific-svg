{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TupleSections #-}
module Graphics.Rasterific.Svg.RasterificRender
    ( DrawResult( .. )
    , renderSvgDocument
    , drawingOfSvgDocument
    ) where

import Data.Monoid( Last( .. ), mempty, (<>) )
import Data.Maybe( fromMaybe  )
import Data.Word( Word8 )
import Control.Monad( foldM )
import Control.Monad.IO.Class( liftIO )
import Control.Monad.Trans.State.Strict( modify, runStateT )
import Control.Applicative( (<$>) )
import qualified Codec.Picture as CP
import Codec.Picture( PixelRGBA8( .. )
                    , PixelRGB16( .. )
                    , PixelRGBA16( .. )
                    , PixelRGBF( .. )
                    , PixelYA16( .. )
                    , PixelCMYK8
                    , PixelYCbCr8
                    , PixelRGB8
                    , DynamicImage( .. )
                    , pixelMap
                    , readImage
                    )
import Codec.Picture.Types( promoteImage
                          , promotePixel
                          , convertPixel
                          )

import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Graphics.Rasterific as R
import Graphics.Rasterific.Linear( V2( V2 ), (^-^), zero )
import Graphics.Rasterific.Outline
import qualified Graphics.Rasterific.Transformations as RT
import Graphics.Text.TrueType
import Graphics.Svg.Types hiding ( Dpi )
import Graphics.Rasterific.Svg.PathConverter
import Graphics.Rasterific.Svg.RenderContext
import Graphics.Rasterific.Svg.RasterificTextRendering

{-import Debug.Trace-}
{-import Text.Printf-}
{-import Text.Groom-}

-- | Represent a Rasterific drawing with the associated
-- image size.
data DrawResult = DrawResult
    { -- | Rasterific drawing, can be reused and composed
      -- with other elements for scene drawing.
      _drawAction :: R.Drawing PixelRGBA8 ()
      -- | Supposed drawing width of the drawing, ideally
      -- represent the final image width.
    , _drawWidth :: {-# UNPACK #-}!Int
      -- | Supposed drawing height of the drawing, ideally
      -- represent the final image height.
    , _drawHeight :: {-# UNPACK #-}!Int
    }

renderSvgDocument :: FontCache -> Maybe (Int, Int) -> Dpi -> Document
                  -> IO (CP.Image PixelRGBA8, LoadedElements)
renderSvgDocument cache sizes dpi doc = do
  (drawing, loaded) <- drawingOfSvgDocument cache sizes dpi doc
  let color = PixelRGBA8 0 0 0 0
      img = R.renderDrawing (_drawWidth drawing) (_drawHeight drawing) color
          $ _drawAction drawing
  return (img, loaded)

drawingOfSvgDocument :: FontCache -> Maybe (Int, Int) -> Dpi -> Document
                     -> IO (DrawResult, LoadedElements)
drawingOfSvgDocument cache sizes dpi doc = case sizes of
    Just s -> renderAtSize s
    Nothing -> renderAtSize $ documentSize dpi doc
  where
    uuWidth = toUserUnit dpi <$> _width doc
    uuHeight = toUserUnit dpi <$> _height doc
    (x1, y1, x2, y2) = case (_viewBox doc, uuWidth, uuHeight) of
        (Just v,      _,      _) -> v
        (     _, Just (Num w), Just (Num h)) -> (0, 0, floor w, floor h)
        _                        -> (0, 0, 1, 1)

    box = (V2 (fromIntegral x1) (fromIntegral y1),
           V2 (fromIntegral x2) (fromIntegral y2))
    emptyContext = RenderContext
        { _renderViewBox = box
        , _initialViewBox = box
        , _contextDefinitions = _definitions doc
        , _fontCache = cache
        , _renderDpi = dpi
        , _subRender = subRenderer
        }

    subRenderer subDoc = do
       (drawing, loaded) <-
           liftIO $ renderSvgDocument cache Nothing dpi subDoc
       modify (<> loaded)
       return drawing

    sizeFitter (V2 0 0, V2 vw vh) (actualWidth, actualHeight)
      | aw /= vw || vh /= ah =
            R.withTransformation (RT.scale (aw / vw) (ah / vh))
           where
             aw = fromIntegral actualWidth
             ah = fromIntegral actualHeight
    sizeFitter (V2 0 0, _) _ = id
    sizeFitter (p@(V2 xs ys), V2 xEnd yEnd) actualSize =
        R.withTransformation (RT.translate (negate p)) .
            sizeFitter (zero, V2 (xEnd - xs) (yEnd - ys)) actualSize

    renderAtSize (w, h) = do
      let stateDraw = mapM (renderSvg emptyContext) $ _elements doc
      (elems, s) <- runStateT stateDraw mempty
      let drawing = sizeFitter box (w, h) $ sequence_ elems
      return (DrawResult drawing w h, s)

withInfo :: (Monad m, Monad m2)
         => (a -> Maybe b) -> a -> (b -> m (m2 ())) -> m (m2 ())
withInfo accessor val action =
    case accessor val of
       Nothing -> return $ return ()
       Just v -> action v

toTransformationMatrix :: Transformation -> RT.Transformation
toTransformationMatrix = go where
  go (TransformMatrix a b c d e f) =
     RT.Transformation a b c d e f
  go (Translate x y) = RT.translate $ V2 x y
  go (Scale xs Nothing) = RT.scale xs xs
  go (Scale xs (Just ys)) = RT.scale xs ys
  go (Rotate angle Nothing) =
      RT.rotate $ toRadian angle
  go (Rotate angle (Just (cx, cy))) =
      RT.rotateCenter (toRadian angle) $ V2 cx cy
  go (SkewX v) = RT.skewX $ toRadian v
  go (SkewY v) = RT.skewY $ toRadian v
  go TransformUnknown = mempty

withTransform :: DrawAttributes -> R.Drawing a ()
              -> R.Drawing a ()
withTransform trans draw =
    case _transform trans of
       Nothing -> draw
       Just t -> R.withTransformation fullTrans draw
         where fullTrans = F.foldMap toTransformationMatrix t

withSvgTexture :: RenderContext -> DrawAttributes
               -> Texture -> Float
               -> [R.Primitive]
               -> IODraw (R.Drawing PixelRGBA8 ())
withSvgTexture ctxt attr texture opacity prims = do
  mayTexture <- prepareTexture ctxt attr texture opacity prims
  case mayTexture of
    Nothing -> return $ return ()
    Just tex ->
      let method = fillMethodOfSvg attr in
      return . R.withTexture tex $ R.fillWithMethod method prims

filler :: RenderContext
       -> DrawAttributes
       -> [R.Primitive]
       -> IODraw (R.Drawing PixelRGBA8 ())
filler ctxt info primitives =
  withInfo (getLast . _fillColor) info $ \svgTexture ->
    let opacity = fromMaybe 1.0 $ _fillOpacity info in
    withSvgTexture ctxt info svgTexture opacity primitives

stroker :: RenderContext -> DrawAttributes -> [R.Primitive]
        -> IODraw (R.Drawing PixelRGBA8 ())
stroker ctxt info primitives =
  withInfo (getLast . _strokeWidth) info $ \swidth ->
    withInfo (getLast . _strokeColor) info $ \svgTexture ->
      let toFloat = lineariseLength ctxt info
          realWidth = toFloat swidth
          dashOffsetStart =
              maybe 0 toFloat . getLast $ _strokeOffset info
          primsList = case getLast $ _strokeDashArray info of
            Just pat ->
                dashedStrokize dashOffsetStart (toFloat <$> pat)
                  realWidth (joinOfSvg info) (capOfSvg info) primitives
            Nothing ->
              [strokize realWidth (joinOfSvg info) (capOfSvg info) primitives]
          opacity = fromMaybe 1.0 $ _strokeOpacity info
          strokerAction acc prims =
           (acc <>) <$>
               withSvgTexture ctxt info svgTexture opacity prims
            
      in
      foldM strokerAction mempty primsList

mergeContext :: RenderContext -> DrawAttributes -> RenderContext
mergeContext ctxt _attr = ctxt

viewBoxOfTree :: Tree -> Maybe (Int, Int, Int, Int)
viewBoxOfTree (SymbolTree g) = _groupViewBox g
viewBoxOfTree _ = Nothing

geometryOfNamedElement :: RenderContext -> String -> Tree
geometryOfNamedElement ctxt str =
  maybe None extractGeometry . M.lookup str $ _contextDefinitions ctxt
  where
    extractGeometry e = case e of
      ElementLinearGradient _ -> None
      ElementRadialGradient _ -> None
      ElementPattern _ -> None
      ElementMarker _ -> None
      ElementGeometry g -> g

imgToPixelRGBA8 :: DynamicImage -> CP.Image PixelRGBA8
imgToPixelRGBA8 img = case img of
  ImageY8 i -> promoteImage i
  ImageY16 i ->
    pixelMap (\y -> let v = w2b y in PixelRGBA8 v v v 255) i
  ImageYF i ->
    pixelMap (\f -> let v = f2b f in PixelRGBA8 v v v 255) i
  ImageYA8 i -> promoteImage i
  ImageYA16 i ->
      pixelMap (\(PixelYA16 y a) -> let v = w2b y in PixelRGBA8 v v v (w2b a)) i
  ImageRGB8 i -> promoteImage i
  ImageRGB16 i -> pixelMap rgb162Rgba8 i
  ImageRGBF i -> pixelMap rgbf2rgba8 i
  ImageRGBA8 i -> i
  ImageRGBA16 i -> pixelMap rgba162Rgba8 i
  ImageYCbCr8 i -> pixelMap (promotePixel . yCbCr2Rgb) i
  ImageCMYK8 i -> pixelMap (promotePixel . cmyk2Rgb) i
  ImageCMYK16 i -> pixelMap (rgb162Rgba8 . convertPixel) i
  where
    yCbCr2Rgb :: PixelYCbCr8 -> PixelRGB8
    yCbCr2Rgb = convertPixel

    cmyk2Rgb :: PixelCMYK8 -> PixelRGB8
    cmyk2Rgb = convertPixel

    w2b v = fromIntegral $ v `div` 257
    f2b :: Float -> Word8
    f2b v = floor . max 0 . min 255 $ v * 255

    rgbf2rgba8 (PixelRGBF r g b) =
      PixelRGBA8 (f2b r) (f2b g) (f2b b) 255
    rgba162Rgba8 (PixelRGBA16 r g b a) =
      PixelRGBA8 (w2b r) (w2b g) (w2b b) (w2b a)
    rgb162Rgba8 (PixelRGB16 r g b)=
      PixelRGBA8 (w2b r) (w2b g) (w2b b) 255


renderImage :: RenderContext -> DrawAttributes -> Image
            -> IODraw (R.Drawing PixelRGBA8 ())
renderImage ctxt attr imgInfo = do
  eimg <- liftIO . readImage $ _imageHref imgInfo
  let srect = RectangleTree $ defaultSvg
        { _rectUpperLeftCorner = _imageCornerUpperLeft imgInfo
        , _rectDrawAttributes = _imageDrawAttributes imgInfo
        , _rectWidth = _imageWidth imgInfo
        , _rectHeight = _imageHeight imgInfo
        }

  case eimg of
    Left _ -> renderTree ctxt attr srect
    Right img -> do
      let pAttr = _imageDrawAttributes imgInfo
          info = attr <> pAttr
          context' = mergeContext ctxt pAttr
          p' = linearisePoint context' info $ _imageCornerUpperLeft imgInfo
          w' = lineariseXLength context' info $ _imageWidth imgInfo
          h' = lineariseYLength context' info $ _imageHeight imgInfo
          filling = R.drawImageAtSize (imgToPixelRGBA8 img) 0 p' w' h'
      stroking <- stroker context' info $ R.rectangle p' w' h'
      return . withTransform pAttr $ filling <> stroking

renderSvg :: RenderContext -> Tree -> IODraw (R.Drawing PixelRGBA8 ())
renderSvg initialContext = renderTree initialContext initialAttr
  where
    initialAttr =
      mempty { _strokeWidth = Last . Just $ Num 1.0
             , _strokeLineCap = Last $ Just CapButt
             , _strokeLineJoin = Last $ Just JoinMiter
             , _strokeMiterLimit = Last $ Just 4.0
             , _strokeOpacity = Just 1.0
             , _fillColor = Last . Just . ColorRef $ PixelRGBA8 0 0 0 255
             , _fillOpacity = Just 1.0
             , _fillRule = Last $ Just FillNonZero
             , _fontSize = Last . Just $ Num 16
             , _textAnchor = Last $ Just TextAnchorStart
             }

fitUse :: RenderContext -> DrawAttributes -> Use -> Tree -> R.Drawing px ()
       -> R.Drawing px ()
fitUse ctxt attr uses subTree =
  let origin = linearisePoint ctxt attr $ _useBase uses
      w = lineariseXLength ctxt attr <$> _useWidth uses
      h = lineariseYLength ctxt attr <$> _useHeight uses
  in
  case viewBoxOfTree subTree of
    Nothing -> R.withTransformation (RT.translate origin)
    (Just (xs, ys, xe, ye)) ->
      let boxOrigin = V2 (fromIntegral xs) (fromIntegral ys)
          boxEnd = V2 (fromIntegral xe) (fromIntegral ye)
          V2 bw bh = abs $ boxEnd ^-^ boxOrigin
          xScaleFactor = case w of
            Just wpx -> wpx / bw
            Nothing -> 1.0
          yScaleFactor = case h of
            Just hpx -> hpx / bh
            Nothing -> 1.0
      in
      R.withTransformation $ RT.translate origin
                          <> RT.scale xScaleFactor yScaleFactor
                          <> RT.translate (negate boxOrigin)


renderTree :: RenderContext -> DrawAttributes -> Tree -> IODraw (R.Drawing PixelRGBA8 ())
renderTree = go where
    go _ _ None = return mempty
    go ctxt attr (TextTree tp stext) = renderText ctxt attr tp stext
    go ctxt attr (ImageTree i) = renderImage ctxt attr i
    go ctxt attr (UseTree useData (Just subTree)) = do
      sub <- go ctxt attr' subTree
      return . fitUse ctxt attr useData subTree
             $ withTransform pAttr sub
      where
        pAttr = _useDrawAttributes useData
        attr' = attr <> pAttr

    go ctxt attr (UseTree useData Nothing) = do
      sub <- go ctxt attr' subTree
      return . fitUse ctxt attr useData subTree
             $ withTransform pAttr sub
      where
        pAttr = _useDrawAttributes useData
        attr' = attr <> pAttr
        subTree = geometryOfNamedElement ctxt $ _useName useData

    go ctxt attr (SymbolTree g) = go ctxt attr $ GroupTree g
    go ctxt attr (GroupTree (Group groupAttr subTrees _)) = do
        subTrees' <- mapM (go context' attr') subTrees
        return . withTransform groupAttr $ sequence_ subTrees'
      where attr' = attr <> groupAttr
            context' = mergeContext ctxt groupAttr

    go ctxt attr (RectangleTree (Rectangle pAttr p w h (rx, ry))) = do
      let info = attr <> pAttr
          context' = mergeContext ctxt pAttr
          p' = linearisePoint context' info p
          w' = lineariseXLength context' info w
          h' = lineariseYLength context' info h

          rx' = lineariseXLength context' info rx
          ry' = lineariseXLength context' info ry
          rect = case (rx', ry') of
            (0, 0) -> R.rectangle p' w' h'
            (v, 0) -> R.roundedRectangle p' w' h' v v
            (0, v) -> R.roundedRectangle p' w' h' v v
            (vx, vy) -> R.roundedRectangle p' w' h' vx vy

      filling <- filler context' info rect
      stroking <- stroker context' info rect
      return . withTransform pAttr $ filling <> stroking

    go ctxt attr (CircleTree (Circle pAttr p r)) = do
      let info = attr <> pAttr
          context' = mergeContext ctxt pAttr
          p' = linearisePoint context' info p
          r' = lineariseLength context' info r
          c = R.circle p' r'
      filling <- filler context' info c
      stroking <- stroker context' info c
      return . withTransform pAttr $ filling <> stroking

    go ctxt attr (EllipseTree (Ellipse pAttr p rx ry)) = do
      let info = attr <> pAttr
          context' = mergeContext ctxt pAttr
          p' = linearisePoint context' info p
          rx' = lineariseXLength context' info rx
          ry' = lineariseYLength context' info ry
          c = R.ellipse p' rx' ry'
      filling <- filler context' info c
      stroking <- stroker context' info c
      return . withTransform pAttr $ filling <> stroking

    go ctxt attr (PolyLineTree (PolyLine pAttr points)) =
      go ctxt (dropFillColor attr)
            . PathTree . Path (dropFillColor pAttr)
            $ toPath points
      where
        dropFillColor v = v { _fillColor = Last Nothing }
        toPath [] = []
        toPath (x:xs) =
            [ MoveTo OriginAbsolute [x]
            , LineTo OriginAbsolute xs
            ]

    go ctxt attr (PolygonTree (Polygon pAttr points)) =
      go ctxt attr . PathTree . Path pAttr $ toPath points
      where
        toPath [] = []
        toPath (x:xs) =
            [ MoveTo OriginAbsolute [x]
            , LineTo OriginAbsolute xs
            , EndPath
            ]

    go ctxt attr (LineTree (Line pAttr p1 p2)) = do
      let info = attr <> pAttr
          context' = mergeContext ctxt pAttr
          p1' = linearisePoint context' info p1
          p2' = linearisePoint context' info p2
      stroking <- stroker context' info $ R.line p1' p2'
      return $ withTransform pAttr stroking

    go ctxt attr (PathTree (Path pAttr p)) = do
      let info = attr <> pAttr
          strokePrimitives = svgPathToPrimitives False p
          fillPrimitives = svgPathToPrimitives True p
      filling <- filler ctxt info fillPrimitives
      stroking <- stroker ctxt info strokePrimitives
      return . withTransform pAttr $ filling <> stroking

