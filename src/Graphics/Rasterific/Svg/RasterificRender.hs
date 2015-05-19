{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}
module Graphics.Rasterific.Svg.RasterificRender
    ( DrawResult( .. )
    , renderSvgDocument
    , drawingOfSvgDocument
    , pdfOfSvgDocument
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( (<$>) )
import Data.Monoid( mempty, mconcat )
#endif

import Data.Monoid( Last( .. ), (<>) )
import Data.Maybe( fromMaybe  )
import Data.Word( Word8 )
import Control.Monad( foldM )
import Control.Monad.IO.Class( liftIO )
import Control.Monad.Trans.State.Strict( modify, runStateT )
import Control.Lens( (&), (.~) )
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

import qualified Data.ByteString.Lazy as LB
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Graphics.Rasterific as R
import System.FilePath( (</>), dropFileName )
import Graphics.Rasterific.Linear( V2( V2 ), (^+^), (^-^), (^*), zero )
import Graphics.Rasterific.Outline
import qualified Graphics.Rasterific.Transformations as RT
import Graphics.Text.TrueType
import Graphics.Svg.Types hiding ( Dpi )
import Graphics.Rasterific.Svg.PathConverter
import Graphics.Rasterific.Svg.RenderContext
import Graphics.Rasterific.Svg.RasterificTextRendering

{-import Debug.Trace-}
{-import Text.Groom-}
{-import Text.Printf-}

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

pdfOfSvgDocument :: FontCache -> Maybe (Int, Int) -> Dpi -> Document
                 -> IO (LB.ByteString, LoadedElements)
pdfOfSvgDocument cache sizes dpi doc = do
  (drawing, loaded) <- drawingOfSvgDocument cache sizes dpi doc
  let img = R.renderDrawingAtDpiToPDF (_drawWidth drawing) (_drawHeight drawing) dpi $ _drawAction drawing
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
        , _basePath = _documentLocation doc
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
  rf = realToFrac

  go (TransformMatrix a b c d e f) =
     RT.Transformation (rf a) (rf b) (rf c) (rf d) (rf e) (rf f)
  go (Translate x y) = RT.translate $ V2 (rf x) (rf y)
  go (Scale xs Nothing) = RT.scale (rf xs) (rf xs)
  go (Scale xs (Just ys)) = RT.scale (rf xs) (rf ys)
  go (Rotate angle Nothing) =
      RT.rotate . toRadian $ rf angle
  go (Rotate angle (Just (cx, cy))) =
      RT.rotateCenter (toRadian $ rf angle) $ V2 (rf cx) (rf cy)
  go (SkewX v) = RT.skewX . toRadian $ rf v
  go (SkewY v) = RT.skewY . toRadian $ rf v
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


drawMarker :: (DrawAttributes -> Last ElementRef)
           -> (R.Drawing PixelRGBA8 () -> Bool -> [R.Primitive] -> R.Drawing PixelRGBA8 ())
           -> RenderContext -> DrawAttributes -> [R.Primitive]
           -> IODraw (R.Drawing PixelRGBA8 ())
drawMarker accessor placer ctxt info prims =
  withInfo (getLast . accessor) info $ \markerName ->
    case markerElem markerName of
      Nothing -> return mempty
      Just (ElementMarker mark) -> do
        let subInfo = initialDrawAttributes <> _markerDrawAttributes mark
        markerGeometry <- mapM (renderTree ctxt subInfo)
                        $ _markerElements mark
        let fittedGeometry = baseOrientation mark . fit mark $ mconcat markerGeometry
        return $ placer fittedGeometry (shouldOrient mark) prims
      Just _ -> return mempty
  where
    markerElem RefNone = Nothing
    markerElem (Ref markerName) =
        M.lookup markerName $ _contextDefinitions ctxt

    shouldOrient m = case _markerOrient m of
       Just OrientationAuto -> True
       Nothing -> False
       Just (OrientationAngle _) -> False

    baseOrientation m = case _markerOrient m of
       Nothing -> id
       Just OrientationAuto -> id
       Just (OrientationAngle a) -> 
         R.withTransformation (RT.rotate . toRadian $ realToFrac a)

    units =
      fromMaybe MarkerUnitStrokeWidth . _markerUnits

    toNumber n = case toUserUnit (_renderDpi ctxt) n of
       Num a -> a
       _ -> 1.0

    toStrokeSize n = do
      sw <- toNumber <$> getLast (_strokeWidth info)
      v <- toNumber <$> n
      return . Num $ sw * v

    negatePoint (a, b) =
        (mapNumber negate a, mapNumber negate b)

    fit markerInfo = case units markerInfo of
       MarkerUnitUserSpaceOnUse ->
         fitBox ctxt info
           (Num 0, Num 0)
           (_markerWidth markerInfo)
           (_markerHeight markerInfo)
           (negatePoint $ _markerRefPoint markerInfo)
           (_markerViewBox markerInfo)

       MarkerUnitStrokeWidth ->
         fitBox ctxt info
           (Num 0, Num 0)
           (toStrokeSize $ _markerWidth markerInfo)
           (toStrokeSize $ _markerHeight markerInfo)
           (negatePoint $ _markerRefPoint markerInfo)
           (_markerViewBox markerInfo)

drawEndMarker :: RenderContext -> DrawAttributes -> [R.Primitive]
              -> IODraw (R.Drawing PixelRGBA8 ())
drawEndMarker = drawMarker _markerEnd transformLast where
  transformLast    _ _ [] = return ()
  transformLast geom shouldOrient lst = R.withTransformation trans geom
    where
      prim = last lst
      pp = R.lastPointOf prim
      orient = R.lastTangeantOf prim
      trans | shouldOrient = RT.translate pp <> RT.toNewXBase orient
            | otherwise = RT.translate pp

drawMidMarker :: RenderContext -> DrawAttributes -> [R.Primitive]
               -> IODraw (R.Drawing PixelRGBA8 ())
drawMidMarker = drawMarker _markerMid transformStart where
  transformStart geom shouldOrient = go where
    go [] = return ()
    go [_] = return ()
    go (prim:rest@(p2:_)) = R.withTransformation trans geom >> go rest
      where
        pp = R.lastPointOf prim
        prevOrient = R.lastTangeantOf prim
        nextOrient = R.firstTangeantOf p2
        orient = (prevOrient ^+^ nextOrient) ^* 0.5
        trans | shouldOrient = RT.translate pp <> RT.toNewXBase orient
              | otherwise = RT.translate pp

drawStartMarker :: RenderContext -> DrawAttributes -> [R.Primitive]
                -> IODraw (R.Drawing PixelRGBA8 ())
drawStartMarker = drawMarker _markerStart transformStart where
  transformStart    _ _ [] = return ()
  transformStart geom shouldOrient (prim:_) = R.withTransformation trans geom
    where
      pp = R.firstPointOf prim
      orient = R.firstTangeantOf prim
      trans | shouldOrient = RT.translate pp <> RT.toNewXBase orient
            | otherwise = RT.translate pp

applyGroupOpacity :: DrawAttributes -> R.Drawing PixelRGBA8 () -> R.Drawing PixelRGBA8 ()
applyGroupOpacity attrs sub = case _groupOpacity attrs of
  Nothing -> sub
  Just 1.0 -> sub
  Just opa ->
    R.withGroupOpacity (floor . max 0 . min 255 $ opa * 255) sub

stroker :: Bool -> RenderContext -> DrawAttributes -> [R.Primitive]
        -> IODraw (R.Drawing PixelRGBA8 ())
stroker withMarker ctxt info primitives =
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
            
      in do
        geom <-
          if withMarker then do
            start <- drawStartMarker ctxt info primitives
            mid <- drawMidMarker ctxt info primitives
            end <- drawEndMarker ctxt info primitives
            return $ start <> mid <> end
          else return mempty
        final <- foldM strokerAction mempty primsList
        return (final <> geom)

mergeContext :: RenderContext -> DrawAttributes -> RenderContext
mergeContext ctxt _attr = ctxt

viewBoxOfTree :: Tree -> Maybe (Int, Int, Int, Int)
viewBoxOfTree (SymbolTree (Symbol g)) = _groupViewBox g
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
      ElementMask _ -> None
      ElementClipPath _ -> None
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
  let rootFolder = dropFileName $ _basePath ctxt
      realPath = rootFolder </> _imageHref imgInfo
  eimg <- liftIO $ readImage realPath 
  let srect = RectangleTree $ defaultSvg
        { _rectUpperLeftCorner = _imageCornerUpperLeft imgInfo
        , _rectDrawAttributes =
            _imageDrawAttributes imgInfo & fillColor .~ Last (Just FillNone)
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
      stroking <- stroker False context' info $ R.rectangle p' w' h'
      return . applyGroupOpacity attr . withTransform pAttr $ filling <> stroking

initialDrawAttributes :: DrawAttributes
initialDrawAttributes = mempty
  { _strokeWidth = Last . Just $ Num 1.0
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


renderSvg :: RenderContext -> Tree -> IODraw (R.Drawing PixelRGBA8 ())
renderSvg initialContext = renderTree initialContext initialDrawAttributes


fitBox :: RenderContext -> DrawAttributes
       -> Point -> Maybe Number -> Maybe Number -> Point
       -> Maybe (Int, Int, Int, Int)
       -> R.Drawing px ()
       -> R.Drawing px ()
fitBox ctxt attr basePoint mwidth mheight preTranslate viewbox =
  let origin = linearisePoint ctxt attr basePoint
      preShift = linearisePoint ctxt attr preTranslate
      w = lineariseXLength ctxt attr <$> mwidth
      h = lineariseYLength ctxt attr <$> mheight
  in
  case viewbox of
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
                          <> RT.translate (negate boxOrigin ^+^ preShift)

fitUse :: RenderContext -> DrawAttributes -> Use -> Tree
       -> R.Drawing px ()
       -> R.Drawing px ()
fitUse ctxt attr useElement subTree =
  fitBox ctxt attr
    (_useBase useElement)
    (_useWidth useElement)
    (_useHeight useElement)
    (Num 0, Num 0)
    (viewBoxOfTree subTree)

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

    go ctxt attr (SymbolTree (Symbol g)) = go ctxt attr $ GroupTree g
    go ctxt attr (GroupTree (Group groupAttr subTrees _)) = do
        subTrees' <- mapM (go context' attr') subTrees
        return . applyGroupOpacity groupAttr
               . withTransform groupAttr $ sequence_ subTrees'
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
      stroking <- stroker False context' info rect
      return . applyGroupOpacity pAttr
             . withTransform pAttr $ filling <> stroking

    go ctxt attr (CircleTree (Circle pAttr p r)) = do
      let info = attr <> pAttr
          context' = mergeContext ctxt pAttr
          p' = linearisePoint context' info p
          r' = lineariseLength context' info r
          c = R.circle p' r'
      filling <- filler context' info c
      stroking <- stroker False context' info c
      return . applyGroupOpacity pAttr
             . withTransform pAttr $ filling <> stroking

    go ctxt attr (EllipseTree (Ellipse pAttr p rx ry)) = do
      let info = attr <> pAttr
          context' = mergeContext ctxt pAttr
          p' = linearisePoint context' info p
          rx' = lineariseXLength context' info rx
          ry' = lineariseYLength context' info ry
          c = R.ellipse p' rx' ry'
      filling <- filler context' info c
      stroking <- stroker False context' info c
      return . applyGroupOpacity pAttr
             . withTransform pAttr $ filling <> stroking

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
      stroking <- stroker True context' info $ R.line p1' p2'
      return . applyGroupOpacity pAttr $ withTransform pAttr stroking

    go ctxt attr (PathTree (Path pAttr p)) = do
      let info = attr <> pAttr
          strokePrimitives = svgPathToPrimitives False p
          fillPrimitives = svgPathToPrimitives True p
      filling <- filler ctxt info fillPrimitives
      stroking <- stroker True ctxt info strokePrimitives
      return . applyGroupOpacity pAttr 
             . withTransform pAttr $ filling <> stroking

