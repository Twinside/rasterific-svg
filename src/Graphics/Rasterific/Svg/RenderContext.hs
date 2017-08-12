{-# LANGUAGE CPP #-}
module Graphics.Rasterific.Svg.RenderContext
    ( RenderContext( .. )
    , LoadedElements( .. )
    , loadedFonts
    , loadedImages
    , IODraw
    , ViewBox
    , toRadian
    , capOfSvg
    , joinOfSvg 
    , stripUnits
    , boundingBoxLength
    , boundbingBoxLinearise
    , lineariseXLength
    , lineariseYLength
    , linearisePoint
    , lineariseLength
    , prepareTexture
    , fillAlphaCombine
    , fillMethodOfSvg
    , emTransform
    , toTransformationMatrix
    )
    where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( (<$>) )
import Data.Monoid( Monoid( .. ) )
#endif

import Control.Lens( (&), (.~) )
import Control.Monad.Trans.State.Strict( StateT )
import Codec.Picture( PixelRGBA8( .. ) )
import qualified Codec.Picture as CP
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Monoid( (<>), Last( .. ) )
import Control.Lens( Lens', lens )

import Graphics.Rasterific.Linear( (^-^) )
import qualified Graphics.Rasterific as R
import qualified Graphics.Rasterific.Transformations as RT
import qualified Graphics.Rasterific.Texture as RT
import Graphics.Text.TrueType
import Graphics.Svg.Types
import Graphics.Rasterific.Svg.MeshConverter

import Debug.Trace
import Text.Printf

toRadian :: Floating a => a -> a
toRadian v = v / 180 * pi

type Definitions = M.Map String Element

data RenderContext = RenderContext
    { _initialViewBox     :: !(R.Point, R.Point)
    , _renderViewBox      :: !(R.Point, R.Point)
    , _renderDpi          :: !Int
    , _contextDefinitions :: !Definitions
    , _fontCache          :: !FontCache
    , _subRender          :: !(Document -> IODraw (R.Drawing PixelRGBA8 ()))
    , _basePath           :: !FilePath
    }

data LoadedElements = LoadedElements
    { _loadedFonts  :: M.Map FilePath Font
    , _loadedImages :: M.Map FilePath (CP.Image PixelRGBA8)
    }

instance Monoid LoadedElements where
  mempty = LoadedElements mempty mempty
  mappend (LoadedElements a b) (LoadedElements a' b') =
      LoadedElements (a `mappend` a') (b `mappend` b')

globalBounds :: RenderContext -> R.PlaneBound
globalBounds RenderContext { _renderViewBox = (p1, p2) } =
    R.PlaneBound p1 p2

loadedFonts :: Lens' LoadedElements (M.Map FilePath Font)
loadedFonts = lens _loadedFonts (\a b -> a { _loadedFonts = b })

loadedImages :: Lens' LoadedElements (M.Map FilePath (CP.Image PixelRGBA8))
loadedImages = lens _loadedImages (\a b -> a { _loadedImages = b })

type IODraw = StateT LoadedElements IO

type ViewBox = (R.Point, R.Point)

capOfSvg :: DrawAttributes -> (R.Cap, R.Cap)
capOfSvg attrs =
  case getLast $ _strokeLineCap attrs of
    Nothing -> (R.CapStraight 1, R.CapStraight 1)
    Just CapSquare -> (R.CapStraight 1, R.CapStraight 1)
    Just CapButt -> (R.CapStraight 0, R.CapStraight 0)
    Just CapRound -> (R.CapRound, R.CapRound)


joinOfSvg :: DrawAttributes -> R.Join
joinOfSvg attrs =
  case (getLast $ _strokeLineJoin attrs, getLast $ _strokeMiterLimit attrs) of
    (Nothing, _) -> R.JoinRound
    (Just JoinMiter, Just v) -> R.JoinMiter $ 1 / realToFrac v
    (Just JoinMiter, _) -> R.JoinMiter 0
    (Just JoinBevel, _) -> R.JoinMiter 5
    (Just JoinRound, _) -> R.JoinRound

stripUnits :: RenderContext -> Number -> Number
stripUnits ctxt = toUserUnit (_renderDpi ctxt)

boundingBoxLength :: RenderContext -> DrawAttributes -> R.PlaneBound -> Number
                  -> Float
boundingBoxLength ctxt attr (R.PlaneBound mini maxi) = go where
  R.V2 actualWidth actualHeight =
             abs <$> (maxi ^-^ mini)
  two = 2 :: Int
  coeff = sqrt (actualWidth ^^ two + actualHeight ^^ two)
        / sqrt 2 :: Float
  go num = case num of
    Num n -> realToFrac n
    Em n -> emTransform attr $ realToFrac n
    Percent p -> realToFrac p * coeff
    _ -> go $ stripUnits ctxt num

boundbingBoxLinearise :: RenderContext -> DrawAttributes -> R.PlaneBound -> Point
                      -> R.Point
boundbingBoxLinearise
    ctxt attr (R.PlaneBound mini@(R.V2 xi yi) maxi) (xp, yp) = R.V2 (finalX xp) (finalY yp)
  where
    R.V2 w h = abs <$> (maxi ^-^ mini)
    finalX nu = case nu of
      Num n -> realToFrac n
      Em n -> emTransform attr $ realToFrac n
      Percent p -> realToFrac p * w + xi
      _ -> finalX $ stripUnits ctxt nu

    finalY nu = case nu of
      Num n -> realToFrac n
      Em n -> emTransform attr $ realToFrac n
      Percent p -> realToFrac p * h + yi
      _ -> finalY $ stripUnits ctxt nu

lineariseXLength :: RenderContext -> DrawAttributes -> Number
                 -> Float
lineariseXLength _ _ (Num i) = realToFrac i
lineariseXLength _ attr (Em i) = emTransform attr $ realToFrac i
lineariseXLength ctxt _ (Percent p) = abs (xe - xs) * realToFrac p
  where
    (R.V2 xs _, R.V2 xe _) = _renderViewBox ctxt
lineariseXLength ctxt attr num =
    lineariseXLength ctxt attr $ stripUnits ctxt num

lineariseYLength :: RenderContext -> DrawAttributes -> Number
                 -> Float
lineariseYLength _ _ (Num i) = realToFrac i
lineariseYLength _ attr (Em n) = emTransform attr $ realToFrac n
lineariseYLength ctxt _ (Percent p) = abs (ye - ys) * (realToFrac p)
  where
    (R.V2 _ ys, R.V2 _ ye) = _renderViewBox ctxt
lineariseYLength ctxt attr num =
    lineariseYLength ctxt attr $ stripUnits ctxt num


linearisePoint :: RenderContext -> DrawAttributes -> Point
               -> R.Point
linearisePoint ctxt attr (p1, p2) =
  R.V2 (xs + lineariseXLength ctxt attr p1)
       (ys + lineariseYLength ctxt attr p2)
  where (R.V2 xs ys, _) = _renderViewBox ctxt

emTransform :: DrawAttributes -> Float -> Float
emTransform attr n = case getLast $ _fontSize attr of
    Nothing -> 16 * realToFrac n
    Just (Num v) -> realToFrac v * n
    Just _ -> 16 * n

lineariseLength :: RenderContext -> DrawAttributes -> Number
                -> Float
lineariseLength _ _ (Num i) = realToFrac i
lineariseLength _ attr (Em i) = emTransform attr $ realToFrac i
lineariseLength ctxt _ (Percent v) = realToFrac v * coeff
  where
    (R.V2 x1 y1, R.V2 x2 y2) = _renderViewBox ctxt
    actualWidth = abs $ x2 - x1
    actualHeight = abs $ y2 - y1
    two = 2 :: Int
    coeff = sqrt (actualWidth ^^ two + actualHeight ^^ two)
          / sqrt 2
lineariseLength ctxt attr num =
    lineariseLength ctxt attr $ stripUnits ctxt num

prepareGradientMeshTexture
    :: RenderContext -> DrawAttributes
    -> MeshGradient ->  [R.Primitive]
    -> R.Texture PixelRGBA8
prepareGradientMeshTexture ctxt _attr mesh prims =
  let bounds = F.foldMap R.planeBounds prims
      strip (x, y) = (stripUnits ctxt x, stripUnits ctxt y)
      mesh' = mapMeshBaseCoordiantes strip mesh
      gradTransform = toTransformer $ _meshGradientTransform mesh
      interp = case _meshGradientType mesh of
        GradientBilinear -> R.PatchBilinear
        GradientBicubic -> R.PatchBicubic
  in
  RT.meshPatchTexture interp $
      R.transform gradTransform $ convertGradientMesh (globalBounds ctxt) bounds mesh'

toTransformer :: [Transformation] -> R.Point -> R.Point
toTransformer [] = id
toTransformer lst = RT.applyTransformation combined where
  combined = F.foldMap toTransformationMatrix lst

prepareLinearGradientTexture
    :: RenderContext -> DrawAttributes
    -> LinearGradient -> Float -> [R.Primitive]
    -> R.Texture PixelRGBA8
prepareLinearGradientTexture ctxt attr grad opa prims =
  let bounds = F.foldMap R.planeBounds prims
      lineariser = case _linearGradientUnits grad of
        CoordUserSpace -> linearisePoint ctxt attr
        CoordBoundingBox -> boundbingBoxLinearise ctxt attr bounds
      toA = maybe 1 id
      gradTransform = toTransformer $ _linearGradientTransform grad
      gradient =
        [(offset, fillAlphaCombine (opa * toA opa2) color)
            | GradientStop offset color _ opa2 <- _linearGradientStops grad]
      startPoint = lineariser $ _linearGradientStart grad
      stopPoint = lineariser $ _linearGradientStop grad
  in
  RT.linearGradientTexture gradient (gradTransform startPoint) (gradTransform stopPoint)

prepareRadialGradientTexture
    :: RenderContext -> DrawAttributes
    -> RadialGradient -> Float -> [R.Primitive]
    -> R.Texture PixelRGBA8
prepareRadialGradientTexture ctxt attr grad opa prims =
  let bounds = F.foldMap R.planeBounds prims
      (lineariser, lengthLinearise) = case _radialGradientUnits grad of
        CoordUserSpace ->
          (linearisePoint ctxt attr, lineariseLength ctxt attr)
        CoordBoundingBox ->
          (boundbingBoxLinearise ctxt attr bounds,
           boundingBoxLength ctxt attr bounds)
      toA = maybe 1 id
      gradTransform = toTransformer $ _radialGradientTransform grad
      gradient =
        [(offset, fillAlphaCombine (opa * toA opa2) color)
            | GradientStop offset color _ opa2 <- _radialGradientStops grad]
      center = gradTransform . lineariser $ _radialGradientCenter grad
      radius = lengthLinearise $ _radialGradientRadius grad
  in
  case (_radialGradientFocusX grad,
            _radialGradientFocusY grad) of
    (Nothing, Nothing) ->
      RT.radialGradientTexture gradient center radius
    (Just fx, Nothing) ->
      RT.radialGradientWithFocusTexture gradient center radius
        $ lineariser (fx, snd $ _radialGradientCenter grad)
    (Nothing, Just fy) ->
      RT.radialGradientWithFocusTexture gradient center radius
        $ lineariser (fst $ _radialGradientCenter grad, fy)
    (Just fx, Just fy) ->
      RT.radialGradientWithFocusTexture gradient center radius
        $ lineariser (fx, fy)

fillMethodOfSvg :: DrawAttributes -> R.FillMethod
fillMethodOfSvg attr = case getLast $ _fillRule attr of
    Nothing -> R.FillWinding
    Just FillNonZero -> R.FillWinding
    Just FillEvenOdd -> R.FillEvenOdd

fillAlphaCombine :: Float -> PixelRGBA8 -> PixelRGBA8
fillAlphaCombine opacity (PixelRGBA8 r g b a) =
    PixelRGBA8 r g b alpha
  where
    a' = fromIntegral a / 255.0
    alpha = floor . max 0 . min 255 $ opacity * a' * 255

scalesOfTransformation :: RT.Transformation -> (Float, Float)
scalesOfTransformation (RT.Transformation a c _e
                                          b d _f) = (widthScale, heightScale)
  where
    widthScale = sqrt $ a * a + c * c
    heightScale = sqrt $ b * b + d * d


documentOfPattern :: Definitions -> RT.Transformation -> Int -> Int -> Pattern -> String
                  -> Document
documentOfPattern defs trans w h pat loc = Document
    { _viewBox     = _patternViewBox pat
    , _width       = return . Num $ fromIntegral tileWidth
    , _height      = return . Num $ fromIntegral tileHeight
    , _elements    = _patternElements pat -- [GroupTree asTransformedGroup]
    , _definitions = defs
    , _styleRules  = []
    , _description = ""
    , _documentLocation = loc
    }
  where
    (widthScale, heightScale) = scalesOfTransformation trans
    tileWidth, tileHeight :: Int
    tileWidth = floor $ widthScale * fromIntegral w
    tileHeight = floor $ heightScale * fromIntegral h
    _asGroup = defaultSvg { _groupChildren = _patternElements pat }
    _transfo = Scale 
                (realToFrac widthScale)
                (Just . realToFrac $ heightScale)
    _asTransformedGroup = _asGroup & drawAttr . transform .~ Just [_transfo]


toTransformationMatrix :: Transformation -> RT.Transformation
toTransformationMatrix = go where
  rf = realToFrac
  go (TransformMatrix a d b e c f) =
     RT.Transformation (rf a) (rf b) (rf c) (rf d) (rf e) (rf f)
  go (Translate x y) = RT.translate $ R.V2 (rf x) (rf y)
  go (Scale xs Nothing) = RT.scale (rf xs) (rf xs)
  go (Scale xs (Just ys)) = RT.scale (rf xs) (rf ys)
  go (Rotate angle Nothing) =
      RT.rotate . toRadian $ rf angle
  go (Rotate angle (Just (cx, cy))) =
      RT.rotateCenter (toRadian $ rf angle) $ R.V2 (rf cx) (rf cy)
  go (SkewX v) = RT.skewX . toRadian $ rf v
  go (SkewY v) = RT.skewY . toRadian $ rf v
  go TransformUnknown = mempty


prepareTexture :: RenderContext -> DrawAttributes
               -> Texture -> Float
               -> [R.Primitive]
               -> IODraw (Maybe (R.Texture PixelRGBA8))
prepareTexture _ _ FillNone _opacity _ = return Nothing
prepareTexture _ _ (ColorRef color) opacity _ =
  return . Just . RT.uniformTexture $ fillAlphaCombine opacity color
prepareTexture ctxt attr (TextureRef ref) opacity prims =
    maybe (return Nothing) (prepare mempty) $ M.lookup ref (_contextDefinitions ctxt) where
  prepare rootTrans e = case e of
    ElementGeometry _ -> return Nothing
    ElementMarker _ -> return Nothing
    ElementMask _ -> return Nothing
    ElementClipPath _ -> return Nothing
    ElementMeshGradient mesh ->
      return . Just $ prepareGradientMeshTexture ctxt attr mesh prims
    ElementLinearGradient grad ->
      return . Just $ prepareLinearGradientTexture ctxt attr grad opacity prims
    ElementRadialGradient grad ->
      return . Just $ prepareRadialGradientTexture ctxt attr grad opacity prims
    ElementPattern pat@Pattern { _patternHref = "" } -> do
      let doc = documentOfPattern (_contextDefinitions ctxt) rootTrans w h pat (_basePath ctxt)
          dpi = _renderDpi ctxt
          w = floor . lineariseXLength ctxt attr $ _patternWidth pat
          h = floor . lineariseYLength ctxt attr $ _patternHeight pat
      patDrawing <- _subRender ctxt doc
      return . Just $ RT.patternTexture w h dpi (PixelRGBA8 0 0 0 0) patDrawing
    ElementPattern pat -> do
      let _inverser = maybe id RT.transformTexture . RT.inverseTransformation
          _applyTransformation = RT.transformTexture
          trans = maybe mempty (F.foldMap toTransformationMatrix) $ _patternTransform pat
          nextRef = _patternHref pat
      maybe (return Nothing) (prepare (rootTrans <> trans)) $ M.lookup nextRef (_contextDefinitions ctxt)

