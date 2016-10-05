{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
module Graphics.Rasterific.Svg.MeshConverter where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid( mconcat )
import Control.Applicative( pure, (<$>) )
#endif

import Control.Monad.Primitive( PrimMonad, PrimState )
import Control.Monad.Reader.Class( MonadReader )
import Graphics.Rasterific.Linear( (^+^)
                                 , (^-^)
                                 , lerp
                                 )
import qualified Linear as L
import qualified Graphics.Rasterific as R
import Data.Vector( (//) )
import qualified Data.Vector as V

import Codec.Picture( PixelRGBA8( .. ) )

import Graphics.Svg.Types
import Graphics.Rasterific.MeshPatch
{-import Graphics.Rasterific.Svg.RenderContext-}

toBaseX :: R.PlaneBound -> MeshGradient -> Float
toBaseX bounds mesh = case _meshGradientX mesh of
    Num n -> realToFrac n
    Percent p -> miniX + (maxiX - miniX) * realToFrac p
    Px n  -> realToFrac n
    Em n -> realToFrac n
    Pc n -> realToFrac n
    Mm n -> realToFrac n
    Cm n -> realToFrac n
    Point n -> realToFrac n
    Inches n -> realToFrac n
  where
    R.PlaneBound (R.V2 miniX _miniY) (R.V2 maxiX _maxiY) = bounds

toBaseY :: R.PlaneBound -> MeshGradient -> Float
toBaseY bounds mesh = case _meshGradientY mesh of
    Num n -> realToFrac n
    Percent p -> miniY + (maxiY - miniY) * realToFrac p
    Px n  -> realToFrac n
    Em n -> realToFrac n
    Pc n -> realToFrac n
    Mm n -> realToFrac n
    Cm n -> realToFrac n
    Point n -> realToFrac n
    Inches n -> realToFrac n
  where
    R.PlaneBound (R.V2 _miniX miniY) (R.V2 _maxiX maxiY) = bounds


mapMeshBaseCoordiantes :: ((Number, Number) -> (Number, Number)) -> MeshGradient
                       -> MeshGradient
mapMeshBaseCoordiantes f m = m { _meshGradientX = x, _meshGradientY = y }
  where (x, y) = f (_meshGradientX m, _meshGradientY m)

convertGradientMesh :: R.PlaneBound -> R.PlaneBound -> MeshGradient -> MeshPatch PixelRGBA8
convertGradientMesh globalBounds bounds mesh = scaler rmesh where
  (_, rmesh) = withMesh baseGrid (gatherGeometry svgBasePoint mesh)
  (w, h) = svgMeshSize mesh
  colors = gatherColors mesh w h
  baseGrid = generateLinearGrid w h svgBasePoint svgBasePoint colors

  svgBasePoint =
      R.V2 (toBaseX startBounds mesh) (toBaseY startBounds mesh)

  startBounds = case _meshGradientUnits mesh of
    CoordUserSpace -> globalBounds
    CoordBoundingBox -> R.PlaneBound (R.V2 0 0) (R.V2 1 1)
 
  delta = R._planeMaxBound bounds ^-^ R._planeMinBound bounds
  toBoundingBox p = R._planeMinBound bounds ^+^ delta * p

  scaler :: MeshPatch px -> MeshPatch px
  scaler = case _meshGradientUnits mesh of
    CoordUserSpace -> id
    CoordBoundingBox -> R.transform toBoundingBox


gatherGeometry :: (MonadReader (MutableMesh (PrimState m) px) m, PrimMonad m)
               => R.Point -> MeshGradient -> m ()
gatherGeometry basePoint = mapM_ goRow . zip [0 ..] . _meshGradientRows where
  toCurve firstPatchPoint lastPoint p = case _gradientPath p of
    Just pp -> svgPathToPrimitives firstPatchPoint lastPoint pp
    Nothing -> lastPoint `straightLine` firstPatchPoint

  lastOf (R.CubicBezier _ _ _ a) = a
  firstOf (R.CubicBezier a _ _ _) = a
  goRow (y, r) = mapM_ (goPatch y) . zip [0 ..] $ _meshGradientRowPatches r
  goPatch y (x, patch) = case _meshGradientPatchStops patch of
      -- A     B
      --  +---+
      --  |   |
      --  +---+
      -- D     C
      [a, b, c, d] -> do
        let toC = toCurve basePoint
            northEast = toC basePoint a
            eastSouth = toC (lastOf northEast) b
            southWest = toC (lastOf eastSouth) c
            westNorth = toC (lastOf southWest) d
        setVertice  x       y      $ firstOf northEast
        setVertice (x + 1)  y      $ lastOf northEast
        setVertice (x + 1) (y + 1) $ firstOf southWest
        setVertice  x      (y + 1) $ lastOf southWest
        horizOrdered northEast
        horizUnordered southWest
        vertUnordered westNorth
        vertOrdered eastSouth

      -- A     B
      --  +---+
      --      |
      --  +---+
      --       C
      [a, b, c] | y == 0 -> do
        firstPoint <- getVertice x y
        closePoint <- getVertice x (y + 1)
        let toC = toCurve closePoint
            northEast = toC firstPoint a
            eastSouth = toC (lastOf northEast) b
            southWest = toC (lastOf eastSouth) c
        setVertice (x + 1)  y      $ firstOf eastSouth
        setVertice (x + 1) (y + 1) $ lastOf eastSouth
        horizOrdered northEast
        horizUnordered southWest
        vertOrdered eastSouth


      --       B
      --  +   +
      --  |   |
      --  +---+
      -- D     C
      [b, c, d] -> do
        firstPoint <- getVertice (x + 1) y
        closePoint <- getVertice x y
        let toC = toCurve closePoint
            eastSouth = toC firstPoint b
            southWest = toC (lastOf eastSouth) c
            westNorth = toC (lastOf southWest) d
        setVertice (x + 1) (y + 1) $ firstOf southWest
        setVertice  x      (y + 1) $ lastOf southWest
        horizUnordered southWest
        vertUnordered westNorth
        vertOrdered eastSouth

      --       B
      --      +
      --      |
      --  +---+
      --       C
      [b, c] -> do
        firstPoint <- getVertice (x + 1) y
        closePoint <- getVertice x (y + 1)
        let toC = toCurve closePoint
            eastSouth = toC firstPoint b
            southWest = toC (lastOf eastSouth) c
        setVertice (x + 1) (y + 1) $ firstOf southWest
        horizUnordered southWest
        vertOrdered eastSouth
      _ -> return ()
    where
      horizOrdered (R.CubicBezier _ b c _) = setHorizPoints x y $ InterBezier b c
      horizUnordered (R.CubicBezier _ b c _) = setHorizPoints x (y + 1) $ InterBezier c b
      vertUnordered (R.CubicBezier _ b c _) = setVertPoints x y $ InterBezier c b
      vertOrdered (R.CubicBezier _ b c _) = setVertPoints (x + 1) y $ InterBezier b c


gatherColors :: MeshGradient -> Int -> Int -> V.Vector PixelRGBA8
gatherColors mesh w h = baseVec // foldMap goRow (zip [0 ..] $ _meshGradientRows mesh) where
  baseVec = V.replicate ((w + 1) * (h + 1)) $ PixelRGBA8 0 0 0 255

  goRow (y, row) = foldMap (goPatch y) . zip [0 ..] $ _meshGradientRowPatches row

  goPatch y (x, patch) = case _meshGradientPatchStops patch of
      -- A     B
      --  +---+
      --  |   |
      --  +---+
      -- D     C
      [a, b, c, d] ->
        [setAt 0 0 a, setAt 1 0 b, setAt 1 1 c, setAt 0 1 d]
      -- A     B
      --  +---+
      --      |
      --  +---+
      --       C
      [_a, b, c] | y == 0 -> [setAt 1 0 b, setAt 1 1 c]
      --       B
      --  +   +
      --  |   |
      --  +---+
      -- D     C
      [_b, c, d] -> [setAt 1 1 c, setAt 0 1 d]
      --       B
      --      +
      --      |
      --  +---+
      --       C
      [_b, c] -> [setAt 1 1 c]

      _ -> []
    where
      colorOf s = case _gradientOpacity s of
          Nothing -> _gradientColor s
          Just a -> PixelRGBA8 r g b . floor $ 255 * a
        where
          PixelRGBA8 r g b _ = _gradientColor s


      setAt dx dy stop = (idx, colorOf stop) where
        idx = (y + dy) * (w + 1) + x + dx


svgMeshSize :: MeshGradient -> (Int, Int)
svgMeshSize mesh = (w, h) where
  h = length $ _meshGradientRows mesh
  w = maximum $ length . _meshGradientRowPatches <$> _meshGradientRows mesh

svgPathToPrimitives :: R.Point -> R.Point -> GradientPathCommand -> R.CubicBezier
svgPathToPrimitives firstPatchPoint = go where
  go o GClose = o `straightLine` firstPatchPoint
  go o (GLine OriginRelative c) = o `straightLine` (o ^+^ mp c)
  go o (GLine OriginAbsolute p) = o `straightLine` mp p
  go o (GCurve OriginAbsolute c1 c2 e) =
    R.CubicBezier o (toR c1) (toR c2) (mp e)
  go o (GCurve OriginRelative c1 c2 e) =
    R.CubicBezier o (o ^+^ toR c1) (o ^+^ toR c2) (o ^+^ mp e)

  mp Nothing = firstPatchPoint
  mp (Just p) = toR p

toR :: RPoint -> R.Point
{-# INLINE toR #-}
toR (L.V2 x y) = realToFrac <$> R.V2 x y

straightLine :: R.Point -> R.Point -> R.CubicBezier
straightLine a b = R.CubicBezier a p1 p2 b where
  p1 = lerp (1/3) a b
  p2 = lerp (2/3) a b
