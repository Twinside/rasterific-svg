{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
module Graphics.Rasterific.Svg.MeshConverter where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid( mconcat )
import Control.Applicative( pure, (<$>) )
#endif

import Control.Lens( (^.) )
import Control.Monad.Primitive( PrimMonad, PrimState )
import Control.Monad.Reader.Class( MonadReader )
import Graphics.Rasterific.Linear( (^+^)
                                 , (^-^)
                                 , (^*)
                                 , lerp
                                 )
import qualified Linear as L
import qualified Graphics.Rasterific as R
import Data.Vector( (//) )
import qualified Data.Vector as V

import Codec.Picture( PixelRGBA8( .. ) )

import Graphics.Svg.Types
import Graphics.Rasterific.MeshPatch
import Graphics.Rasterific.Svg.RenderContext

convert :: RenderContext -> R.PlaneBound -> MeshGradient -> MeshPatch PixelRGBA8
convert ctxt bounds mesh = snd $ withMesh baseGrid (gatherGeometry base mesh) where
  (w, h) = svgMeshSize mesh
  colors = gatherColors mesh w h
  baseGrid = generateLinearGrid w h base base colors
  attr = mesh ^. drawAttr 
  svgBasePoint = (_meshGradientX mesh, _meshGradientY mesh)

  base = case _meshGradientUnits mesh of
    CoordUserSpace ->  linearisePoint ctxt attr svgBasePoint
    CoordBoundingBox ->
      boundbingBoxLinearise ctxt attr bounds svgBasePoint

gatherGeometry :: (MonadReader (MutableMesh (PrimState m) px) m, PrimMonad m)
               => R.Point -> MeshGradient -> m ()
gatherGeometry basePoint = mapM_ goRow . zip [0 ..] . _meshGradientRows where
  goRow (y, r) = mapM_ (goPatch y) . zip [0 ..] $ _meshGradientRowPatches r
  goPatch y (x, patch) = case _meshGradientPatchStops patch of
      -- A     B
      --  +---+
      --  |   |
      --  +---+
      -- D     C
      [a, b, c, d] -> return ()
        -- [setAt 0 0 a, setAt 1 0 b, setAt 1 1 c, setAt 0 1 c]
      -- A     B
      --  +---+
      --      |
      --  +---+
      --       C
      [_a, b, c] | y == 0 -> return ()
          -- [setAt 1 0 b, setAt 1 1 c]
      --       B
      --  +   +
      --  |   |
      --  +---+
      -- D     C
      [_b, c, d] -> return () -- [setAt 1 1 c, setAt 1 0 d]
      --       B
      --      +
      --      |
      --  +---+
      --       C
      [_b, c] -> return () -- [setAt 1 1 c]


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
        [setAt 0 0 a, setAt 1 0 b, setAt 1 1 c, setAt 0 1 c]
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
      [_b, c, d] -> [setAt 1 1 c, setAt 1 0 d]
      --       B
      --      +
      --      |
      --  +---+
      --       C
      [_b, c] -> [setAt 1 1 c]
    where
      setAt dx dy stop = (idx, _gradientColor stop) where
        idx = (y + dy) * (w + 1) + x + dx


svgMeshSize :: MeshGradient -> (Int, Int)
svgMeshSize mesh = (width, height) where
  height = length $ _meshGradientRows mesh
  width = maximum $ length . _meshGradientRowPatches <$> _meshGradientRows mesh

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
