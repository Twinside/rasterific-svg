{-# LANGUAGE CPP #-}
module Graphics.Rasterific.Svg.MeshConverter where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid( mconcat )
import Control.Applicative( pure, (<$>) )
#endif

import Graphics.Rasterific.Linear( (^+^)
                                 , (^-^)
                                 , (^*)
                                 , lerp
                                 )
import qualified Linear as L
import qualified Graphics.Rasterific as R
import Codec.Picture( PixelRGBA8 )

import Graphics.Svg.Types
import Graphics.Rasterific.MeshPatch

convert :: MeshGradient -> MeshPatch PixelRGBA8
convert = undefined

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

