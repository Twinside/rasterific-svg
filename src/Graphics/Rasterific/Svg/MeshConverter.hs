{-# LANGUAGE CPP #-}
module Graphics.Rasterific.Svg.MeshConverter where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid( mconcat )
import Control.Applicative( pure, (<$>) )
#endif

import Codec.Picture( PixelRGBA8 )

import Graphics.Svg.Types
import Graphics.Rasterific.MeshPatch

convert :: MeshGradient -> MeshPatch PixelRGBA8
convert = undefined

svgMeshSize :: MeshGradient -> (Int, Int)
svgMeshSize mesh = (width, height) where
  height = length $ _meshGradientRows mesh
  width = maximum $ length <$> _meshGradientRows mesh


data PathConverter 

svgPathToPrimitives :: Bool -> [PathCommand] -> [R.Primitive]
svgPathToPrimitives shouldClose lst
    | shouldClose && not (nearZero $ norm (lastPoint ^-^ firstPoint)) =
        concat $ prims ++ [R.line lastPoint firstPoint]
    | otherwise = concat prims
  where
    ((lastPoint, _, firstPoint), prims) =
        mapAccumL go (zero, zero, zero) $ singularize lst

    go (latest, p, first) GClose =
        ((first, p, first), R.line latest first)

    go (_, _, _) (MoveTo OriginAbsolute (p:_)) = ((p', p', p'), [])
      where p' = toR p
    go (o, _, _) (MoveTo OriginRelative (p:_)) =
        ((pp, pp, pp), []) where pp = o ^+^ toR p

    go (o, _, fp) (LineTo OriginRelative (c:_)) =
        ((p, p, fp), R.line o p) where p = o ^+^ toR c

    go (o, _, fp) (LineTo OriginAbsolute (p:_)) =
        ((p', p', fp), R.line o $ toR p)
          where p' = toR p

    go (o, _, fp) (CurveTo OriginAbsolute ((c1, c2, e):_)) =
        ((e', c2', fp),
            [R.CubicBezierPrim $ R.CubicBezier o (toR c1) c2' e'])
       where e' = toR e
             c2' = toR c2

    go (o, _, fp) (CurveTo OriginRelative ((c1, c2, e):_)) =
        ((e', c2', fp), [R.CubicBezierPrim $ R.CubicBezier o c1' c2' e'])
      where c1' = o ^+^ toR c1
            c2' = o ^+^ toR c2
            e' = o ^+^ toR e

