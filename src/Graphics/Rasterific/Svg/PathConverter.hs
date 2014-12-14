module Graphics.Rasterific.Svg.PathConverter
        ( svgPathToPrimitives
        , svgPathToRasterificPath
        ) where

import Control.Applicative( (<$>), pure )
import Data.List( mapAccumL )
import Graphics.Rasterific.Linear( (^+^)
                                 , (^-^)
                                 , (^*)
                                 , norm
                                 , nearZero
                                 , zero )
import qualified Graphics.Rasterific as R
import qualified Linear as L
import Graphics.Svg.Types

singularize :: [Path] -> [Path]
singularize = concatMap go
  where
   go (MoveTo _ []) = []
   go (MoveTo o (x: xs)) = MoveTo o [x] : go (LineTo o xs)
   go (LineTo o lst) = LineTo o . pure <$> lst
   go (HorizontalTo o lst) = HorizontalTo o . pure <$> lst
   go (VerticalTo o lst) = VerticalTo o . pure <$> lst
   go (CurveTo o lst) = CurveTo o . pure <$> lst
   go (SmoothCurveTo o lst) = SmoothCurveTo o . pure <$> lst
   go (QuadraticBezier o lst) = QuadraticBezier o . pure <$> lst
   go (SmoothQuadraticBezierCurveTo o lst) =
       SmoothQuadraticBezierCurveTo o . pure <$> lst
   go (ElipticalArc o lst) = ElipticalArc o . pure <$> lst
   go EndPath = [EndPath]

toR :: RPoint -> R.Point
{-# INLINE toR #-}
toR (L.V2 x y) = R.V2 x y

svgPathToPrimitives :: Bool -> [Path] -> [R.Primitive]
svgPathToPrimitives _ lst | isPathWithArc lst = []
svgPathToPrimitives shouldClose lst
    | shouldClose && not (nearZero $ norm (lastPoint ^-^ firstPoint)) =
        concat $ prims ++ [R.line lastPoint firstPoint]
    | otherwise = concat prims
  where
    ((lastPoint, _, firstPoint), prims) =
        mapAccumL go (zero, zero, zero) $ singularize lst

    go (latest, p, first) EndPath =
        ((first, p, first), R.line latest first)

    go o (HorizontalTo _ []) = (o, [])
    go o (VerticalTo _ []) = (o, [])
    go o (MoveTo _ []) = (o, [])
    go o (LineTo _ []) = (o, [])
    go o (CurveTo _ []) = (o, [])
    go o (SmoothCurveTo _ []) = (o, [])
    go o (QuadraticBezier _ []) = (o, [])
    go o (SmoothQuadraticBezierCurveTo  _ []) = (o, [])

    go (_, _, _) (MoveTo OriginAbsolute (p:_)) = ((p', p', p'), [])
      where p' = toR p
    go (o, _, _) (MoveTo OriginRelative (p:_)) =
        ((pp, pp, pp), []) where pp = o ^+^ toR p

    go (o@(R.V2 _ y), _, fp) (HorizontalTo OriginAbsolute (c:_)) =
        ((p, p, fp), R.line o p) where p = R.V2 c y
    go (o@(R.V2 x y), _, fp) (HorizontalTo OriginRelative (c:_)) =
        ((p, p, fp), R.line o p) where p = R.V2 (x + c) y

    go (o@(R.V2 x _), _, fp) (VerticalTo OriginAbsolute (c:_)) =
        ((p, p, fp), R.line o p) where p = R.V2 x c
    go (o@(R.V2 x y), _, fp) (VerticalTo OriginRelative (c:_)) =
        ((p, p, fp), R.line o p) where p = R.V2 x (c + y)

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

    go (o, control, fp) (SmoothCurveTo OriginAbsolute ((c2, e):_)) =
        ((e', c2', fp), [R.CubicBezierPrim $ R.CubicBezier o c1' c2' e'])
      where c1' = o ^* 2 ^-^ control
            c2' = toR c2
            e' = toR e

    go (o, control, fp) (SmoothCurveTo OriginRelative ((c2, e):_)) =
        ((e', c2', fp), [R.CubicBezierPrim $ R.CubicBezier o c1' c2' e'])
      where c1' = o ^* 2 ^-^ control
            c2' = o ^+^ toR c2
            e' = o ^+^ toR e

    go (o, _, fp) (QuadraticBezier OriginAbsolute ((c1, e):_)) =
        ((e', c1', fp), [R.BezierPrim $ R.Bezier o c1' e'])
      where e' = toR e
            c1' = toR c1

    go (o, _, fp) (QuadraticBezier OriginRelative ((c1, e):_)) =
        ((e', c1', fp), [R.BezierPrim $ R.Bezier o c1' e'])
      where c1' = o ^+^ toR c1
            e' = o ^+^ toR e

    go (o, control, fp)
       (SmoothQuadraticBezierCurveTo OriginAbsolute (e:_)) =
       ((e', c1', fp), [R.BezierPrim $ R.Bezier o c1' e'])
      where c1' = o ^* 2 ^-^ control
            e' = toR e

    go (o, control, fp)
       (SmoothQuadraticBezierCurveTo OriginRelative (e:_)) =
       ((e', c1', fp), [R.BezierPrim $ R.Bezier o c1' e'])
      where c1' = o ^* 2 ^-^ control
            e' = o ^+^ toR e

    go _ (ElipticalArc _ _) = error "Unimplemented"


-- | Conversion function between svg path to the rasterific one.
svgPathToRasterificPath :: Bool -> [Path] -> R.Path
svgPathToRasterificPath shouldClose lst =
    R.Path firstPoint shouldClose $ concat commands
 where
  lineTo p = [R.PathLineTo p]
  cubicTo e1 e2 e3 = [R.PathCubicBezierCurveTo e1 e2 e3]
  quadTo e1 e2 = [R.PathQuadraticBezierCurveTo e1 e2]

  ((_, _, firstPoint), commands) =
     mapAccumL go (zero, zero, zero) $ singularize lst
    
  go (_, p, first) EndPath =
      ((first, p, first), [])

  go o (HorizontalTo _ []) = (o, [])
  go o (VerticalTo _ []) = (o, [])
  go o (MoveTo _ []) = (o, [])
  go o (LineTo _ []) = (o, [])
  go o (CurveTo _ []) = (o, [])
  go o (SmoothCurveTo _ []) = (o, [])
  go o (QuadraticBezier _ []) = (o, [])
  go o (SmoothQuadraticBezierCurveTo  _ []) = (o, [])

  go (_, _, _) (MoveTo OriginAbsolute (p:_)) =
      ((pp, pp, pp), []) where pp = toR p
  go (o, _, _) (MoveTo OriginRelative (p:_)) =
      ((pp, pp, pp), []) where pp = o ^+^ toR p

  go (R.V2 _ y, _, fp) (HorizontalTo OriginAbsolute (c:_)) =
      ((p, p, fp), lineTo p) where p = R.V2 c y
  go (R.V2 x y, _, fp) (HorizontalTo OriginRelative (c:_)) =
      ((p, p, fp), lineTo p) where p = R.V2 (x + c) y

  go (R.V2 x _, _, fp) (VerticalTo OriginAbsolute (c:_)) =
      ((p, p, fp), lineTo p) where p = R.V2 x c
  go (R.V2 x y, _, fp) (VerticalTo OriginRelative (c:_)) =
      ((p, p, fp), lineTo p) where p = R.V2 x (c + y)

  go (o, _, fp) (LineTo OriginRelative (c:_)) =
      ((p, p, fp), lineTo p) where p = o ^+^ toR c

  go (_, _, fp) (LineTo OriginAbsolute (p:_)) =
      ((p', p', fp), lineTo p')
     where p' = toR p

  go (_, _, fp) (CurveTo OriginAbsolute ((c1, c2, e):_)) =
      ((e', c2', fp), cubicTo c1' c2' e')
      where e' = toR e
            c2' = toR c2
            c1' = toR c1

  go (o, _, fp) (CurveTo OriginRelative ((c1, c2, e):_)) =
      ((e', c2', fp), cubicTo c1' c2' e')
    where c1' = o ^+^ toR c1
          c2' = o ^+^ toR c2
          e' = o ^+^ toR e

  go (o, control, fp) (SmoothCurveTo OriginAbsolute ((c2, e):_)) =
      ((e', c2', fp), cubicTo c1' c2' e')
    where c1' = o ^* 2 ^-^ control
          c2' = toR c2
          e' = toR e

  go (o, control, fp) (SmoothCurveTo OriginRelative ((c2, e):_)) =
      ((e', c2', fp), cubicTo c1' c2' e')
    where c1' = o ^* 2 ^-^ control
          c2' = o ^+^ toR c2
          e' = o ^+^ toR e

  go (_, _, fp) (QuadraticBezier OriginAbsolute ((c1, e):_)) =
      ((e', c1', fp), quadTo c1' e')
      where e' = toR e
            c1' = toR c1

  go (o, _, fp) (QuadraticBezier OriginRelative ((c1, e):_)) =
      ((e', c1', fp), quadTo c1' e')
    where c1' = o ^+^ toR c1
          e' = o ^+^ toR e

  go (o, control, fp)
     (SmoothQuadraticBezierCurveTo OriginAbsolute (e:_)) =
     ((e', c1', fp), quadTo c1' e')
    where c1' = o ^* 2 ^-^ control
          e' = toR e

  go (o, control, fp)
     (SmoothQuadraticBezierCurveTo OriginRelative (e:_)) =
     ((e', c1', fp), quadTo c1' e')
    where c1' = o ^* 2 ^-^ control
          e' = o ^+^ toR e

  go _ (ElipticalArc _ _) = error "Unimplemented"

