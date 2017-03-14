{-# LANGUAGE CPP #-}
module Graphics.Rasterific.Svg.PathConverter
        ( svgPathToPrimitives
        , svgPathToRasterificPath
        ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid( mconcat )
import Control.Applicative( pure, (<$>) )
#endif

import Data.List( mapAccumL )
import Data.Fixed( mod' )
import Graphics.Rasterific.Linear( (^+^)
                                 , (^-^)
                                 , (^*)
                                 , norm
                                 , nearZero
                                 , zero )
import qualified Graphics.Rasterific as R
import Linear( dot, (!*!), (!*), V2( V2 ), scaled )
import qualified Linear as L
import Graphics.Svg.Types
import Graphics.Rasterific.Svg.RenderContext
import Graphics.Rasterific.Svg.ArcConversion

import Debug.Trace
import Text.Printf

singularize :: [PathCommand] -> [PathCommand]
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
   go (EllipticalArc o lst) = EllipticalArc o . pure <$> lst
   go EndPath = [EndPath]

toR :: RPoint -> R.Point
{-# INLINE toR #-}
toR (L.V2 x y) = realToFrac <$> R.V2 x y

fromR :: R.Point -> RPoint
{-# INLINE fromR #-}
fromR (R.V2 x y) = realToFrac <$> L.V2 x y

svgPathToPrimitives :: Bool -> [PathCommand] -> [R.Primitive]
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
    go o (EllipticalArc  _ []) = (o, [])

    go (_, _, _) (MoveTo OriginAbsolute (p:_)) = ((p', p', p'), [])
      where p' = toR p
    go (o, _, _) (MoveTo OriginRelative (p:_)) =
        ((pp, pp, pp), []) where pp = o ^+^ toR p

    go (o@(R.V2 _ y), _, fp) (HorizontalTo OriginAbsolute (c:_)) =
        ((p, p, fp), R.line o p) where p = R.V2 (realToFrac c) y
    go (o@(R.V2 x y), _, fp) (HorizontalTo OriginRelative (c:_)) =
        ((p, p, fp), R.line o p) where p = R.V2 (x + realToFrac c) y

    go (o@(R.V2 x _), _, fp) (VerticalTo OriginAbsolute (c:_)) =
        ((p, p, fp), R.line o p) where p = R.V2 x (realToFrac c)
    go (o@(R.V2 x y), _, fp) (VerticalTo OriginRelative (c:_)) =
        ((p, p, fp), R.line o p) where p = R.V2 x (realToFrac c + y)

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

    go acc@(o, _, _) (EllipticalArc OriginAbsolute (e:_)) =
        (accFinal, mconcat outList)
      where
        (accFinal, outList) = mapAccumL go acc $ arcToSegments (fromR o) e

    go back@(o,_,_) (EllipticalArc OriginRelative ((rx, ry, rot, f1, f2, p): _)) =
      go back $ EllipticalArc OriginAbsolute [new]
        where p' = p L.^+^ (fromR o)
              new = (rx, ry, rot, f1, f2, p')


-- | Conversion function between svg path to the rasterific one.
svgPathToRasterificPath :: Bool -> [PathCommand] -> R.Path
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
  go o (EllipticalArc  _ []) = (o, [])

  go (_, _, _) (MoveTo OriginAbsolute (p:_)) =
      ((pp, pp, pp), []) where pp = toR p
  go (o, _, _) (MoveTo OriginRelative (p:_)) =
      ((pp, pp, pp), []) where pp = o ^+^ toR p

  go (R.V2 _ y, _, fp) (HorizontalTo OriginAbsolute (c:_)) =
      ((p, p, fp), lineTo p) where p = R.V2 (realToFrac c) y
  go (R.V2 x y, _, fp) (HorizontalTo OriginRelative (c:_)) =
      ((p, p, fp), lineTo p) where p = R.V2 (x + realToFrac c) y

  go (R.V2 x _, _, fp) (VerticalTo OriginAbsolute (c:_)) =
      ((p, p, fp), lineTo p) where p = R.V2 x (realToFrac c)
  go (R.V2 x y, _, fp) (VerticalTo OriginRelative (c:_)) =
      ((p, p, fp), lineTo p) where p = R.V2 x (realToFrac c + y)

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

  go back@(o, _, _) (EllipticalArc OriginAbsolute (com:_)) = (nextState, mconcat pathCommands)
    where
      (nextState, pathCommands) =
          mapAccumL go back $ arcToSegments (fromR o) com
  go back@(o, _, _) (EllipticalArc OriginRelative ((rx, ry, rot, f1, f2, p):_)) =
      go back $ EllipticalArc OriginAbsolute [new]
        where p' = p L.^+^ (fromR o)
              new = (rx, ry, rot, f1, f2, p')


{-
-- | SVG arc representation uses "endpoint parameterisation" where we specify the endpoint of the arc.
-- This is to be consistent with the other path commands.  However we need to convert this to "centre point
--  parameterisation" in order to calculate the arc. Handily, the SVG spec provides all the required maths
-- in section "F.6 Elliptical arc implementation notes".
--
-- Some of this code has been borrowed from the Batik library (Apache-2 license).
arcToSegments' :: RPoint -> (Coord, Coord, Coord, Bool, Bool, RPoint)
              -> [PathCommand]
arcToSegments' orig (radX, radY, _rotateX, _large, _sweep, pos)
  | orig == pos = mempty -- equivalent to omitting the elliptical arc segment entirely. (by spec)
  | nearZero radX || nearZero radY = [LineTo OriginAbsolute [pos]]
arcToSegments' orig (radX, radY, rotateX, large, sweep, pos) = bezierCommands where
  bezierCommands = arcToBeziers (transform, c) angleStart angleExtent

  m !*! p = (`dot` p) <$> m

  angle = toRadian rotateX `mod'` (2 * pi)

  -- Step 1 : Compute (x1', y1') - the transformed start point
  -- Compute the midpoint of the line between the current and the end point
  midpoint@(V2 dx2 dy2) = (orig - pos) * 0.5
  rotation = mkRotation angle 
  p1@(V2 x1 y1) = trace (printf "rotation: %s" (show rotation)) $ rotation !* midpoint
  p1Square@(V2 x1_sq y1_sq) = p1 * p1

  radius@(V2 rx ry)
    | radiusCheck > 1 = trace ("radius modified") $ localRadius ^* sqrt radiusCheck
    | otherwise = trace ("radius untouched") $ localRadius
    where
      -- Sign of the radii is ignored (behaviour specified by the spec)
      localRadius = V2 (abs radX) (abs radY)
      
      -- We simplify the calculations by transforming the arc so that the origin is at the
      -- midpoint calculated above followed by a rotation to line up the coordinate axes
      -- with the axes of the ellipse.
      V2 cx cy = p1Square / (localRadius * localRadius)
      radiusCheck = cx + cy

  -- Step 2 : Compute (cx1, cy1) - the transformed centre point
  sign | large == sweep = -1
       | otherwise = 1

  V2 rx_sq ry_sq = radius * radius

  sq = trace ("radius: " ++ show radius) $ max 0 $
    ((rx_sq * ry_sq) - (rx_sq * y1_sq) - (ry_sq * x1_sq)) /
    ((rx_sq * y1_sq) + (ry_sq * x1_sq))

  coef = sign * sqrt (trace ("sq: " ++ show sq) sq)
  c1@(V2 cx1 cy1) = trace ("coef: " ++ show coef) $ V2 (coef * ((rx * y1) / ry)) (coef * (- (ry * x1) / rx))

  -- Step 3 : Compute (cx, cy) from (cx1, cy1)
  s@(V2 sx2 sy2) = trace ("c1: " ++ show c1) $ (orig + pos) * 0.5
  c@(V2 cx cy) = trace ("s: " ++ show s) $ s + (rotation !* c1)

  -- Step 4 : Compute the angleStart (angle1) and the angleExtent (dangle)
  u@(V2 ux uy) = trace ("c: " ++ show c) $ (p1 - c) / radius
  v@(V2 vx vy) = trace ("u: " ++ show u) $ (negate p1 - c) / radius

  -- Compute the angle start
  sign' | uy < 0 = -1
        | otherwise = 1

  angleStart = (sign' * acos (ux / L.norm u)) `mod'` (2 * pi)

  -- the angle extent
  p = trace ("angleStart: " ++ show angleStart) $ ux * vx + uy * vy
  n = sqrt (L.quadrance u * L.quadrance v)

  sign'' | ux * vy - uy * vx < 0 = -1
         | otherwise = 1

  angleExtent = trace ("angleExtent v: " ++ show v) $ clampedExtent `mod'` (2 * pi) where
    v = trace (printf "p: %g n: %g" p n) $ sign'' * acos (p / n)
    clampedExtent 
      | not sweep && v > 0 = v - 2 * pi
      | sweep && v < 0 = v + 2 * pi
      | otherwise = v

  -- Many elliptical arc implementations including the Java2D and Android ones, only
  -- support arcs that are axis aligned.  Therefore we need to substitute the arc
  -- with bezier curves.  The following method call will generate the beziers for
  -- a unit circle that covers the arc angles we want.
  transform = mkScale rx ry L.!*! mkRotation angle

arcToBeziers :: (L.M22 Double, L.V2 Double) -> Double -> Double -> [PathCommand]
arcToBeziers ttt@(transform, pos) angleStart angleExtent =
    trace (printf "trans: %s %g -> %g [%d]" (show ttt) angleStart angleExtent segmentCount) $
        if angleExtent == angleExtent then
            computeSegment <$> [0 .. segmentCount - 1]
        else mempty where
  toFinal p = (transform L.!* p) + pos
  segmentCount :: Int
  segmentCount = ceiling $ abs angleExtent / (pi / 2)
  increment = angleExtent / fromIntegral segmentCount

  -- The length of each control point vector is given by the following formula.
  controlLength = 4 / 3 * sin (increment * 0.5) / (1.0 + cos (increment * 0.5))

  computeSegment i = CurveTo OriginAbsolute [(toFinal p0, toFinal p1, toFinal p2)] where
    angle = angleStart + fromIntegral i * increment
    --  Calculate the control vector at this angle
    (dx, dy) = (cos angle, sin angle)

    -- First control point
    p0 = L.V2 (dx - controlLength * dy) (dy + controlLength * dx)

    angle' = angle + increment
    (dx', dy') = (cos angle', sin angle')
    p1 = L.V2 (dx' + controlLength * dy') (dy' - controlLength * dx')
    p2 = L.V2 dx' dy'

-- | Create a 2 dimensional rotation matrix given an angle
-- expressed in radians.
mkRotation :: Floating a => a -> L.M22 a
mkRotation angle =
  L.V2 (L.V2 ca (-sa))
       (L.V2 sa ca)
  where
    ca = cos angle
    sa = sin angle

mkScale :: Floating a => a -> a -> L.M22 a
mkScale sx sy =
  L.V2 (L.V2 sx  0)
       (L.V2  0 sy)

mkRota' :: Floating a => a -> L.M22 a
mkRota' angle =
  L.V2 (L.V2 ca sa)
       (L.V2 (-sa) ca)
  where
    ca = cos angle
    sa = sin angle

arcToSegments :: RPoint -> (Coord, Coord, Coord, Bool, Bool, RPoint)
              -> [PathCommand]
arcToSegments orig (radX, radY, rotateX, large, sweep, pos) =
    trace ("arcToSegments: " ++ show angleSampling) $ 
    [segmentToBezier transBackward (V2 xc yc) th2 th3
            | (th2, th3) <- zip angleSampling $ tail angleSampling]
  where
    angleSampling =
        [th0 + i * th_arc / fromIntegral segmentCount | i <- fromIntegral <$> [0 .. segmentCount]]
    theta = toRadian rotateX
    rotation = mkRota' theta

    V2 px py =
      (mkRota' theta !* (orig L.^-^ pos)) ^* 0.5

    (rx, ry)
      | tmp > 1 = (rx' * sqtmp, ry' * sqtmp)
      | otherwise = (rx', ry')
      where
        sqtmp = sqrt tmp
        tmp = (px * px) / (rx' * rx') + (py * py) / (ry' * ry')
        rx' = abs radX
        ry' = abs radY

    transBackward = mkRotation theta !*! scaled (V2 rx ry)
    trans = scaled (V2 (1 / rx) (1 / ry)) !*! rotation

    orig'@(V2 x0 y0) = trans !* orig
    pos'@(V2 x1 y1) = trans !* pos
    delta = pos' L.^-^ orig'
    d = delta `dot` delta

    sfactor | sweep == large = - factor
            | otherwise = factor
      where
        factor = sqrt . max 0 $ 1 / d - 0.25

    xc = 0.5 * (x0 + x1) - sfactor * (y1-y0)
    yc = 0.5 * (y0 + y1) + sfactor * (x1-x0)

    th0 = atan2 (y0 - yc) (x0 - xc)
    th1 = atan2 (y1 - yc) (x1 - xc)

    th_arc | tmp < 0 && sweep = tmp + 2 * pi
           | tmp > 0 && not sweep = tmp - 2 * pi
           | otherwise = tmp
      where
        tmp = th1 - th0

    segmentCount :: Int
    segmentCount = ceiling . abs $ th_arc / (pi / 2 + 0.001)

segmentToBezier :: L.M22 Coord ->  RPoint -> Coord -> Coord -> PathCommand
segmentToBezier trans (V2 cx cy) th0 th1 =
    CurveTo OriginAbsolute [(trans !* p1, trans !* p2, trans !* p3)]
  where
    th_half = 0.5 * (th1 - th0)
    t = (8 / 3) * sin (th_half * 0.5) * sin (th_half * 0.5) / sin th_half
    
    p1 = V2 (cx + cos th0 - t * sin th0) (cy + sin th0 + t * cos th0)
    p3@(V2 x3 y3) = V2 (cx + cos th1) (cy + sin th1)
    p2 = V2 (x3 + t * sin th1) (y3 - t * cos th1)
-- -}
