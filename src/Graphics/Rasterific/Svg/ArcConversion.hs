{-# LANGUAGE BangPatterns #-}
-- |
-- see https://github.com/GNOME/librsvg/blob/ebcbfae24321f22cd8c04a4951bbaf70b60d7f29/rust/src/path_builder.rs
module Graphics.Rasterific.Svg.ArcConversion( arcToSegments ) where

import Graphics.Svg.Types
import Linear( M22
             , nearZero
             , dot
             , (!*!)
             , (!*)
             , V2( V2 )
             , norm
             , quadrance
             , scaled )

import Debug.Trace
import Text.Printf

toRadian :: Floating a => a -> a
toRadian v = v / 180 * pi

-- | Create a 2 dimensional rotation matrix given an angle
-- expressed in radians.
mkRotation :: Floating a => a -> M22 a
mkRotation angle =
  V2 (V2 ca (-sa))
     (V2 sa ca)
  where
    ca = cos angle
    sa = sin angle

mkRota' :: Floating a => a -> M22 a
mkRota' angle =
  V2 (V2 ca sa)
     (V2 (-sa) ca)
  where
    ca = cos angle
    sa = sin angle

arcSegment :: V2 Double -> Double -> Double -> V2 Double -> Double
           -> PathCommand
arcSegment c th0 th1 r angle = comm where
  !comm = CurveTo OriginAbsolute
    [( c + (finalRotation !* p1)
     , c + (finalRotation !* p2)
     , c + (finalRotation !* p3)
     )]

  !finalRotation = mkRotation $ toRadian angle

  !th_half = 0.5 * (th1 - th0)
  !t = trace (printf "th0: %g th1: %g" th0 th1) $ tlog "t: " $ (8.0 / 3.0) *
      sin (th_half * 0.5) *
      sin (th_half * 0.5) /
      sin th_half

  !cosTh0 = cos th0
  !sinTh0 = sin th0
  !cosTh1 = cos th1
  !sinTh1 = sin th1

  !p1 = r * V2 (cosTh0 - t * sinTh0) (sinTh0  + t * cosTh0)
  !p3 = r * V2 cosTh1 sinTh1
  !p2 = p3 + r * V2 (t * sinTh1) (-t * cosTh1)
  
tlog :: Show a => String -> a -> a
tlog _msg v = v -- trace (msg ++ show v) v

-- See Appendix F.6 Elliptical arc implementation notes
-- http://www.w3.org/TR/SVG/implnote.html#ArcImplementationNotes */
arc :: V2 Double -> Double -> Double -> Double -> Bool -> Bool -> V2 Double
    -> [PathCommand]
arc p1@(V2 x1 y1) rxOrig ryOrig x_axis_rotation is_large_arc is_sweep p2@(V2 x2 y2)
    | p1 == p2 = mempty
    | nearZero (abs rxOrig) || nearZero (abs ryOrig) = [LineTo OriginAbsolute [p2]]
    | kCheck == 0 = mempty
    | norm kk == 0 = mempty
    | k5Norm == 0 = mempty
    | otherwise = segs
  where
    f = tlog "f: " $ trace (printf "arc(/*x1:*/%g, /*y1:*/%g, /*rx:*/%g, /*ry:*/%g, /*x_axis_rotation:*/%g, /*is_large_arc:*/%s, /*is_sweep:*/%s, /*x2:*/%g, /*y2:*/%g) " x1 y1 rxOrig ryOrig x_axis_rotation (show is_large_arc) (show is_sweep) x2 y2) $ toRadian x_axis_rotation
    
    k = tlog "k: " $ (p1 - p2) * 0.5
    p1_@(V2 x1_ y1_) = tlog "p1_: " $ mkRota' f !* k
    
    radius@(V2 rx ry)
        | gamma > 1 = tlog "radius_gamma: " $ V2 (abs rxOrig * sqrt gamma) (abs ryOrig * sqrt gamma)
        | otherwise = tlog "radius: " $ V2 (abs rxOrig) (abs ryOrig)
      where gamma = (x1_ * x1_) / (rxOrig * rxOrig) + (y1_ * y1_) / (ryOrig * ryOrig)
    
    sweepCoeff | is_sweep == is_large_arc = -1
               | otherwise = 1

    -- Compute the center
    kCheck = tlog "kCheck: " $ rx * rx * y1_ * y1_ + ry * ry * x1_ * x1_
    
    kc = tlog "kc: " $ (sweepCoeff *) . sqrt . abs $ (rx * rx * ry * ry) / kCheck - 1.0
    
    c_@(V2 cx_ cy_) = tlog "c_: "$V2 (kc * rx * y1_ / ry) (-kc * ry * x1_ / rx)
    c@(V2 cx cy) = tlog "c: " $ (mkRotation f !* c_) + (p1 + p2) * 0.5
    
    -- Compute start angle
    kk@(V2 k1 k2) = tlog "kk: " $ (p1_ - c_) / radius
    kkk@(V2 k3 k4) = tlog "kkk: " $ ((-p1_) - c_) / radius

    theta1 = tlog "theta1: " $ (if k2 < 0 then negate else id) . acos . min 1 . max (-1) $ k1 / norm kk
    
    -- Compute delta_theta
    k5Norm = tlog "k5Norm: " $ sqrt $ quadrance kk * quadrance kkk
    
    delta_theta
      | is_sweep && v < 0.0 = tlog "delta_theta is_sweep: " $ v + 2 * pi
      | not is_sweep && v > 0.0 = tlog "delta_theta not is_sweep: " $ v - 2 * pi
      | otherwise = tlog "delta_thata: " $ v
      where
        vBase = acos . min 1 . max (-1) $ (k1 * k3 + k2 * k4) / k5Norm;
        v | k1 * k4 - k3 * k2 < 0.0 = - vBase
          | otherwise = vBase
    
    -- Now draw the arc
    n_segs :: Int
    n_segs = tlog "n_segs: " $ ceiling . abs $ delta_theta / (pi * 0.5 + 0.001)

    angleAt v = theta1 + fromIntegral v * delta_theta / fromIntegral n_segs
    
    segs = trace ("n_segs:" ++ show n_segs) $
      [arcSegment c (angleAt i) (angleAt $ i + 1) (V2 rx ry) x_axis_rotation | i <- [0 .. n_segs - 1]]

arcToSegments :: RPoint -> (Coord, Coord, Coord, Bool, Bool, RPoint)
              -> [PathCommand]
arcToSegments orig (radX, radY, rotateX, large, sweep, pos) =
    arc orig radX radY rotateX large sweep pos

