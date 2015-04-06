{-# LANGUAGE CPP #-}

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( (<$>) )
#endif

import Criterion
import Criterion.Main
import Graphics.Text.TrueType( emptyFontCache )
import Graphics.Svg
import Graphics.Rasterific.Svg

main :: IO ()
main = do
  f <- loadSvgFile "test/tiger.svg"
  {-cache <- loadCreateFontCache-}
  case f of
     Nothing -> putStrLn "Error while loading SVG"
     Just doc -> do
       let loader = fst <$> renderSvgDocument emptyFontCache Nothing 96 doc
       defaultMainWith defaultConfig
            [ bench "Tiger render" $ whnfIO loader ]

