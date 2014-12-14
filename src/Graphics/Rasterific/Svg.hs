module Graphics.Rasterific.Svg
                    ( LoadedFonts
                    , renderSvgDocument
                    ) where

import qualified Graphics.Rasterific.Svg.RasterificRender as RR
import Graphics.Svg.Types
import Graphics.Rasterific.Svg.RenderContext

import Graphics.Text.TrueType

import Graphics.Svg

import Codec.Picture( Image, PixelRGBA8( .. ) )

{-import Graphics.Svg.CssParser-}

renderSvgDocument :: FontCache -> Maybe (Int, Int) -> Document
                  -> IO (Image PixelRGBA8, LoadedFonts)
renderSvgDocument cache size =
    RR.renderSvgDocument cache size . applyCSSRules 

