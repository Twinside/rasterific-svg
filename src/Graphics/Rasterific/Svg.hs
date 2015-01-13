module Graphics.Rasterific.Svg
                    ( LoadedFonts
                    , Result( .. )
                    , renderSvgDocument
                    , renderSvgFile
                    , loadCreateFontCache
                    ) where

import qualified Data.ByteString.Lazy as B
import qualified Graphics.Rasterific.Svg.RasterificRender as RR
import Data.Binary( encodeFile, decodeOrFail )
import Graphics.Svg.Types hiding ( Dpi )
import Graphics.Svg hiding ( Dpi )
import Graphics.Rasterific.Svg.RenderContext

import System.Directory( doesFileExist )
import Graphics.Text.TrueType


import Codec.Picture( Image
                    , PixelRGBA8( .. )
                    , writePng )

{-import Graphics.Svg.CssParser-}

renderSvgDocument :: FontCache -> Maybe (Int, Int) -> Dpi -> Document
                  -> IO (Image PixelRGBA8, LoadedFonts)
renderSvgDocument cache size dpi =
    RR.renderSvgDocument cache size dpi . applyCSSRules . resolveUses

data Result
  = ResultSuccess
  | ResultError String
  deriving (Eq, Show)

-- | Convert an SVG file to a PNG file, return True
-- if the operation went without problems.
renderSvgFile :: FilePath -> FilePath -> IO Result
renderSvgFile svgfilename pngfilename = do
  f <- loadSvgFile svgfilename
  case f of
     Nothing -> return $ ResultError "Error while loading SVG"
     Just doc -> do
        cache <- loadCreateFontCache "fonty-texture-cache"
        (finalImage, _) <- renderSvgDocument cache Nothing 96 doc
        writePng pngfilename finalImage
        return ResultSuccess

loadCreateFontCache :: FilePath -> IO FontCache
loadCreateFontCache filename = do
  exist <- doesFileExist filename
  if exist then loadCache else createWrite
  where
    loadCache = do
      bstr <- B.readFile filename
      case decodeOrFail bstr of
        Left _ -> createWrite
        Right (_, _, v) -> return v
      
    createWrite = do
      cache <- buildCache
      encodeFile filename cache
      return cache

