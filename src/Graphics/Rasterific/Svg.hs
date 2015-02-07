-- | Svg renderer based on Rasterific.
--
-- Here is a simple example of loading a SVG file (using svg-tree)
-- rendering it to a picture, and saving it to a PNG (using Juicy.Pixels)
--
-- @
-- import Codec.Picture( writePng )
-- import Graphics.Svg( loadSvgFile )
-- import Graphics.Rasterific.Svg( loadCreateFontCache
--                               , renderSvgDocument
--                               )
-- loadRender :: FilePath -> FilePath -> IO ()
-- loadRender svgfilename pngfilename = do
--   f <- loadSvgFile svgfilename
--   case f of
--     Nothing -> putStrLn "Error while loading SVG"
--     Just doc -> do
--       cache <- loadCreateFontCache "fonty-texture-cache"
--       (finalImage, _) <- renderSvgDocument cache Nothing 96 doc
--       writePng pngfilename finalImage
-- @
--
module Graphics.Rasterific.Svg
                    ( -- * Main functions
                      drawingOfSvgDocument
                    , renderSvgDocument
                    , loadCreateFontCache
                      -- * Types
                    , LoadedElements( .. )
                    , Result( .. )
                    , DrawResult( .. )
                    , Dpi
                      -- * Other helper functions
                    , renderSvgFile
                    ) where

import qualified Data.ByteString.Lazy as B
import Graphics.Rasterific.Svg.RasterificRender( DrawResult( .. ) )
import qualified Graphics.Rasterific.Svg.RasterificRender as RR
import Data.Binary( encodeFile, decodeOrFail )
import Graphics.Svg.Types hiding ( Dpi )
import Graphics.Svg hiding ( Dpi )
import Graphics.Rasterific.Svg.RenderContext

import System.Directory( doesFileExist )
import Graphics.Text.TrueType


import qualified Codec.Picture as CP
import Codec.Picture( PixelRGBA8( .. )
                    , writePng )

{-import Graphics.Svg.CssParser-}

-- | Render an svg document to an image.
-- If you provide a size, the document will be stretched to
-- match the provided size.
--
-- The DPI parameter really should depend of your screen, but
-- a good default value is 96
--
-- The use of the IO Monad is there to allow loading of fonts
-- and referenced images.
renderSvgDocument :: FontCache          -- ^ Structure used to access fonts
                  -> Maybe (Int, Int)   -- ^ Optional document size
                  -> Dpi                -- ^ Current resolution for text and elements
                  -> Document           -- ^ Svg document
                  -> IO (CP.Image PixelRGBA8, LoadedElements)
renderSvgDocument cache size dpi =
    RR.renderSvgDocument cache size dpi . applyCSSRules . resolveUses

-- | Render an svg document to a Rasterific Drawing.
-- If you provide a size, the document will be stretched to
-- match the provided size.
--
-- The DPI parameter really should depend of your screen, but
-- a good default value is 96
--
-- The use of the IO Monad is there to allow loading of fonts
-- and referenced images.
drawingOfSvgDocument :: FontCache          -- ^ Structure used to access fonts
                     -> Maybe (Int, Int)   -- ^ Optional document size
                     -> Dpi                -- ^ Current resolution for text and elements
                     -> Document           -- ^ Svg document
                     -> IO (DrawResult, LoadedElements)
drawingOfSvgDocument cache size dpi =
    RR.drawingOfSvgDocument cache size dpi . applyCSSRules . resolveUses

-- | Rendering status.
data Result
  = ResultSuccess       -- ^ No problem found
  | ResultError String  -- ^ Error with message
  deriving (Eq, Show)

-- | Convert an SVG file to a PNG file, return True
-- if the operation went without problems.
-- 
-- This function will call loadCreateFontCache with
-- the filename "fonty-texture-cache"
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

-- | This function will create a font cache,
-- a structure allowing to quickly match a font
-- family name and style to a specific true type font
-- on disk.
--
-- The cache is saved on disk at the filepath given
-- as parameter. If a cache is found it is automatically
-- loaded from the file.
--
-- Creating the cache is a rather long operation (especially
-- on Windows), that's why you may want to keep the cache
-- around.
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

