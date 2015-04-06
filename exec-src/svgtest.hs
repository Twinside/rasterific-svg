{-# LANGUAGE CPP #-}
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( (<$>) )
import Data.Foldable( foldMap )
#endif

import Control.Monad( forM_ )
import Data.Monoid( (<>) )
import Data.List( isSuffixOf, sort )
import System.Environment( getArgs )
import System.Directory( createDirectoryIfMissing
                       , getDirectoryContents
                       )
import qualified Text.Blaze.Html5 as HT
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as H
import qualified Text.Blaze.Html.Renderer.String as H
import System.FilePath( dropExtension, (</>), (<.>), splitFileName )

import Codec.Picture( writePng )

import Graphics.Text.TrueType( FontCache )
import Graphics.Rasterific.Svg
import Graphics.Svg hiding ( text, path )
{-import Debug.Trace-}
{-import Text.Printf-}
{-import Text.Groom-}

loadRender :: [String] -> IO ()
loadRender [] = putStrLn "not enough arguments"
loadRender [_] = putStrLn "not enough arguments"
loadRender (svgfilename:pngfilename:_) = do
  f <- loadSvgFile svgfilename
  case f of
     Nothing -> putStrLn "Error while loading SVG"
     Just doc -> do
        cache <- loadCreateFontCache "fonty-texture-cache"
        (finalImage, _) <- renderSvgDocument cache Nothing 96 doc
        writePng pngfilename finalImage

testOutputFolder :: FilePath
testOutputFolder = "gen_test"

img :: FilePath -> Int -> Int -> H.Html
img path _w _h = H.img H.! H.src (H.toValue path)

table :: [H.Html] -> [[H.Html]] -> H.Html
table headers cells =
  H.table $ header <> foldMap (H.tr . foldMap H.td) cells
  where header = H.tr $ foldMap H.th headers

testFileOfPath :: FilePath -> FilePath
testFileOfPath path = testOutputFolder </> base <.> "png"
  where (_, base) = splitFileName path

svgTestFileOfPath :: FilePath -> FilePath
svgTestFileOfPath path = testOutputFolder </> base <.> "svg"
  where (_, base) = splitFileName path


text :: String -> H.Html
text txt = H.toHtml txt <> H.br

generateFileInfo :: FilePath -> [H.Html]
generateFileInfo path =
    [ text path, img path 0 0
    , img pngRef 0 0
    , img (testFileOfPath path) 0 0
    , img (svgTestFileOfPath path) 0 0]
  where
    pngRef = dropExtension path <.> "png"

toHtmlDocument :: H.Html -> String
toHtmlDocument html = H.renderHtml $
  H.html $ H.head (HT.title $ H.toHtml "Test results")
        <> H.body html
                
analyzeFolder :: FontCache -> FilePath -> IO ()
analyzeFolder cache folder = do
  createDirectoryIfMissing True testOutputFolder
  fileList <- sort . filter (".svg" `isSuffixOf`) <$> getDirectoryContents folder
  let hdr = H.toHtml <$> ["name", "W3C Svg", "W3C ref PNG", "mine", "svgmine"]
      all_table =
        table hdr . map generateFileInfo $ map (folder </>) fileList
      doc = toHtmlDocument all_table
      (_, folderBase) = splitFileName folder

  print fileList

  writeFile (folder </> ".." </> folderBase <.> "html") doc
  forM_ fileList $ \p -> do
    let realFilename = folder </> p
    putStrLn $ "Loading: " ++ realFilename
    svg <- loadSvgFile realFilename
    {-putStrLn $ groom svg-}
    case svg of
      Nothing -> putStrLn $ "Failed to load " ++ p
      Just d -> do
        putStrLn $ "   => Rendering " ++ show (documentSize 96 d)
        (finalImage, _) <- renderSvgDocument cache Nothing 96 d
        writePng (testFileOfPath p) finalImage

        putStrLn "   => XMLize"
        saveXmlFile (svgTestFileOfPath p) d


testSuite :: IO ()
testSuite = do
    cache <- loadCreateFontCache "rasterific-svg-font-cache"
    analyzeFolder cache "w3csvg"
    {-analyzeFolder cache "test"-}

main :: IO ()
main = do
    args <- getArgs
    case args of
      "test":_ -> testSuite
      _ -> loadRender args

