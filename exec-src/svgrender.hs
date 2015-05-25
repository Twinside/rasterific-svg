
{-# LANGUAGE CPP #-}
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( (<$>), (<*>), pure )
#endif

import Control.Applicative( (<|>) )
import Control.Monad( when )
import Data.Monoid( (<>) )
import Codec.Picture( writePng )
import System.Directory( getTemporaryDirectory )
import System.FilePath( (</>), replaceExtension )

import Options.Applicative( Parser
                          , ParserInfo
                          , argument
                          , execParser
                          , fullDesc
                          , header
                          , help
                          , helper
                          , info
                          , long
                          , metavar
                          , progDesc
                          , str
                          , switch
                          , auto
                          , option
                          )

import Graphics.Rasterific.Svg( loadCreateFontCache
                              , renderSvgDocument
                              )
import Graphics.Svg( loadSvgFile
                   , documentSize )
import System.Exit( ExitCode( ExitFailure, ExitSuccess )
                  , exitWith )

data Options = Options
  { _inputFile  :: !FilePath
  , _outputFile :: !FilePath
  , _verbose    :: !Bool
  , _width      :: !Int
  , _height     :: !Int
  , _dpi        :: !Int
  }

argParser :: Parser Options
argParser = Options
  <$> ( argument str
            (metavar "SVGINPUTFILE"
            <> help "SVG file to render to png"))
  <*> ( argument str
            (metavar "OUTPUTFILE"
            <> help ("Output file name, same as input with"
                    <> " different extension if unspecified."))
        <|> pure "" )
  <*> ( switch (long "verbose" <> help "Display more information") )
  <*> ( option auto
            (  long "width"
            <> help "Force the width of the rendered PNG"
            <> metavar "WIDTH" )
        <|> pure 0
        )
  <*> ( option auto
            (  long "height"
            <> help "Force the height of the rendered PNG"
            <> metavar "HEIGHT" )
        <|> pure 0 )
  <*> ( option auto
            (  long "dpi"
            <> help "DPI used for text rendering and various real life sizes"
            <> metavar "DPI" )
        <|> pure 96 )

progOptions :: ParserInfo Options
progOptions = info (helper <*> argParser)
      ( fullDesc
     <> progDesc "Convert SVGINPUTFILE into a png OUTPUTFILE"
     <> header "svgrender svg file renderer." )

outFileName :: Options -> FilePath
outFileName Options { _outputFile = "", _inputFile = inf } =
    replaceExtension inf "png"
outFileName opt = _outputFile opt

fixSize :: Options -> (Int, Int) -> (Int, Int)
fixSize opt (w, h) = (notNull (_width opt) w, notNull (_height opt) h)
  where
    notNull v v' = if v <= 0 then v' else v

runConversion :: Options -> IO ()
runConversion options = do
  tempDir <- getTemporaryDirectory 
  cache <- loadCreateFontCache $ tempDir </> "rasterific-svg-font-cache"
  let filename = _inputFile options
      whenVerbose = when (_verbose options) . putStrLn
  whenVerbose $ "Loading: " ++ filename 

  svg <- loadSvgFile filename
  case svg of
    Nothing -> do
      putStrLn $ "Failed to load " ++ filename
      exitWith $ ExitFailure 1

    Just d -> do
      let dpi = _dpi options
          size = fixSize options $ documentSize dpi d
      whenVerbose $ "Rendering at " ++ show size
      (finalImage, _) <- renderSvgDocument cache (Just size) dpi d
      writePng (outFileName options) finalImage
      exitWith ExitSuccess

main :: IO ()
main = execParser progOptions >>= runConversion

