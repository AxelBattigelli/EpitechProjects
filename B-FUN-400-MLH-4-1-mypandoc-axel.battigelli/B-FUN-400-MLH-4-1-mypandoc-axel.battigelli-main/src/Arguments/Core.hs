{-
-- EPITECH PROJECT, 2025
-- myPandoc
-- File description:
-- Core.hs
-}

module Arguments.Core (parseArgs, usage, showHelp, processFile,
  defaultOptions, Options(..), optionsDescr, detectFormat, 
  determineInputFormat) where

import System.Console.GetOpt
import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)
import Input.Xml (parseXmlDocument)
import Input.Json (parseJsonDocument)
import Input.Markdown (parseMarkdownDocument)
import Output.Xml (documentToXml)
import Output.Json (documentToJson)
import Output.Markdown (documentToMarkdown)
import Output.Html (documentToHtml)
import Output.Yaml (documentToYaml)
import Document (Document)

data Options = Options
  { inputFile  :: FilePath
  , inputFmt   :: Maybe String
  , outputFile :: Maybe FilePath
  , outputFmt  :: String
  , showHelp   :: Bool
  } deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options "" Nothing Nothing "" False

usage :: String
usage = unlines
  [ "USAGE: ./mypandoc -i ifile -f oformat [-o ofile] [-e iformat]"
  , ""
  , "  ifile   path to the file to convert"
  , "  oformat output format (xml, json, markdown, html, yaml)"
  , "  ofile   path to the output file"
  , "  iformat input format (xml, json, markdown)"
  ]

optionsDescr :: [OptDescr (Options -> Options)]
optionsDescr =
  [ Option ['i'] [] (ReqArg (\f o -> o { inputFile = f }) "ifile")
      "path to the file to convert"
  , Option ['f'] [] (ReqArg (\f o -> o { outputFmt = f }) "oformat")
      "output format (xml, json, markdown)"
  , Option ['o'] [] (ReqArg (\f o -> o { outputFile = Just f }) "ofile")
      "path to the output file"
  ] ++ optionsDescr'


optionsDescr' :: [OptDescr (Options -> Options)]
optionsDescr' =
  [ Option ['e'] [] (ReqArg (\f o -> o { inputFmt = Just f }) "iformat")
      "input format (xml, json, markdown)"
  , Option ['h'] ["help"] (NoArg (\o -> o { showHelp = True }))
      "display this help and exit"
  ]

parseArgs :: [String] -> Either String Options
parseArgs argv =
  case getOpt Permute optionsDescr argv of
    (optsFns, [], []) ->
      let opts = foldl (flip id) defaultOptions optsFns
      in if (null (inputFile opts) || null (outputFmt opts)) && 
        not (showHelp opts) 
           then Left "Missing required options: -i and -f"
           else Right opts
    (_, _, errs) -> Left (concat errs)

detectFormat :: String -> Maybe String
detectFormat content =
  case break (== '.') (reverse content) of
    (_    , "")        -> Nothing
    (revExt, '.' : _)  ->
      let ext = reverse revExt in
      case ext of
        "md" -> Just "markdown"
        _    -> Just ext

handleError :: String -> IO a
handleError msg = hPutStrLn stderr msg >> exitWith (ExitFailure 84)

processFile :: Options -> IO ()
processFile opts = do
  content <- readFile (inputFile opts)
  let inFmt = determineInputFormat opts content
  doc <- parseDocument inFmt content
  outputResult opts doc

determineInputFormat :: Options -> String -> String
determineInputFormat opts _content =
  case inputFmt opts of
    Just fmt -> fmt
  -- Nothing  -> maybe "unknown" id (detectFormat content)
    Nothing  -> maybe "unknown" id (detectFormat (inputFile opts))

parseDocument :: String -> String -> IO Document
parseDocument "xml" content =
  case parseXmlDocument content of
    Right d  -> return d
    Left err -> handleError ("XML parsing error: " ++ err)
parseDocument "json" content =
  case parseJsonDocument content of
    Right d  -> return d
    Left err -> handleError ("JSON parsing error: " ++ err)
parseDocument "markdown" content =
  case parseMarkdownDocument content of
    Right d  -> return d
    Left err -> handleError ("MARKDOWN parsing error: " ++ err)
parseDocument _ _ = handleError "Unsupported input format"

outputResult :: Options -> Document -> IO ()
outputResult opts doc =
  let outputStr = renderOutput (outputFmt opts) doc
  in
    writeOutput (outputFile opts) outputStr

renderOutput :: String -> Document -> String
renderOutput fmt doc = case fmt of
  "xml"      -> documentToXml doc
  "json"     -> documentToJson doc
  "markdown" -> documentToMarkdown doc
  "html"     -> documentToHtml doc
  "yaml"     -> documentToYaml doc
  "native"   -> show doc
  _          -> "Unsupported output format"

writeOutput :: Maybe FilePath -> String -> IO ()
writeOutput (Just path) content = writeFile path content
writeOutput Nothing     content = putStrLn content
