{-
-- EPITECH PROJECT, 2025
-- imageCompressor
-- File description:
-- imageCompressor
-}

module Main (main) where

import System.Environment ( getArgs )
import System.IO.Error (tryIOError, isDoesNotExistError)
import Control.Exception ( handle, throw )

import Parser
import Image
import Algo
import Error

displayHelp :: IO ()
displayHelp = mapM_ putStrLn
  ["USAGE: ./imageCompressor -n N -l L -f F\n",
  "\tN\tnumber of colors in the final image",
  "\tL\tconvergence limit",
  "\tF\tpath to the file containing the colors of the pixels"]

checkFileExistence :: FilePath -> IO (Either IOError String)
checkFileExistence path = tryIOError (readFile path)

handleFileContent :: Options -> IO ()
handleFileContent opt = do
  result <- checkFileExistence (f opt)
  case result of
    Left e
      | isDoesNotExistError e -> throw $ ArgError "No such file"
      | otherwise -> throw e
    Right file ->
      let pixelList = readImage file in 
        (putStrLn $ "args : " ++ show (n opt) ++ ", " ++ show (l opt) ++ ", "
          ++ f opt) >> kMeanAlgo pixelList (n opt) (l opt)

main :: IO ()
main = handle errorHandler $ do
  args <- getArgs
  optionsList <- parser args
  case optionsList of
    Right opt -> handleFileContent opt
    _ -> displayHelp
