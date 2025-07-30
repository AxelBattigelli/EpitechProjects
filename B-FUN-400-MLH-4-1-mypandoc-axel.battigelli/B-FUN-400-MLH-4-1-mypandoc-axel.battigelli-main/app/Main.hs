{-
-- EPITECH PROJECT, 2025
-- myPandoc
-- File description:
-- Main.hs
-}

module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import Arguments.Core (parseArgs, usage, showHelp, processFile)

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err -> 
      putStrLn (err ++ "\n" ++ usage) >>
      exitWith (ExitFailure 84)
    Right opts
      | showHelp opts -> putStrLn usage
      | otherwise     -> processFile opts
