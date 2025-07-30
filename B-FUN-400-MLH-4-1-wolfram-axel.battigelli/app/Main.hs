{-
-- EPITECH PROJECT, 2025
-- wolfram
-- File description:
-- wolfram
-}
module Main (main) where

import Lib (runWithArgs, validateRule, parseArgs)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))

main :: IO ()
main = getArgs >>= \args ->
    let argMap = parseArgs args
    in case validateRule argMap of
        Right rule -> runWithArgs argMap rule
        Left err -> putStrLn err >> exitWith (ExitFailure 84)
