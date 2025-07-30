{-
-- EPITECH PROJECT, 2025
-- imageCompressor
-- File description:
-- parser
-}

module Parser (
  parser,
  ArgumentType(..),
  Options(..)
) where

import Control.Exception ( throw )
import Error

-- Defined structure for arguments
data Options = Options
             { n :: Int
             , l :: Double
             , f :: String
             } deriving Show

data ArgumentType = Invalid
                  | Helper
                  | Other
                  deriving (Show, Enum)

parser :: [String] -> IO (Either ArgumentType Options)
parser args = case parseArgument args of
  Right r -> return $ Right r
  Left Helper -> return $ Left Helper
  Left _ -> throw $ ArgError "Invalid argument"

parseArgument :: [String] -> Either ArgumentType Options
parseArgument ["-h"] = Left Helper
parseArgument ["--help"] = Left Helper

parseArgument ["-n", nVal, "-l", lVal, "-f", fVal] = Right Options
  { n = read nVal :: Int, l = read lVal :: Double, f = fVal }
parseArgument ["-l", lVal, "-n", nVal, "-f", fVal] = Right Options
  { n = read nVal :: Int, l = read lVal :: Double, f = fVal }
parseArgument ["-f", fVal, "-n", nVal, "-l", lVal] = Right Options
  { n = read nVal :: Int, l = read lVal :: Double, f = fVal }

parseArgument ["-n", nVal, "-f", fVal, "-l", lVal] = Right Options
  { n = read nVal :: Int, l = read lVal :: Double, f = fVal }
parseArgument ["-l", lVal, "-f", fVal, "-n", nVal] = Right Options
  { n = read nVal :: Int, l = read lVal :: Double, f = fVal }
parseArgument ["-f", fVal, "-l", lVal, "-n", nVal] = Right Options
  { n = read nVal :: Int, l = read lVal :: Double, f = fVal }

parseArgument _ = Left Invalid
