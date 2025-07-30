{-
-- EPITECH PROJECT, 2025
-- imageCompressor
-- File description:
-- error
-}

module Error (
  errorHandler,
  ExceptionType (..)
) where

import Control.Exception
import System.IO
import System.Exit ( exitWith, ExitCode(ExitFailure) )

data ExceptionType = ArgError String
                   | ExecError String
                   deriving (Show)
instance Exception ExceptionType

errorHandler :: ExceptionType -> IO ()
errorHandler (ArgError s) = hPutStrLn stderr s >> exitWith (ExitFailure 84)
errorHandler (ExecError s) = hPutStrLn stderr s >> exitWith (ExitFailure 84)
