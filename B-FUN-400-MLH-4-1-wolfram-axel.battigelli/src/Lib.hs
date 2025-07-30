{-
-- EPITECH PROJECT, 2025
-- wolfram
-- File description:
-- wolfram
-}

module Lib (
  parseArgs,
  applyRule,
  nextGen,
  printGen,
  runAutomaton,
  validateRule,
  getParam,
  usageMessage,
  generateInitialGen,
  runWithArgs,
) where

import Data.Bits (testBit)

parseArgs :: [String] -> [(String, String)]
parseArgs (x:y:ys) = (x, y) : parseArgs ys
parseArgs _ = []

applyRule :: Int -> [Int] -> Int
applyRule rule [a, b, c] =
    if testBit rule (a * 4 + b * 2 + c) then 1 else 0
applyRule _ _ = 0

nextGen :: Int -> [Int] -> [Int]
nextGen _ [] = []
nextGen _ [_] = []
nextGen _ [_, _] = []
nextGen rule (l:c:r:xs) = (applyRule rule [l,c,r]):(nextGen rule (c:r:xs))

printGen :: [Int] -> IO ()
printGen = putStrLn . map (\x -> if x == 1 then '*' else ' ')

runAutomaton :: Int -> Int -> Int -> Int -> Int -> [Int] -> IO ()
runAutomaton rule start numLines window move gen =
    mapM_ (\(i, j) -> 
        let expandedGen = expandGen i j
            middle = length expandedGen `div` 2 - window `div` 2
        in printGen . take window . drop (middle - move) $ expandedGen
    ) (zip [0..] (take numLines generations))
    where 
        generations = drop start (iterate (nextGen rule . expandGen 1) gen)
        expandGen i j = replicate i 0 ++ j ++ replicate i 0

validateRule :: [(String, String)] -> Either String Int
validateRule argMap =
    case lookup "--rule" argMap of
        Just ruleStr -> case reads ruleStr of
            [(rule, "")] | rule >= 0 && rule <= 255 -> Right rule
            _ -> Left "Error: Rule must be a number between 0 and 255"
        Nothing -> Left usageMessage

getParam :: Read a => [(String, String)] -> String -> a -> a
getParam argMap key defaultValue =
    maybe defaultValue read (lookup key argMap)

usageMessage :: String
usageMessage =
    "Usage: wolfram --rule <0-255> [--start N] [--lines N] [--window N]" ++
    "[--move N]"

generateInitialGen :: Int -> [Int]
generateInitialGen genSize =
    replicate (genSize `div` 2) 0 ++ [1] ++ replicate (genSize `div` 2) 0

runWithArgs :: [(String, String)] -> Int -> IO ()
runWithArgs argMap rule =
    let start = getParam argMap "--start" 0
        numLines = getParam argMap "--lines" (maxBound)
        window = getParam argMap "--window" 80
        move = getParam argMap "--move" 0
        genSize = 115 * window
        initialGen = generateInitialGen genSize
    in runAutomaton rule start numLines window move initialGen
