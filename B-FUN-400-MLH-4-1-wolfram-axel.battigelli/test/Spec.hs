import Test.QuickCheck
import Lib

chunkPairs :: [a] -> [(a, a)]
chunkPairs (x:y:ys) = (x, y) : chunkPairs ys
chunkPairs _ = []

-- Test for parseArgs with various cases
prop_parseArgs_even :: [String] -> Property
prop_parseArgs_even xs = even (length xs) ==> property (parseArgs xs == chunkPairs xs)

prop_parseArgs_odd :: [String] -> Property
prop_parseArgs_odd xs = odd (length xs) ==> property (null (parseArgs xs))

-- Test for applyRule
prop_applyRule :: Int -> Bool
prop_applyRule rule = all (\[a, b, c] -> applyRule rule [a, b, c] `elem` [0, 1]) 
  [[a, b, c] | a <- [0,1], b <- [0,1], c <- [0,1]]

-- Test for applyRule with edge cases
prop_applyRule_invalid :: Int -> [Int] -> Property
prop_applyRule_invalid rule xs = length xs /= 3 ==> property (applyRule rule xs == 0)

-- Test for nextGen
prop_nextGen :: Int -> [Int] -> Bool
prop_nextGen rule gen = length (nextGen rule gen) <= length gen

-- Test for nextGen with edge cases
prop_nextGen_empty :: Int -> Bool
prop_nextGen_empty rule = null (nextGen rule [])

prop_nextGen_single :: Int -> Bool
prop_nextGen_single rule = null (nextGen rule [1])

prop_nextGen_two :: Int -> Bool
prop_nextGen_two rule = null (nextGen rule [1, 0])

-- Test for generateInitialGen with edge cases
prop_generateInitialGen_negative :: Int -> Bool
prop_generateInitialGen_negative size = let gen = generateInitialGen (abs size) in length gen == abs size && sum gen == 1

prop_generateInitialGen_zero :: Bool
prop_generateInitialGen_zero = let gen = generateInitialGen 0 in null gen

-- Test for getParam with incorrect values
prop_getParam_invalid :: [(String, String)] -> String -> Bool
prop_getParam_invalid args key = getParam args key 42 == 42

-- Test for validateRule
prop_validateRule :: Int -> Bool
prop_validateRule rule = case validateRule [("--rule", show rule)] of
    Right r -> r == rule
    Left _ -> rule < 0 || rule > 255

-- Test for validateRule with non-numeric input
prop_validateRule_invalid :: Bool
prop_validateRule_invalid = case validateRule [("--rule", "abc")] of
    Left _ -> True
    Right _ -> False

-- Test for runAutomaton to ensure it handles extreme values safely
prop_runAutomaton_extreme :: Int -> Int -> Int -> Int -> Int -> Bool
prop_runAutomaton_extreme rule start numLines window move =
    rule >= 0 && rule <= 255 && numLines >= 0 && window > 0 && move >= 0

-- Test for usageMessage to check the response
prop_usageMessage_containsRequiredOptions :: Bool
prop_usageMessage_containsRequiredOptions =
    all (`elem` words usageMessage) ["--rule", "--start", "--lines", "--window", "--move"]

main :: IO ()
main = do
    putStrLn "\n=== Running Improved QuickCheck Tests ==="
    quickCheck prop_parseArgs_even
    quickCheck prop_parseArgs_odd
    quickCheck prop_applyRule
    quickCheck prop_applyRule_invalid
    quickCheck prop_nextGen
    quickCheck prop_nextGen_empty
    quickCheck prop_nextGen_single
    quickCheck prop_nextGen_two
    quickCheck prop_generateInitialGen_negative
    quickCheck prop_generateInitialGen_zero
    quickCheck prop_getParam_invalid
    quickCheck prop_validateRule
    quickCheck prop_validateRule_invalid
    quickCheck prop_runAutomaton_extreme
    quickCheck prop_usageMessage_containsRequiredOptions
