{-
-- EPITECH PROJECT, 2025
-- imageCompressor
-- File description:
-- algo
-}

module Algo (
  kMeanAlgo
) where

import System.Environment ()
import System.Random
import Data.Ord (comparing)
import Data.Function (on)
import Data.List (sort, transpose, minimumBy, groupBy, permutations)

-- Define types for Point and Color
type Point = (Int, Int)
type Color = (Int, Int, Int)
type Pixel = (Point, Color)
type Cluster = (Color, [Pixel])

-- Algo part

centroidToColor :: [Double] -> Color
centroidToColor [r, g, b] = (round r, round g,round b)

kMeanAlgo :: [Pixel] -> Int -> Double -> IO ()
kMeanAlgo [] _ _ = return ()
kMeanAlgo pixels points convergenceLimit = do
  g <- initStdGen
  printClusters (map (\c -> (centroidToColor $ centroid c, c)) $
    kmeans' g points convergenceLimit pixels)

getVect :: Pixel -> [Double]
getVect (_, (r, g, b)) = [fromIntegral r, fromIntegral g, fromIntegral b]

dist :: [Double] -> [Double] -> Double
dist a b = sqrt . sum $ zipWith (\x y-> (x-y) ^ 2) a b

centroid :: [Pixel] -> [Double]
centroid points = map (flip (/) l . sum) $ transpose (map getVect points)
    where l = fromIntegral $ length points

closest :: [[Double]] -> [Double] -> [Double]
closest points point = minimumBy (comparing $ dist point) points

recluster' :: [[Double]] -> [Pixel] -> [[Pixel]]
recluster' centroids points = map (map snd) $
    groupBy ((==) `on` fst) reclustered
    where reclustered = sort [(closest centroids (getVect a), a) | a <- points]

recluster :: [[Pixel]] -> [[Pixel]]
recluster clusters = recluster' centroids $ concat clusters
    where centroids = map centroid clusters

part :: Int -> [Pixel] -> [[Pixel]]
part x ys
     | zs' == [] = [zs]
     | otherwise = zs : part x zs'
    where (zs, zs') = splitAt x ys

getConvergence :: [[Pixel]] -> [[Pixel]] -> Double
getConvergence left right = maximum $
  zipWith (\l r -> dist (centroid l) (centroid r)) left right

-- | Recluster points
kmeans'' :: Double -> [[Pixel]] -> [[Pixel]]
kmeans'' convergenceLimit clusters
    | getConvergence clusters clusters' < convergenceLimit = clusters
    | otherwise             = kmeans'' convergenceLimit clusters'
    where clusters' = recluster clusters

factorial :: Int -> Int
factorial n
  | n <= 1 = 1
  | otherwise = n * factorial (n - 1)

shuffle :: (RandomGen g) => [Pixel] -> g -> [Pixel]
shuffle pixel g = 
  let (newPixel, _) = next g
  in pixel
shuffle xs g = (permutations xs) !! 
  (fst $ uniformR (0, (factorial $ length xs) - 1) g)

kmeans' ::(RandomGen g) => g -> Int -> Double -> [Pixel] -> [[Pixel]]
kmeans' g k convergenceLimit points = kmeans'' convergenceLimit $
    part l $ shuffle points g
    where l = (length points + k - 1) `div` k


-- Display part

printPixel :: [Pixel] -> IO ()
printPixel [] = return ()
printPixel (((x, y), (r, g, b)):xs) =
    putStrLn ("(" ++ show x ++ "," ++ show y ++ ") ("
        ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")") >>
    printPixel xs

printClusters :: [Cluster] -> IO ()
printClusters [] = return ()
printClusters (((r, g, b), pixels):xs) =
    putStrLn ("--\n(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")\n-") >>
    printPixel pixels >> 
    printClusters xs
