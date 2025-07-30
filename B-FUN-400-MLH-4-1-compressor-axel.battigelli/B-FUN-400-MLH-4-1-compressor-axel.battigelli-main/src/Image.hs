{-
-- EPITECH PROJECT, 2025
-- imageCompressor
-- File description:
-- image
-}

module Image (
  readImage
) where

-- Define types for Point and Color
type Point = (Int, Int)
type Color = (Int, Int, Int)
type Pixel = (Point, Color)

readImage :: String -> [Pixel]
readImage content = map readPixel (lines content)

readPixel :: String -> Pixel
readPixel line = (getPositions posStr, getColors colorStr)
  where
    parts = words line
    posStr = filter (`notElem` "()") (head parts)
    colorStr = filter (`notElem` "()") (last parts)

getPositions :: String -> Point
getPositions posStr = (read x, read y)
  where
    [x, y] = words (map comaToSpace posStr)

getColors :: String -> Color
getColors colorStr = (read r, read g, read b)
  where
    [r, g, b] = words (map comaToSpace colorStr) 

comaToSpace :: Char -> Char
comaToSpace ',' = ' '
comaToSpace a = a
