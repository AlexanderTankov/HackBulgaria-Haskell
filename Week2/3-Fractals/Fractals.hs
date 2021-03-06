module Fractals where

import Data.Complex (Complex(..), magnitude)


-- The complex number a + bi corresponds to the point (a, b)
type Point = Complex Double
type Coordinate = (Double, Double)
-- A Fractal is a function which given a coordinate
-- returns the number of iterations before diverging
type Fractal = Int -> Int -> Int

-- Number of iterations after which we say that a point doesn't diverge
maxIterations :: Num a => a
maxIterations = 127

-- The size of the image we'll colour
screen :: Num a => (a, a)
screen = (600, 600)

-- The part of the fractal we want to see
window :: (Coordinate, Coordinate)
window = ((-1.9, 1.3), (0.7, -1.3))


-- Mapping a pixel to a coordinate
pixelToCoordinate :: Double -> Double -> Int -> Int -> (Coordinate, Coordinate) -> Coordinate
pixelToCoordinate fstScr sndScr x y ((minx, maxy), (maxx, miny)) =
    ((minx + divX * rangeX), (miny + divY * rangeY))
    where
        divX = fromIntegral x / fstScr
        divY = fromIntegral y / sndScr
        rangeX = maxx - minx
        rangeY = maxy - miny


-- Implement the formula for Mandelbrot's fractal
mandelbrot :: Point -- Coordinate for calculation
           -> Point -- Current z value
           -> Int   -- Number of current iteration
           -> Int   -- Iterations before diverging
mandelbrot c z iter
    | iter == maxIterations = 0
    | magnitude z > 2 = iter - 1
    | otherwise = (mandelbrot c (z^2 + c) (iter + 1))

-- Returning the number of iterations needed for given
-- point to diverge
drawMandelbrot :: Double -> Double -> Int -> Int -> Int
drawMandelbrot fstScr sndScr x y = mandelbrot (x' :+ y') (0 :+ 0) 0
  where (x', y') = pixelToCoordinate fstScr sndScr x y window
