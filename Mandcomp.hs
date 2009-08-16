{-# LANGUAGE BangPatterns #-}
module Mandcomp
  where

import Control.Parallel.Strategies
import Graphics.UI.GLUT
import Data.Array.IO

import Mandstate

type Pix = IOUArray Int Double
instance (Num a) => NFData (Color3 a)

-- Number of iterations to escape
mandPoint :: Int -> Double -> Double -> Double -> Double -> Double
mandPoint !n !x !y cx cy | n > maxIter       = 0.0
					     | (x2 + y2) > 4.0   = 1.0 - logBase 2 (0.5 * logBase 2 (x*x + y*y)) + fromIntegral n
					     | otherwise		 = mandPoint (n+1) (x2 - y2 + cx) (2*x*y + cy) cx cy where
	x2 = x*x
	y2 = y*y

compPoints :: Double -> Double -> Double -> Pix -> IO ()
compPoints xm ym rng arr = mapM_ (\(x,y) -> writePix x y arr) indicies where
	writePix x y a = writeArray a (x + width*y) (mandPoint 0 0.0 0.0 cx cy) where
		fi = fromIntegral
		cx = rng * (fi x - fi w2) / fi w2 + xm
		cy = rng * (fi y - fi h2) / fi h2 + ym

-- Colour a vertex
colorMand :: Double -> Double -> Color3 GLdouble
colorMand 0.0 _ = fmap realToFrac $ Color3 0.0 0.0 0.0
colorMand m cm = fmap realToFrac $ Color3 r g b where
	r = 0.5 + 0.5 * cos (m * cm) 
	g = 0.5 + 0.5 * cos ((m + 16.0) * cm)
	b = 0.5 + 0.5 * cos ((m + 32.0) * cm)