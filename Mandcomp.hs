{-# LANGUAGE BangPatterns #-}
module Mandcomp (compMandPoints)
  where

import Control.Parallel.Strategies
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Mandstate

instance (Num a) => NFData (Color3 a)

maxIter = 800
chunkSize = (width * height) `div` (4 * 20) 

compMandPoints :: Double -> Double -> Double -> Double -> [(Int, Int, Color3 GLdouble)]
compMandPoints xmid ymid range cm = [(x,y,mandPoint 0 0.0 0.0 ((range*normx x) + xmid) ((range*normy y) + ymid) cm) | x <- [0  .. width-1], y <- [0 .. height-1] ] `using` parListChunk chunkSize rnf where
  normx i = (fi i - fi w2) / fi w2
  normy j = (fi j - fi h2) / fi h2

fi = fromIntegral

-- Number of iterations to escape
mandPoint :: Int -> Double -> Double -> Double -> Double -> Double -> Color3 GLdouble
mandPoint !n !x !y cx cy cm | n > maxIter       = Color3 0 0 0
							| (x*x + y*y) > 4.0 = colorMand x y n cm
							| otherwise		 = mandPoint (n+1) (x*x - y*y + cx) (2*x*y + cy) cx cy cm

-- Colour a vertex
colorMand :: Double -> Double -> Int -> Double -> Color3 GLdouble
colorMand x y n cm = fmap realToFrac $ Color3 r g b where
	m = 1.0 - (logBase 2 (logBase 2 $ sqrt (x*x + y*y))) + fromIntegral n
	r = 0.2 + 0.8 * cos (m * cm) 
	g = 0.4 + 0.6 * cos ((m + 16.0) * cm)
	b = 0.5 + 0.5 * cos ((m + 32.0) * cm)