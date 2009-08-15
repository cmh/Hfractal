{-# LANGUAGE BangPatterns #-}
module Mandcomp
  where

import Control.Parallel.Strategies
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.Array.IO
import Mandstate

type Pix = IOUArray Int Double
instance (Num a) => NFData (Color3 a)

maxIter = 400
indicies = [(i,j) | i <- [1..(width-1)], j <- [1..(height-1)]]

-- Number of iterations to escape
mandPoint :: Int -> Double -> Double -> Double -> Double -> Double
mandPoint !n !x !y cx cy | n > maxIter       = 0.0
					     | (x*x + y*y) > 4.0 = 1.0 - logBase 2 (logBase 2 (sqrt (x*x + y*y))) + fromIntegral n
					     | otherwise		 = mandPoint (n+1) (x*x - y*y + cx) (2*x*y + cy) cx cy	

compPoints :: Double -> Double -> Double -> Pix -> IO ()
compPoints xmid ymid range arr = mapM_ (\(x,y) -> writePix x y arr) indicies where
	writePix x y arr = do 	
		writeArray arr (x + width*y) (mandPoint 0 0.0 0.0 ((range*normx x) + xmid) ((range*normy y) + ymid))
	normx i = (fi i - fi w2) / fi w2
	normy j = (fi j - fi h2) / fi h2
	fi = fromIntegral

-- Colour a vertex
colorMand :: Double -> Double -> Color3 GLdouble
colorMand 0 _ = fmap realToFrac $ Color3 0.0 0.0 0.0
colorMand m cm = fmap realToFrac $ Color3 r g b where
	r = 0.5 + 0.5 * cos (m * cm) 
	g = 0.5 + 0.5 * cos ((m + 16.0) * cm)
	b = 0.5 + 0.5 * cos ((m + 32.0) * cm)