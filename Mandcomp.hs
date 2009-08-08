module Mandcomp (compMandPoints)
  where

import Control.Parallel.Strategies
--import Data.Array.Parallel.Prelude

{-
import Prelude hiding (map)
import Data.List.Stream
-}


import Graphics.UI.GLUT
import Mandstate
instance (Num a) => NFData (Color3 a)

maxIter = 600

parMapChunk cs strat f xs = map f xs `using` parListChunk cs strat
chunkSize = (width * height) `div` (4 * 20) 

compMandPoints :: Double -> Double -> Double -> Double -> [(Int, Int, Color3 Double)]
compMandPoints xmid ymid range cm = parMapChunk chunkSize rnf (\(x,y) -> (x, y, mandPoint 0 0.0 0.0 ((range*normx x) + xmid) ((range*normy y) + ymid) cm)) [ (x,y) | x <- [0  .. width-1], y <- [0 .. height-1] ] where
  normx i = (fromIntegral i - fromIntegral w2) / fromIntegral w2
  normy j = (fromIntegral j - fromIntegral h2) / fromIntegral h2

-- Number of iterations to escape
mandPoint :: Int -> Double -> Double -> Double -> Double -> Double -> Color3 Double
mandPoint n x y cx cy cm | n > maxIter = Color3 0 0 0
			 | (x*x + y*y) > 4.0	= colorMand x y n cm
			 | otherwise		= mandPoint (n+1) (x*x - y*y + cx) (2*x*y + cy) cx cy cm

-- Color a vertex
colorMand :: Double -> Double -> Int -> Double -> Color3 Double
colorMand x y n cm = Color3 r g b where
	m = 1.0 - (logBase 2 (logBase 2 $ sqrt (x*x + y*y))) + fromIntegral n
	r = 0.2 + 0.8 * cos (m * cm)
	g = 0.4 + 0.6 * cos ((m + 16.0) * cm)
	b = 0.5 + 0.5 * cos ((m + 32.0) * cm)