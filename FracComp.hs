{-# LANGUAGE BangPatterns #-}
module FracComp
  where

import Graphics.UI.GLUT
import Data.Array.IO

import FracState

type Pix = IOUArray Int Double
data Sz = Sz Int Int deriving (Eq, Show)

-- Number of iterations to escape
mandPoint :: Int -> Double -> Double -> Double -> Double -> Double
mandPoint !n !x !y !cx !cy | n > maxIter       = 0.0
					       | (x2 + y2) > 4.0   = 1.0 - logBase 2 (0.5 * logBase 2 (x2 + y2)) + fromIntegral n
					       | otherwise		   = mandPoint (n+1) (x2 - y2 + cx) (2.0*x*y + cy) cx cy where
	x2 = x*x  --This CSE saves a few cycles
	y2 = y*y

compPoints :: Double -> Double -> Double -> Sz -> Pix -> IO ()
compPoints xm ym rng sz@(Sz width height) arr = mapM_ (\k -> writePix k arr) $ take ((width-1)*(height-1)) indicies where
	writePix k a = writeArray a k (mandPoint 0 0.0 0.0 cx cy) where
		(x, y) = k `divMod` height
		fi = fromIntegral
		cx = rng * (fi x - fi w2) / fi w2 + xm :: Double
		cy = rng * (fi y - fi h2) / fi h2 + ym :: Double
		(w2, h2) = (width `div` 2, height `div` 2)

-- Colour a vertex
colourMand :: Double -> Double -> Color3 GLdouble
colourMand 0.0 _ = fmap realToFrac $ Color3 0.0 0.0 0.0
colourMand m cm = fmap realToFrac $ Color3 r g b where
	r = 0.5 + 0.5 * cos (m * cm) 
	g = 0.5 + 0.5 * cos ((m + 16.0) * cm)
	b = 0.5 + 0.5 * cos ((m + 32.0) * cm)

-----------------------------------------
--QuickCheck Properties
-----------------------------------------
--TODO: Conjure up some more properties

prop_reflection :: Double -> Double -> Bool
prop_reflection x y = mandPoint 0 0.0 0.0 x y == mandPoint 0 0.0 0.0 x (-y)