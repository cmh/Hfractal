{-# LANGUAGE BangPatterns #-}
module FracComp
  where

import Graphics.UI.GLUT
import Data.Array.IO

import FracState

type Pix = IOUArray Int Double

-- Number of iterations to escape
mandPoint :: Int -> Double -> Double -> Double -> Double -> Int -> Double
mandPoint !n !x !y cx cy mi | n > mi		   = 0.0
					        | (x2 + y2) > 4.0  = 1.0 - logBase 2 (0.5 * logBase 2 (x2 + y2)) + fromIntegral n
					        | otherwise		   = mandPoint (n+1) (x2 - y2 + cx) (2.0*x*y + cy) cx cy mi where
	x2 = x*x  --This CSE saves a few cycles
	y2 = y*y

compPoints :: Double -> Double -> Double -> Int -> Sz -> Pix -> IO ()
compPoints xm ym rng mi sz@(Sz width height) arr = go 0 0 where
	go !x !y | y == height  = return () 
			 | x == width   = go 0 (y+1)
			 | otherwise = do 
		writeArray arr k (mandPoint 0 0.0 0.0 cx cy mi)
		go (x+1) y where
			k = x + y*width
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
prop_reflection x y = mandPoint 0 0.0 0.0 x y 500 == mandPoint 0 0.0 0.0 x (-y) 500