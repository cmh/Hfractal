{-# LANGUAGE BangPatterns #-}
module PointComp
	where
{- Module for computing fractal functions at an indivdual point -}

{- Want an arbitrary class which defines computation of a fractal at a point -}
{-class FracComp where-}
	{-compAt :: Double -> Double -> Int -> Double	-}

-- Number of iterations to escape
mandPoint :: Double -> Double -> Int -> Double
mandPoint cx cy mi | inCardiod cx cy = 0.0
				   | otherwise       = go 0 0.0 0.0 where
	inCardiod cx cy = q * (q + (cx - 0.25)) < 0.25 * cy * cy where
		q = (cx - 0.25) * (cx - 0.25) + cy*cy
	go !n !x !y | x2 + y2 > 4.0    = 1.0 - logBase 2 (0.5 * logBase 2 (x2 + y2)) + fromIntegral n
				| n > mi		   = 0.0
				| otherwise		   = go (n+1) (x2 - y2 + cx) (2.0*x*y + cy) where
		!x2 = x*x 
		!y2 = y*y  --This CSE saves a few cycles

-----------------------------------------
--QuickCheck Properties
-----------------------------------------

prop_reflection :: Double -> Double -> Bool
prop_reflection x y = mandPoint x y 500 == mandPoint x (-y) 500
