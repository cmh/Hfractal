{-# LANGUAGE BangPatterns #-}
module PointComp
	where

{- Module for computing fractal functions at an indivdual point -}

-- Number of iterations to escape
mandPoint :: Double -> Double -> Int -> Double
mandPoint cx cy mi = go 0 0.0 0.0 where
	go !n !x !y | n > mi		   = 0.0
				| (x2 + y2) > 4.0 = 1.0 - logBase 2 (0.5 * logBase 2 (x2 + y2)) + fromIntegral n
				| otherwise		   = go (n+1) (x2 - y2 + cx) (2.0*x*y + cy) where
		!x2 = x*x  --This CSE saves a few cycles
		!y2 = y*y
