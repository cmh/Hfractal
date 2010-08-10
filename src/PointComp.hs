{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module PointComp
	where
	
import Foreign
import Foreign.C

{- Module for computing fractal functions at an indivdual point -}

-- Number of iterations to escape
mandPoint :: Double -> Double -> Int -> Double
mandPoint = mandPoint2

mandPoint1 :: Double -> Double -> Int -> Double
mandPoint1 cx cy mi = go 0 0.0 0.0 where
	go !n !x !y | n > mi		   = 0.0
				| (x2 + y2) > 4.0 = 1.0 - logBase 2 (0.5 * logBase 2 (x2 + y2)) + fromIntegral n
				| otherwise		   = go (n+1) (x2 - y2 + cx) (2.0*x*y + cy) where
		!x2 = x*x  --This CSE saves a few cycles
		!y2 = y*y

foreign import ccall "pointcomp.h mandpoint" c_mandpoint :: CDouble -> CDouble -> CInt -> CDouble
mandPoint2 :: Double ->	Double -> Int -> Double
mandPoint2 cx cy mi = realToFrac (c_mandpoint (realToFrac cx) (realToFrac cy) (fromIntegral mi))
