{-# LANGUAGE BangPatterns #-}
module FracComp
  where

import Graphics.UI.GLUT
import Data.Array.IO
import Control.Concurrent
import Control.Exception
import System.IO.Unsafe

import FracState

type Pix = IOUArray Int Double

-- Number of iterations to escape the mandelbrot set
mandPoint :: Int -> Double -> Double -> Double -> Double -> Int -> Double
mandPoint !n !x !y cx cy mi | n > mi		   = 0.0
			    | (x2 + y2) > 4.0      = 1.0 - logBase 2 (0.5 * logBase 2 (x2 + y2)) + fromIntegral n
			    | otherwise		   = mandPoint (n+1) (x2 - y2 + cx) (2.0*x*y + cy) cx cy mi where
	!x2 = x*x  --This CSE saves a few cycles
	!y2 = y*y

{-
-- Number of iterations to escape the burning ship fractal
burnPoint :: Int -> Double -> Double -> Double -> Double -> Int -> Double
burnPoint !n !x !y cx cy mi | n > mi		   = 0.0
					        | (x2 + y2) > 4.0  = 1.0 - logBase 2 (0.5 * logBase 2 (x2 + y2)) + fromIntegral n
					        | otherwise		   = burnPoint (n+1) (x2 - y2 + cx) (-2.0 * abs (x*y) + cy) cx cy mi where
	!x2 = x*x 
	!y2 = y*y
-}

children :: MVar [MVar ()]
children = unsafePerformIO (newMVar [])

waitForChildren :: IO ()
waitForChildren = do
	cs <- takeMVar children
	case cs of
		[]   -> do
			putMVar children []
			return ()
		m:ms -> do
			putMVar children ms
			takeMVar m
			waitForChildren

forkChild :: IO () -> IO ThreadId
forkChild io = do
	mvar <- newEmptyMVar
	childs <- takeMVar children
	putMVar children (mvar:childs)
	forkIO (io `finally` putMVar mvar ())


mandPointSampled !x !y !xrng !yrng !mi ss = average points where
	points = [ mandPoint 0 0.0 0.0 (x + dx) (y + dy) mi | 
			   dx <- ((take ss) . iterate (+xrng)) 0.0, 
			   dy <- ((take ss) . iterate (+yrng)) 0.0]
	average xs = sum xs / (fromIntegral . length) xs


--This gives an image in a sligtly different position than the unsampled function
--But the code is easier this way
compPointsSampled :: Double -> Double -> Double -> Int -> Sz -> Pix -> Int -> IO ()
compPointsSampled xm ym rng mi sz@(Sz width height) arr ss = do
	go 0
	waitForChildren where
		go !y | y == height = return () 
	              | otherwise = forkChild (goRow 0 y) >> go (y+1)
		goRow !x y  | x == width  = return () :: IO ()
			    | otherwise = do	
			writeArray arr k (mandPointSampled cx cy xrng yrng mi ss)
			goRow (x+1) y where
				(xrng, yrng) = (rng / fi (ss * width), rng / fi (ss * height))
				k = x + y*width
				fi = fromIntegral
				cx = rng * (fi x - fi w2) / fi width + xm :: Double
				cy = rng * (fi y - fi h2) / fi height + ym :: Double
				(w2, h2) = (width `div` 2, height `div` 2) 

compPoints :: Double -> Double -> Double -> Int -> Sz -> Pix -> IO ()
compPoints xm ym rng mi sz@(Sz width height) arr = do
	go 0
	waitForChildren where
		go !y | y == height = return () 
		      | otherwise = forkChild (goRow 0 y) >> go (y+1)
		goRow !x y  | x == width  = return () :: IO ()
			    | otherwise = do	
			writeArray arr k (mandPoint 0 0.0 0.0 cx cy mi)
			goRow (x+1) y where
				k = x + y*width
				fi = fromIntegral
				cx = rng * (fi x - fi w2) / fi width + xm :: Double
				cy = rng * (fi y - fi h2) / fi height + ym :: Double
				(w2, h2) = (width `div` 2, height `div` 2) 

-----------------------------------------
--QuickCheck Properties
-----------------------------------------
--TODO: Conjure up some more properties

prop_reflection :: Double -> Double -> Bool
prop_reflection x y = mandPoint 0 0.0 0.0 x y 500 == mandPoint 0 0.0 0.0 x (-y) 500
