{-# LANGUAGE BangPatterns #-}
module FracComp
  where

import Graphics.UI.GLUT
import Data.Array.IO
import Control.Concurrent
import Control.Exception
import System.IO.Unsafe

import FracState
import PointComp

type Pix = IOUArray Int Double
type RowVals = IOUArray Int Double
type ColVals = IOUArray Int Double

--Could keep maximum escape value in here for colouring purposes
data PixArray = PixArray {
	pixels :: Pix,
	rows :: RowVals,
	cols :: ColVals,
	pixelsTemp :: Pix,
	rowsTemp :: RowVals,
	colsTemp :: ColVals,
	siz :: Sz}

initPixArray :: Int -> Int -> IO PixArray 
initPixArray width height = do
	pixels <- newArray (0, width * height - 1) 0.0 :: IO Pix
	rows <- newArray (0, height) 0.0 :: IO RowVals
	cols <- newArray (0, width) 0.0 :: IO ColVals
	pixelsTemp <- newArray (0, width * height - 1) 0.0 :: IO Pix
	rowsTemp <- newArray (0, height) 0.0 :: IO RowVals
	colsTemp <- newArray (0, width) 0.0 :: IO ColVals
	return (PixArray pixels rows cols pixelsTemp rowsTemp colsTemp (Sz width height))

copyArr :: Int -> IOUArray Int Double -> IOUArray Int Double -> IO ()
copyArr end orig dest = go 0 where
	go !x | x == end = return ()
	      | otherwise = do
		t <- readArray orig x
		writeArray dest x t
		go (x + 1)

copyPix :: Sz -> Pix -> Pix -> IO () 
copyPix sz@(Sz width height) = copyArr (width * height)

copyRow :: Sz -> RowVals -> RowVals -> IO ()
copyRow sz@(Sz width height) = copyArr height

copyCol :: Sz -> RowVals -> RowVals -> IO ()
copyCol sz@(Sz width height) = copyArr width

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


mandPointSampled !x !y !xrng !yrng !mi ss = if (any (== 0.0) points) then 0.0 else average points where
	points = [ mandPoint (x + dx) (y + dy) mi | 
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

--Fill a Pix array with an initial computation centered at xm ym at zoom range
compPoints :: Double -> Double -> Double -> Int -> PixArray -> IO ()
compPoints xm ym rng mi pa@(PixArray arr rows cols _ _ _ sz@(Sz width height)) = do
	go 0
	waitForChildren where
		go !y | y == height = return () 
			  | otherwise = forkChild (goRow 0 y) >> go (y+1)
		goRow !x y  | x == width  = return () :: IO ()
					| otherwise = do	
			writeArray arr k (mandPoint cx cy mi)
			writeArray rows x cx  --This is horribly inefficient, but quick fix
			writeArray cols y cy
			goRow (x+1) y where
				k = x + y*width
				fi = fromIntegral
				cx = rng * (fi x - fi w2) / fi width + xm :: Double
				cy = rng * (fi y - fi h2) / fi height + ym :: Double
				(w2, h2) = (width `div` 2, height `div` 2) 

--Moving from a previously computed pixel array to a new one. Hopefull reusing any applicable
--previous values.
mp :: Double -> Double -> Double -> Int -> PixArray -> IO ()
mp xm ym rng mi pa@(PixArray pix rows cols pixt rowst colst sz@(Sz width height)) = do
	go 0 0
	waitForChildren where
		(w2, h2) = (width `div` 2, height `div` 2) 
		fi = fromIntegral
		step = rng / fi (height * 2) :: Double
		go !rowIndex !y = do
			if (y >= height) 
				then do return () :: IO ()
				else do rc <- (readArray rows rowIndex)
					let cy = {-# SCC "cy" #-} rng * (fi y - fi h2) / fi height + ym :: Double
					if (rowIndex >= height) 
						then do writeArray rowst y cy
							forkChild (goRow 0 y) >> go rowIndex (y+1)
						else if (rc < (cy - step)) 
							then do go (rowIndex + 1) y 
							else if (rc > (cy - step) && rc < (cy + step)) 
								then do writeArray rowst y rc
									--putStrLn "Cache"
									forkChild (goRowCache rowIndex rc cy 0 0 y) >> go rowIndex (y+1)
								else do writeArray rowst y cy
									--putStrLn "NoCache"
									forkChild (goRow 0 y) >> go rowIndex (y+1)
		goRowCache ri rc cy !colIndex !x y = do
			if (x >= width) 
				then do return () :: IO ()
				else do cc <- (readArray cols colIndex)
					let cx = rng * (fi x - fi w2) / fi width + xm :: Double
					let k = x + y*width	
					if (colIndex >= width) 
						then do writeArray colst x cx
							writeArray pixt k (mandPoint cx cy mi)
							goRowCache ri rc cy colIndex (x+1) y 
						else if (cc < (cx - step)) 
							then do goRowCache ri rc cy (colIndex + 1) x y 
							else do
								if (cc > (cx - step) && cc < (cx + step)) 
									then do writeArray colst x cc    --This is a bit of a fuck up
										oldVal <- readArray pix (colIndex + ri * width)
										writeArray pixt k oldVal
										goRowCache ri rc cy colIndex (x+1) y 
									else do writeArray colst x cx
										writeArray pixt k (mandPoint cx cy mi)
										goRowCache ri rc cy colIndex (x+1) y 
		goRow !x y  | x >= width = return () :: IO ()
					| otherwise = do	
			writeArray pixt k (mandPoint cx cy mi)
			writeArray colst x cx
			goRow (x+1) y where
				k = x + y*width
				cx = rng * (fi x - fi w2) / fi width + xm :: Double
				cy = rng * (fi y - fi h2) / fi height + ym :: Double

{-
chunkSize = 20

mp' beg xm ym rng mi pa@(PixArray pix rows cols pixt rowst colst sz@(Sz width height)) = do 
	putStrLn (show beg)
	let end = beg + chunkSize
	if (end >= height)
		then do mp beg height xm ym rng mi pa
			waitForChildren
		else do 
			forkChild (mp beg end xm ym rng mi pa) >> mp' end xm ym rng mi pa
			waitForChildren
-}

movePoints :: Double -> Double -> Double -> Int -> PixArray -> IO ()
movePoints xm ym rng mi pa@(PixArray pix rows cols pixt rowst colst sz@(Sz width height)) = do
	mp xm ym rng mi pa
	copyPix sz pixt pix   --Flip the arrays
	copyRow sz rowst rows
	copyCol sz colst cols

