{-# LANGUAGE BangPatterns #-}
module FracImg
	where

import FracComp
import FracState

import Graphics.GD
import Data.Array.IO hiding (range)
import System.IO
import Graphics.Rendering.OpenGL

w = 1600
h = 1600
filepath = "." :: FilePath

convColour :: Color3 GLdouble -> Graphics.GD.Color
convColour (Color3 r g b) = rgb (f r) (f g) (f b) where
	f = (floor . (* 256))

--pixel ::  Image -> Double -> (Int, Double) -> IO ()
pixelWrite im (Sz w h) cm pixarr = go 0 0 where
	go !x !y | y == h = return ()
			 | x == w = go 0 (y+1)
			 | otherwise = do
		p <- readArray pixarr (x + y*w)
		setPixel (x,y) (convColour $ colourMand p cm) im
		go (x+1) y

imagAt ::  FilePath -> Mandstate -> IO ()
imagAt fp Mandstate{xmid=xm, ymid=ym, range=rng, colourmul=cm} = do
	pixarr <- newArray (0, w*h-1) 0.0 :: IO Pix
	im <- newImage (w, h)
	compPoints xm ym rng (Sz w h) pixarr
	pixelWrite im (Sz w h) cm pixarr 
	savePngFile fp im

{-
imgrange :: Double-> Double-> Double-> Double-> Double-> Int-> [Char]-> IO ()
imgrange xm ym cm initrange scale num rootfp = mapM_ (\(fn, r) -> imagAt fn xm ym cm r) $ zip fns ranges where
	fns = map (\s -> rootfp ++ show (100000 + s) ++ ".png") [1..num]
	ranges = take num $ iterate (/scale) initrange
-}
