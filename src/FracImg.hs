{-# LANGUAGE BangPatterns #-}
module FracImg
	where

import FracComp
import FracState
import FracColour

import Graphics.GD
import Data.Array.IO hiding (range)
import System.IO
import Graphics.Rendering.OpenGL

supSamp = 1       --Use subpixel sampling
imgMaxIter = 35 --High iteration for the output image
w = 500		  --High resolution for the output image
h = 500
filepath = "." :: FilePath

--Convert a GL Colour datatype to a GD Colour datatype
convColour :: Color3 GLdouble -> Graphics.GD.Color
convColour (Color3 r g b) = rgb (f r) (f g) (f b) where
	f = (floor . (* 256))

pixelWrite im (Sz w h) cf cm pixarr = go 0 0 where
	go !x !y | y == h = return ()
			 | x == w = go 0 (y+1)
			 | otherwise = do
		p <- readArray pixarr (x + y*w)
		(antiAliased $ setPixel (x,y)) (convColour $ colourPoint cf p cm) im
		go (x+1) y

imagAt ::  FilePath -> Mandstate -> IO ()
imagAt fp (Mandstate xm ym rng cm cf mi) = do
	pixarr <- newArray (0, w*h-1) 0.0 :: IO Pix
	im <- newImage (w, h)
	compPointsSampled xm ym rng imgMaxIter (Sz w h) pixarr supSamp
	pixelWrite im (Sz w h) cf cm pixarr 
	savePngFile fp im

{-
imgrange :: Double-> Double-> Double-> Double-> Double-> Int-> [Char]-> IO ()
imgrange xm ym cm initrange scale num rootfp = mapM_ (\(fn, r) -> imagAt fn xm ym cm r) $ zip fns ranges where
	fns = map (\s -> rootfp ++ show (100000 + s) ++ ".png") [1..num]
	ranges = take num $ iterate (/scale) initrange
-}
