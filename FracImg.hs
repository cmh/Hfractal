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
pixel im cm pixarr k = do 
	p <- readArray pixarr k
	setPixel (k `divMod` h) (convColour $ colourMand p cm) im

imagAt ::  FilePath -> Mandstate -> IO ()
imagAt fp Mandstate{xmid=xm, ymid=ym, range=rng, colourmul=cm} = do
	pixarr <- newArray (0, w*h-1) 0.0 :: IO Pix
	im <- newImage (w, h)
	compPoints xm ym rng (Sz w h) pixarr
	mapM_ (pixel im cm pixarr) $ take ((h-1)*(w-1)) indicies
	savePngFile fp im

{-
imgrange :: Double-> Double-> Double-> Double-> Double-> Int-> [Char]-> IO ()
imgrange xm ym cm initrange scale num rootfp = mapM_ (\(fn, r) -> imagAt fn xm ym cm r) $ zip fns ranges where
	fns = map (\s -> rootfp ++ show (100000 + s) ++ ".png") [1..num]
	ranges = take num $ iterate (/scale) initrange
-}
