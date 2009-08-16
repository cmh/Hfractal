module FracImg
	where

import FracComp
import FracState

import Graphics.GD
import System.IO
import Graphics.Rendering.OpenGL

imgSize = (1000, 1000)
filepath = "." :: FilePath

convColour :: Color3 GLdouble -> Graphics.GD.Color
convColour (Color3 r g b) = rgb (f r) (f g) (f b) where
	f = (floor . (* 256))

pixel ::  Image -> Double -> Double -> IO ()
pixel im cm p = setPixel (x,y) (convColour $ mandColour p c) im

imagAt ::  FilePath -> Double -> Double -> Double -> Double -> IO ()
imagAt fp Opt@Option{size=Sz w h, ms=Mandstate xm ym rng cm} = do
	im <- newImage imgSize
	mapM_ (pixel im cm) $ (compPoints xm ym r)
	savePngFile fp im

{-
imgrange :: Double-> Double-> Double-> Double-> Double-> Int-> [Char]-> IO ()
imgrange xm ym cm initrange scale num rootfp = mapM_ (\(fn, r) -> imagAt fn xm ym cm r) $ zip fns ranges where
	fns = map (\s -> rootfp ++ show (100000 + s) ++ ".png") [1..num]
	ranges = take num $ iterate (/scale) initrange
-}
