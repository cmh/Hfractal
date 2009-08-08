import Mandcomp
import Mandstate

import Graphics.GD
import System.IO
import Graphics.Rendering.OpenGL

filepath = "imgs2/tmp" :: FilePath

convColour :: Color3 Double -> Graphics.GD.Color
convColour (Color3 r g b) = rgb (f r) (f g) (f b) where
	f = (floor . (* 256))

pixel ::  Image -> (Int, Int, Color3 Double) -> IO ()
pixel im (x, y, c) = setPixel (x,y) (convColour c) im

imagAt ::  FilePath -> Double -> Double -> Double -> Double -> IO ()
imagAt fp xm ym cm r = do
	im <- newImage (width, height) 
	mapM_ (pixel im) $ (compMandPoints xm ym r cm)
	savePngFile fp im

imgrange :: Double-> Double-> Double-> Double-> Double-> Int-> [Char]-> IO ()
imgrange xm ym cm initrange scale num rootfp = mapM_ (\(fn, r) -> imagAt fn xm ym cm r) $ zip fns ranges where
	fns = map (\s -> rootfp ++ show (100000 + s) ++ ".png") [1..num]
	ranges = take num $ iterate (/scale) initrange

main = do
	imgrange 0.001643721971153 0.822467633298876 0.0525 2.0 1.01 5000 filepath
