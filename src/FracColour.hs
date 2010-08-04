module FracColour 
	where

import Graphics.UI.GLUT hiding (blend)--(Color3, GLdouble)
import Data.Colour as C
import Data.Colour.SRGB
import Data.List as L
import Data.IntMap as IM
import Data.Maybe
import Data.Colour.Names
import Graphics.GD

-- Create a pallette of colours with which is a map from escape iterations to colours
createPallete :: Int -> [(Int, Colour Double)] -> IntMap (Colour Double)
createPallete maxIter points | length points < 2 || maximum (L.map fst points) > maxIter 
                                                 || minimum (L.map fst points) < 0 = error "Invalid pallete reference points"
							 | otherwise  = fromList (interpolate $ sortBy (\x y -> fst x `compare` fst y) points)

interpolate :: [(Int, Colour Double)] -> [(Int, Colour Double)]
interpolate [(_,_)] = []
interpolate ((n1,c1):(n2,c2):xs) = [(n1 + i, C.blend (fromIntegral i / delta) c1 c2) | i <- [0 .. (n2 - n1 - 1)]] ++ (interpolate ((n2,c2):xs)) where
	delta = fromIntegral (n2 - n1)

colourPoint' :: IntMap (Colour Double) -> Double -> Colour Double
colourPoint' colpal n = {-# SCC "blend" #-} C.blend (n-fromIntegral n1) (fromJust c1) (fromJust c2) where --Will it blend?
	n1 = {-# SCC "floor" #-} truncate n
	n2 = n1 + 1
	(c1, c2) = {-# SCC "lookups" #-} (IM.lookup n1 colpal, IM.lookup n2 colpal)

pallete = createPallete 5002 [(0,red), (1200, white), (1600, yellow), (2000, blend 0.3 white orange), (2500,darken 0.2 orange), (3200, blend 0.7 orange black), (4400, white), (5002, red)]

{-
colourPoint :: Double -> Double -> Color3 GLdouble
colourPoint 0.0 _ = fmap realToFrac $ Color3 0.0 0.0 0.0
colourPoint n _ = let c = toSRGB $ colourPoint' pallete n in
	{-# SCC "conversions" #-} fmap realToFrac $ Color3 (channelRed c) (channelGreen c) (channelBlue c)
-}

colourGD :: Double -> Double -> Graphics.GD.Color
colourGD 0.0 _ = rgb 0 0 0
colourGD n _ = let c = toSRGB24 $ colourPoint' pallete n in
	rgb (fromIntegral $ channelRed c) (fromIntegral $ channelGreen c) (fromIntegral $ channelBlue c)

-- Colour a vertex based on the number of iterations it took to escape
colourPoint :: Double -> Double -> Color3 GLdouble
colourPoint 0.0 _ = fmap realToFrac $ Color3 0.0 0.0 0.0
colourPoint m cm = fmap realToFrac $ Color3 r g b where
	r = 0.5 + 0.5 * cos (m * cm) 
	g = 0.5 + 0.5 * cos ((m + 16.0) * cm)
	b = 0.5 + 0.5 * cos ((m + 32.0) * cm)
