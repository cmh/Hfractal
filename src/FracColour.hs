{-# LANGUAGE BangPatterns #-}
module FracColour
	where

import Unsafe.Coerce
import Graphics.UI.GLUT hiding (blend)--(Color3, GLdouble)
import Data.Colour as C
import Data.Colour.SRGB
import Data.List as L
import Data.IntMap as IM
import Data.Maybe
import Data.Colour.Names
import Graphics.GD
import Data.Ord (comparing)

-- Create a pallette of colours with which is a map from escape iterations to colours
createPallete :: Int -> [(Int, Colour Double)] -> IntMap (Colour Double)
createPallete maxIter points | length points < 2 || maximum (L.map fst points) > maxIter
                                                 || minimum (L.map fst points) < 0 = error "Invalid pallete reference points"
							 | otherwise  = fromList (interpolate $ sortBy (\x y -> Data.Ord.comparing fst x y) points)

interpolate :: [(Int, Colour Double)] -> [(Int, Colour Double)]
interpolate [(_,_)] = []
interpolate ((n1,c1):(n2,c2):xs) = [(n1 + i, C.blend (fromIntegral i / delta) c1 c2) | i <- [0 .. (n2 - n1 - 1)]] ++ (interpolate ((n2,c2):xs)) where
	delta = fromIntegral (n2 - n1)

colourPoint' :: IntMap (Colour Double) -> Double -> Colour Double
colourPoint' colpal n = {-# SCC "blend" #-} C.blend (n-fromIntegral n1) (fromJust c1) (fromJust c2) where --Will it blend?
	n1 = {-# SCC "floor" #-} truncate n
	n2 = n1 + 1
	(c1, c2) = {-# SCC "lookups" #-} (IM.lookup n1 colpal, IM.lookup n2 colpal)

pallete = createPallete 5002 [(0,red), (200, white), (1600, yellow), (2000, blend 0.3 white orange),
                              (2500, darken 0.2 orange), (3200, blend 0.7 orange black), (4400, white), (5002, red)]

colourPointPal :: Double -> Double -> Color3 GLdouble
colourPointPal 0.0 _ = fmap doubleToGLdouble $ Color3 0.0 0.0 0.0
colourPointPal n _ = let c = toSRGB $ colourPoint' pallete n in
	{-# SCC "conversions" #-} fmap doubleToGLdouble $ Color3 (channelRed c) (channelGreen c) (channelBlue c)


doubleToGLdouble :: Double -> GLdouble
doubleToGLdouble = unsafeCoerce --This is neccesary as realToFrac is very slow (esp. in 7.x), the indirection will allow easier refactoring

colourPointFun = colourPointFun1

-- Colour a vertex based on the number of iterations it took to escape
colourPointFun1 :: Double -> Double -> Color3 GLdouble
colourPointFun1 0.0 _ = Color3 0.0 0.0 0.0
colourPointFun1 m cm = {-# SCC "conversions" #-} fmap doubleToGLdouble $ Color3 r g b where
	!r = {-# SCC "red" #-} 0.5 + 0.5 * cos (m * cm)
	!g = {-# SCC "green" #-} 0.5 + 0.5 * cos ((m + 16.0) * cm)
	!b = {-# SCC "blue" #-} 0.5 + 0.5 * cos ((m + 32.0) * cm)

colourPointFun2 :: Double -> Double -> Color3 GLdouble
colourPointFun2 0.0 _ = Color3 0.0 0.0 0.0
colourPointFun2 m cm = {-# SCC "conversions" #-} Color3 x x x where
	!x = {-# SCC "x" #-} doubleToGLdouble $ (min m 255.0) / 255.0

{-
--Store a colour specified by 3 doubles as an int
colToInt :: Double -> Double -> Double -> Int

-- Range based colouring, need access to the maxiter here (after refactor)
colourPointFun2 :: Double -> Double -> Color3 GLdouble
colourPointFun2 0.0 _ = fmap doubleToGLdouble $ Color3 0.0 0.0 0.0
colourPointFun2 m cm = fmap doubleToGLdouble $ Color3 r g b where
	frac = fromIntegral m / fromIntegral maxIter
-}

-- The function used to render. can be selected at runtime by the parameter cf
-- FIXME: this computes a mod for each point - not good.
{- colourPoint cf | cf `mod` 2 == 1 = colourPointFun
                  | cf `mod` 2 == 0 = colourPointPal
-}

colourPoint cf = colourPointFun

-- Colour a point outputting a GD compatiable value for png files
colourGD :: Double -> Double -> Graphics.GD.Color
colourGD 0.0 _ = rgb 0 0 0
colourGD n _ = let c = toSRGB24 $ colourPoint' pallete n in
	rgb (fromIntegral $ channelRed c) (fromIntegral $ channelGreen c) (fromIntegral $ channelBlue c)
