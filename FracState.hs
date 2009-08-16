module FracState
  where

maxIter, maxWidth, maxHeight :: Int
maxIter = 500
(maxWidth, maxHeight) = (1200,1200)

--Keep the indicies as a global list so they aren't recopmuted every rendering
indicies :: [Int]
indicies = [0 .. (maxWidth-1)*(maxHeight-1)]

rangemul, cmul :: Double
rangemul = 1.02
cmul     = 1.3

data Mandstate = Mandstate {
  xmid :: Double,
  ymid :: Double,
  range :: Double,
  colourmul :: Double } deriving (Eq, Show)
