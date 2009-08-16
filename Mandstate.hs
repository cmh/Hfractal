module Mandstate
  where

maxIter,w2,h2,width,height :: Int
w2 = 250 
h2 = 250 
width = 2*w2 
height = 2*h2 
maxIter = 500

rangemul, cmul :: Double
rangemul = 1.02
cmul     = 1.3

indicies :: [(Int,Int)]
indicies = [(i,j) | i <- [1..(width-1)], j <- [1..(height-1)]] :: [(Int,Int)]

data Mandstate = Mandstate {
  xmid :: Double,
  ymid :: Double,
  range :: Double,
  colourmul :: Double } deriving Eq
