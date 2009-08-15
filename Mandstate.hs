module Mandstate
  where

w2 = 400 :: Int
h2 = 400 :: Int
width = 2*w2
height = 2*h2

data Mandstate = Mandstate {
  xmid :: Double,
  ymid :: Double,
  range :: Double,
  colourmul :: Double } deriving Eq
