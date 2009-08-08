module Mandstate
  where

w2 = 300 :: Int
h2 = 300 :: Int
width = 2*w2
height = 2*h2

data Mandstate = Mandstate {
  xmid :: Double,
  ymid :: Double,
  range :: Double,
  colourmul :: Double } deriving Eq
