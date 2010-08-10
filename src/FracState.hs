{-# LANGUAGE TemplateHaskell #-}
module FracState
  where

import Data.Accessor
import Data.Accessor.Basic (T)
import Data.Accessor.Template

--These values alter the state in various ways
iteradd :: Int
rangemul, cmul :: Double
rangemul = 1.02
cmul     = 1.2
iteradd  = 100

data Mandstate = Mandstate {
  xmid_ :: Double,
  ymid_ :: Double,
  range_ :: Double,
  colourmul_ :: Double,
  colourfun_ :: Int,
  maxiter_ :: Int} deriving (Eq, Show) 
$( deriveAccessors ''Mandstate )

data Sz = Sz {
  wi_ :: Int,
  hi_ :: Int} deriving (Eq, Show)
$( deriveAccessors ''Sz )

data Options = Options
	{ size_:: Sz,
	  ms_  :: Mandstate   
	} deriving (Eq, Show)
$( deriveAccessors ''Options )
