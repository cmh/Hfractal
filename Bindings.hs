module Bindings
  where

import Graphics.UI.GLUT
import Data.IORef
import System.Exit
import Data.Accessor

import FracState
import FracImg

keyboardAct :: IORef Mandstate -> Key -> KeyState -> IO ()
keyboardAct ms (SpecialKey KeyLeft) Down = do
  ms' <- readIORef ms
  modifyIORef ms (xmid ^: ((-) ( 0.05 * ms'^.range)))
keyboardAct ms (SpecialKey KeyRight) Down = do
  ms' <- readIORef ms
  modifyIORef ms (xmid ^: ((+) ( 0.05 * ms'^.range)))
keyboardAct ms (SpecialKey KeyUp) Down = do
  ms' <- readIORef ms
  modifyIORef ms (ymid ^: ((+) ( 0.05 * ms'^.range)))
keyboardAct ms (SpecialKey KeyDown) Down = do
  ms' <- readIORef ms
  modifyIORef ms (ymid ^: ((-) ( 0.05 * ms'^.range)))
keyboardAct ms (Char '+') Down = do
  modifyIORef ms (range ^: (/rangemul))
keyboardAct ms (Char '-') Down = do
  modifyIORef ms (range ^: (*rangemul))
keyboardAct ms (Char 'a') Down = do
  modifyIORef ms (colourmul ^: (*cmul)) 
keyboardAct ms (Char 's') Down = do
  modifyIORef ms (colourmul ^: (/cmul)) 
keyboardAct ms (Char '<') Down = do
  modifyIORef ms (maxIter ^: ((-) iteradd))
keyboardAct ms (Char '>') Down = do
  modifyIORef ms (maxIter ^: (+ iteradd))
keyboardAct ms (Char 'p') Down = do
  ms' <- readIORef ms
  imagAt "frac.png" ms' --TODO: Allow user-namable output images
keyboardAct ms (Char 'o') Down = do
  ms' <- readIORef ms
  print ms'
keyboardAct _ (Char '\27') _ = exitWith ExitSuccess
keyboardAct _ _ _ = return ()