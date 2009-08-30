module Bindings
  where

import Graphics.UI.GLUT
import Data.IORef
import System.Exit
import Data.Accessor
import Control.Concurrent

import FracState
import FracImg

keyboardMouseAct :: IORef Mandstate -> Sz -> Key -> KeyState -> Position -> IO ()
--Keyboard actions as described in README
keyboardMouseAct ms _ (SpecialKey KeyLeft) Down _ = do
  ms' <- readIORef ms
  modifyIORef ms (xmid ^: ((+) ( -0.05 * ms'^.range)))
keyboardMouseAct ms _ (SpecialKey KeyRight) Down _ = do
  ms' <- readIORef ms
  modifyIORef ms (xmid ^: ((+) ( 0.05 * ms'^.range)))
keyboardMouseAct ms _ (SpecialKey KeyUp) Down _ = do
  ms' <- readIORef ms
  modifyIORef ms (ymid ^: ((+) ( 0.05 * ms'^.range)))
keyboardMouseAct ms _ (SpecialKey KeyDown) Down _ = do
  ms' <- readIORef ms
  modifyIORef ms (ymid ^: ((+) ( -0.05 * ms'^.range)))
keyboardMouseAct ms _ (Char '+') Down _ = do
  modifyIORef ms (range ^: (/rangemul))
keyboardMouseAct ms _ (Char '-') Down _ = do
  modifyIORef ms (range ^: (*rangemul))
keyboardMouseAct ms _ (Char 'a') Down _ = do
  modifyIORef ms (colourmul ^: (*cmul)) 
keyboardMouseAct ms _ (Char 's') Down _ = do
  modifyIORef ms (colourmul ^: (/cmul)) 
keyboardMouseAct ms _ (Char '<') Down _ = do
  modifyIORef ms (maxiter ^: ((-) iteradd))
keyboardMouseAct ms _ (Char '>') Down _ = do
  modifyIORef ms (maxiter ^: (+ iteradd))
keyboardMouseAct ms _ (Char 'p') Down _ = do
  ms' <- readIORef ms
  putStrLn "Creating frac.png"
  forkIO (imagAt "frac.png" ms' >> putStrLn "Finished Image") >> return ()
  --TODO: Allow user-namable output images, make this concurrent
keyboardMouseAct ms _ (Char 'o') Down _ = do
  ms' <- readIORef ms
  print ms'
--Mouse actions
keyboardMouseAct ms _ (MouseButton WheelUp) Down _ = do
  modifyIORef ms (range ^: (/ (rangemul*1.05)))
keyboardMouseAct ms _ (MouseButton WheelDown) Down _ = do
  modifyIORef ms (range ^: (* (rangemul*1.05)))
keyboardMouseAct ms (Sz w h) (MouseButton RightButton) Down (Position x y) = do
  ms' <- readIORef ms
  modifyIORef ms ((xmid ^: ((+) ((x `mp` w) * ms'^.range ))) .
				  (ymid ^: ((+) (-(y `mp` h) * ms'^.range ))) .
				  (range ^: (/rangemul)))
keyboardMouseAct ms (Sz w h) (MouseButton LeftButton) Down (Position x y) = do
  ms' <- readIORef ms
  modifyIORef ms ((xmid ^: ((+) ((x `mp` w) * ms'^.range ))) .
				  (ymid ^: ((+) (-(y `mp` h) * ms'^.range ))))
--Exit and default
keyboardMouseAct _ _ (Char '\27') _ _ = exitWith ExitSuccess
keyboardMouseAct _ _ _ _ _ = return ()

--TODO, have a separate mouse and keyboard functions and then a wrapper with cases on the type of Key

mp :: GLint -> Int -> Double
mp x w = (fromIntegral x - fromIntegral (w `div` 2)) / fromIntegral w