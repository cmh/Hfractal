module Bindings
  where

import Graphics.UI.GLUT
import Data.IORef
import Mandstate
import System.Exit

rangemul = 1.02
cmul = 1.3

keyboardAct :: IORef Mandstate -> Key -> KeyState -> IO ()
keyboardAct ms (SpecialKey KeyLeft) Down = do
  modifyIORef ms (\m@Mandstate{xmid=x,range=r} -> m{xmid=x - 0.1*r})
keyboardAct ms (SpecialKey KeyRight) Down = do
  modifyIORef ms (\m@Mandstate{xmid=x,range=r} -> m{xmid=x + 0.1*r})
keyboardAct ms (SpecialKey KeyUp) Down = do
  modifyIORef ms (\m@Mandstate{ymid=y,range=r} -> m{ymid=y + 0.1*r})
keyboardAct ms (SpecialKey KeyDown) Down = do
  modifyIORef ms (\m@Mandstate{ymid=y,range=r} -> m{ymid=y - 0.1*r})
keyboardAct ms (Char '+') Down = do
  modifyIORef ms (\m@Mandstate{range=r} -> m{range=r/rangemul})
keyboardAct ms (Char '-') Down = do
  modifyIORef ms (\m@Mandstate{range=r} -> m{range=r*rangemul})
keyboardAct ms (Char 'a') Down = do
  modifyIORef ms (\m@Mandstate{colourmul=cm} -> m{colourmul=cm*cmul})
keyboardAct ms (Char 's') Down = do
  modifyIORef ms (\m@Mandstate{colourmul=cm} -> m{colourmul=cm/cmul})
keyboardAct _ (Char '\27') _ = exitWith ExitSuccess
keyboardAct _ _ _ = return ()