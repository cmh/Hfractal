import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Exit
import Data.IORef

import Bindings
import Mandstate
import Mandcomp

zeroState = Mandstate {xmid = 0.0, ymid = 0.0, range = 2.0, colourmul = 0.05}
state1    = Mandstate {xmid=0.001643721971153, ymid=0.822467633298876, range=0.05, colourmul=0.0625}

inializeScreen :: IO()
inializeScreen = do
	(progname,_) <- getArgsAndInitialize
	initialDisplayMode $= [DoubleBuffered]
	createWindow "Mandlebrot Viewer"
	windowSize $= Size (fromIntegral width) (fromIntegral height)
	clearColor $= Color4 0 0 0 0
	matrixMode $= Projection
	ortho2D 0.0 (fromIntegral width) 0.0 (fromIntegral height) 
	matrixMode $= Modelview 0

setCallBacks :: IO()
setCallBacks = do
	ms <- newIORef state1
	angle <- newIORef (0.0 :: GLfloat)
	reshapeCallback $= Just reshape
	idleCallback $= Just idle
	keyboardMouseCallback $= Just (keyboardMouse ms)
	displayCallback $= (display ms)

main :: IO()
main = do
	inializeScreen
	setCallBacks
	mainLoop

idle = do
	postRedisplay Nothing

reshape s@(Size w h) = do
	viewport $= (Position 0 0, s)
	postRedisplay Nothing

display ms = do
	clear [ColorBuffer]
	loadIdentity
	Mandstate{xmid=x, ymid=y, range=r, colourmul=cm} <- get ms
	preservingMatrix $ do
		renderPrimitive Points $ mapM_ displayVert $ compMandPoints x y r cm
	swapBuffers

displayVert (x,y,c) = do 
	color c
	vertex $ Vertex2 (fromIntegral x) ((fromIntegral y) :: GLfloat)

keyboardMouse ms key state modifiers position = do
	keyboardAct ms key state