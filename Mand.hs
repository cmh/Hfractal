import Graphics.UI.GLUT
import Data.IORef
import Data.Array.IO hiding (range)

import Bindings
import Mandstate
import Mandcomp

zeroState, state1 :: Mandstate
zeroState = Mandstate {xmid = 0.0, ymid = 0.0, range = 2.0, colourmul = 0.05}
state1    = Mandstate {xmid = 0.001643721971153, ymid = 0.822467633298876, range = 0.05, colourmul = 0.0625}
state = state1

inializeScreen :: IO()
inializeScreen = do
	(progname,_) <- getArgsAndInitialize
	initialDisplayMode $= [DoubleBuffered]
	lineSmooth  $= Enabled
	blendFunc   $= (SrcAlpha, OneMinusSrcAlpha)
	createWindow "HFractal"
	windowSize $= Size (fromIntegral (width-1)) (fromIntegral (height-1))
	clearColor $= Color4 0 0 0 0
	matrixMode $= Projection
	ortho2D 0.0 (fromIntegral width) 0.0 (fromIntegral height) 
	matrixMode $= Modelview 0

setCallBacks :: IO()
setCallBacks = do
	--Create the state and pixel array
	ms <- newIORef state
	pixarr <- newArray (0, width*height-1) 0.0 :: IO Pix
	--Set the callbacks
	reshapeCallback $= Just reshape
	idleCallback $= Just idle
	keyboardMouseCallback $= Just (keyboardMouse ms)
	displayCallback $= display ms pixarr

main :: IO()
main = do
	inializeScreen
	setCallBacks
	mainLoop

idle ::  IO ()
idle = do
	postRedisplay Nothing

reshape :: Size -> IO ()
reshape s = do
	viewport $= (Position 0 0, s)
	postRedisplay Nothing

display :: (HasGetter g) => g Mandstate -> Pix -> IO ()
display ms pixarr = do
	clear [ColorBuffer]
	loadIdentity
	Mandstate{xmid=x, ymid=y, range=r, colourmul=cm} <- get ms
	compPoints x y r pixarr
	preservingMatrix $ do
		renderPrimitive Points $ mapM_ (displayPix cm pixarr) indicies
	swapBuffers

displayPix :: Double -> IOUArray Int Double -> (Int, Int) -> IO ()
displayPix cm pixarr (i,j) = do
	dk <- readArray pixarr (i + j*width)
	color (colorMand dk cm)
	vertex $ Vertex2 (fromIntegral i) (fromIntegral j :: GLfloat)

keyboardMouse ms key state _ _ = do
	keyboardAct ms key state