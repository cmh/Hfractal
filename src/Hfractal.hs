{-# LANGUAGE BangPatterns #-}
import Graphics.UI.GLUT
import Data.IORef
import Data.Array.IO hiding (range)
import System.Console.GetOpt
import System.Environment (getArgs)
import Data.Accessor

import Bindings
import FracState
import FracComp
import FracColour

inializeScreen opts@(Options (Sz w h) _) = do
	(progname,_) <- getArgsAndInitialize
	initialDisplayMode $= [DoubleBuffered]
	lineSmooth  $= Enabled
	blendFunc   $= (SrcAlpha, OneMinusSrcAlpha)
	initialWindowSize $=  Size (fromIntegral w) (fromIntegral h)
	createWindow "HFractal"
	reshapeCallback $= Just (reshape opts)

setCallBacks opts@(Options s@(Sz w h) state) = do
	--Create the state and pixel array
	ms <- newIORef state 
	pix <- initPixArray w h
	--Deal with the window size
	matrixMode $= Projection
	ortho2D 0.0 (fromIntegral w) 0.0 (fromIntegral h)
	matrixMode $= Modelview 0
	--Set the callbacks
	keyboardMouseCallback $= Just (keyboardMouse ms s)
	displayCallback $= display ms s pix

--Display Callback and related functions

display :: (HasGetter g) => g Mandstate -> Sz -> PixArray -> IO ()
display ms sz@(Sz w h) pix@(PixArray pixarr _ _ _ _ _ _ ) = do
	clear [ColorBuffer]
	loadIdentity
	(Mandstate x y r cm cf mi) <- get ms --Get state
	movePoints x y r mi pix --Compute escape iterations for this state
	preservingMatrix $ do
		renderPrimitive Points $ displayPix sz cf cm pixarr
	swapBuffers

--Takes the array with escape iterations (+ smoothing) and displays using
--the colour function defined in FracComp
displayPix :: Sz -> Int -> Double -> IOUArray Int Double -> IO ()
displayPix sz@(Sz width height) cf cm pixarr = go 0 0 where
	go !x !y | y == height = return ()
	         | x == width  = go 0 (y+1)	
			 | otherwise   = do
		dk <- readArray pixarr (x + y*width)
		color (colourPointFun dk cm)
		vertex $ Vertex2 (fromIntegral x) (fromIntegral y :: GLfloat)
		go (x+1) y

-----------------------------------------
--Other Callbacks
-----------------------------------------

reshape ::  Options -> Size -> IO ()
reshape opts s'@(Size w h) = do
	viewport $= (Position 0 0, s')
	--setCallBacks opts{size_=Sz (fromIntegral w) (fromIntegral h)} --Reset the callbacks so that the pixarr is recreated
	postRedisplay Nothing

keyboardMouse ms s key state _ pos = do
	keyboardMouseAct ms s key state pos
	postRedisplay Nothing

-----------------------------------------
--Option Parsing and Main loop
----------------------------------------

--Some interesting starting positions
zeroState, state0, state1, state2 :: Mandstate
zeroState = Mandstate 0.0 0.0 2.0 0.05 1 500
state0	  = Mandstate (-0.14076572210832694) 0.8510989379408804 1.0 0.05 2 5000
state1    = Mandstate 0.001643721971153 0.822467633298876 0.05 0.0625 1 400
state2    = Mandstate 0.35473015182773904 9.541013313560959e-2 0.0002 0.0625 1 600
state     = state1

defOpts = Options (Sz 500 500) state

options :: [OptDescr (Options -> Options)]
options = [ 
	Option ['w'] ["width"] (ReqArg (\w -> size^:wi^=(read w))  "Window width") "Set width of rendering window", 
	Option ['h'] ["height"] (ReqArg (\h -> size^:hi^=(read h)) "Window height") "Set height of rendering window",
	Option ['x'] ["x-mid"] (ReqArg (\x -> ms^:xmid^=(read x)) "Real(z)") "Set the real part of the initial z (double)",
	Option ['y'] ["y-mid"] (ReqArg (\y -> ms^:ymid^=(read y)) "Imag(z)") "Set the imaginary part of the inital z (double)",
	Option ['i'] ["maxiter"] (ReqArg (\i -> ms^:maxiter^=(read i)) "Max iterations") "Maximum iterations until escape (int)",
	Option ['z'] ["zoom"] (ReqArg (\z -> ms^:range^=(read z)) "Zoom") "Level of zoom (double)"]

getOpts :: [String] -> IO Options
getOpts argv = case getOpt Permute options argv of
	(o, [], []) -> return $ foldl (flip ($)) defOpts o
	(_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
	where header = "Usage: hfractal [OPTIONS...] [+RTS -N{cores}]"

main :: IO()
main = do
	opts <- getOpts =<< getArgs
	inializeScreen opts 
	setCallBacks opts
	mainLoop