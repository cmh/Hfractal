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

inializeScreen opts@(Options (Sz w h) _) = do
	(progname,_) <- getArgsAndInitialize
	initialDisplayMode $= [DoubleBuffered]
	lineSmooth  $= Enabled
	blendFunc   $= (SrcAlpha, OneMinusSrcAlpha)
	createWindow "HFractal"
	windowSize $= Size (fromIntegral (w-2)) (fromIntegral (h-1))
	matrixMode $= Projection
	ortho2D 0.0 (fromIntegral (w-1)) 0.0 (fromIntegral (h-1)) 
	matrixMode $= Modelview 0

setCallBacks opts@(Options s@(Sz w h) state) = do
	--Create the state and pixel array
	ms <- newIORef state
	pixarr <- newArray (0, w*h-1) 0.0 :: IO Pix
	--Set the callbacks
	reshapeCallback $= Just (reshape opts)
	idleCallback $= Just idle
	keyboardMouseCallback $= Just (keyboardMouse ms)
	displayCallback $= display ms s pixarr

----------------------------------------
--Display Callback and related functions
----------------------------------------

display ms sz@(Sz w h) pixarr = do
	clear [ColorBuffer]
	loadIdentity
	(Mandstate x y r cm mi) <- get ms --Get state
	compPoints x y r mi sz pixarr     --Compute escape iterations for this state
	preservingMatrix $ do
		renderPrimitive Points $ displayPix sz cm pixarr 
	swapBuffers

--Takes the array with escape iterations (+ smoothing) and displays using
--the colour function defined in FracComp
displayPix :: Sz -> Double -> IOUArray Int Double -> IO ()
displayPix sz@(Sz width height) cm pixarr = go 0 0 where
	go !x !y | y == height = return ()
	         | x == width  = go 0 (y+1)	
			 | otherwise   = do
		dk <- readArray pixarr (x + y*width)
		color (colourMand dk cm)
		vertex $ Vertex2 (fromIntegral x) (fromIntegral y :: GLfloat)
		go (x+1) y

-----------------------------------------
--Other Callbacks
-----------------------------------------

--TODO: This is necessary currently
--want to only postRedisplay if something changes
idle ::  IO ()
idle = do
	postRedisplay Nothing

reshape opts s'@(Size w h) = do
	viewport $= (Position 0 0, s')
	--setCallBacks opts{size=Sz (fromIntegral w) (fromIntegral h)} --Reset the callbacks so that the pixarr is recreated
	postRedisplay Nothing

keyboardMouse ms key state _ _ = do
	keyboardAct ms key state

-----------------------------------------
--Option Parsing and Main loop
----------------------------------------

--Some defualt starting positions
zeroState, state1 :: Mandstate
zeroState = Mandstate 0.0 0.0 2.0 0.05 500
state1    = Mandstate 0.001643721971153 0.822467633298876 0.05 0.0625 500
state     = state1

defOpts = Options (Sz 400 400) state

--TODO: Tidy up the option parser with Data.Accessor(.Template)
options :: [OptDescr (Options -> Options)]
options = [ 
	Option ['w'] ["width"] (ReqArg (\x -> size^:wi^=(r x))  "WIDTH") "Set width of rendering window", 
	Option ['h'] ["height"] (ReqArg (\y -> size^:hi^=(r y)) "HEIGHT") "Set height of rendering window"] where 
	--Options ['x'] ["x-mid"] (NoArg (\x opt -> opt { ms {xmid = read x} }))
	r = read

getOpts :: [String] -> IO Options
getOpts argv = case getOpt Permute options argv of
	(o, [], []) -> return $ foldl (flip ($)) defOpts o
	(_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
	where header = "Usage: hfractal [OPTION...]"

main :: IO()
main = do
	opts <- getOpts =<< getArgs
	inializeScreen opts 
	setCallBacks opts
	mainLoop