import Graphics.UI.GLUT
import Data.IORef
import Data.Array.IO hiding (range)
import System.Console.GetOpt
import System.Environment (getArgs)

import Bindings
import FracState
import FracComp

inializeScreen opts@Options{size=Sz w h} = do
	(progname,_) <- getArgsAndInitialize
	initialDisplayMode $= [DoubleBuffered]
	lineSmooth  $= Enabled
	blendFunc   $= (SrcAlpha, OneMinusSrcAlpha)
	createWindow "HFractal"
	windowSize $= Size (fromIntegral (w-2)) (fromIntegral (h-1))
	--clearColor $= Color4 0 0 0 0
	matrixMode $= Projection
	ortho2D 0.0 (fromIntegral (w-1)) 0.0 (fromIntegral (h-1)) 
	matrixMode $= Modelview 0

setCallBacks opts@Options{size=s@(Sz w h), ms=state} = do
	--Create the state and pixel array
	ms <- newIORef state
	pixarr <- newArray (0, (w-1)*(h-1)) 0.0 :: IO Pix
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
	Mandstate{xmid=x, ymid=y, range=r, colourmul=cm} <- get ms
	compPoints x y r sz pixarr
	preservingMatrix $ do
		renderPrimitive Points $ mapM_ (displayPix cm h pixarr) $ take ((w-1)*(h-1)) indicies
	swapBuffers

displayPix :: Double -> Int -> IOUArray Int Double -> Int -> IO ()
displayPix cm h pixarr k = do
	let (i,j) = k `divMod` h
	dk <- readArray pixarr k
	color (colourMand dk cm)
	vertex $ Vertex2 (fromIntegral i) (fromIntegral j :: GLfloat)

-----------------------------------------
--Other Callbacks
-----------------------------------------

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

zeroState, state1 :: Mandstate
zeroState = Mandstate {xmid = 0.0, ymid = 0.0, range = 2.0, colourmul = 0.05}
state1    = Mandstate {xmid = 0.001643721971153, ymid = 0.822467633298876, range = 0.05, colourmul = 0.0625}
state = state1

defOpts = Options {size=Sz 300 300, ms=state}

--TODO: Tidy up the option parser with Data.Accessor(.Template)
options :: [OptDescr (Options -> Options)]
options = [ 
	Option ['w'] ["width"] (ReqArg (\x opt -> opt {size = sx (size opt) (r x)} ) "WIDTH") "Set width of rendering window", 
	Option ['h'] ["height"] (ReqArg (\y opt -> opt {size = sy (size opt) (r y)} ) "HEIGHT") "Set height of rendering window"] where 
	--Options ['x'] ["x-mid"] (NoArg (\x opt -> opt { ms {xmid = read x} }))
	sx (Sz x y) x' = (Sz x' y)
	sy (Sz x y) y' = (Sz x y')
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