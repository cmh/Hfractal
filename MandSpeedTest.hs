import Mandcomp
import Data.Time.Clock
import System.Environment
import Control.Parallel.Strategies
import Graphics.Rendering.OpenGL
import Data.Array.IO
import Mandstate

main = do
	start <- getCurrentTime
	pixarr <- newArray (0, width*height-1) 0.0 :: IO Pix
	(compPoints 0.002 0.8 0.05 pixarr) 
	(compPoints 0.202 2.8 1.05 pixarr)
	(compPoints 0.002 1.1 0.001 pixarr)
	(compPoints 0.002 1.11 0.101 pixarr)
	(compPoints 0.002 1.12 0.201 pixarr)
	end <- getCurrentTime
	putStrLn $ show (end `diffUTCTime` start) ++ " elapsed."
