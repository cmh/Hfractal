import Mandcomp
import Data.Time.Clock
import System.Environment
import Control.Parallel.Strategies
import Graphics.Rendering.OpenGL

main = do
	putStrLn $ "Starting computation of mandlebrot set points"
	start <- getCurrentTime
	putStrLn $ "Last element is + " ++ ((show . last) $ ((compMandPoints 0.002 0.8 0.05 0.6) `using` rnf)) --last to force evaluatio
	end <- getCurrentTime
	putStrLn $ show (end `diffUTCTime` start) ++ " elapsed."
