import FracComp
import FracState
import Data.Time.Clock
import Data.Array.IO

(testWidth, testHeight) = (400, 400)
mi = 500

main = do
	start <- getCurrentTime
	pixarr <- newArray (0, testWidth*testHeight-1) 0.0 :: IO Pix
	mapM_ (\r -> compPoints 0.002 0.8 r mi (Sz testWidth testHeight) pixarr) [0.1, 0.15 .. 1.0]
	end <- getCurrentTime
	putStrLn $ show (end `diffUTCTime` start) ++ " elapsed."
