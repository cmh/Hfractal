module FracSpeedTest where

import FracComp
import FracState
import Data.Time.Clock
import Data.Array.IO

(testWidth, testHeight) = (800, 800)
mi = 800

main = do
	start <- getCurrentTime
	pixarr <- initPixArray testWidth testHeight
	mapM_ (\r -> compPoints 0.002 0.8 r mi pixarr) [0.1, 0.11 .. 0.3]
	end <- getCurrentTime
	putStrLn $ show (end `diffUTCTime` start) ++ " elapsed."
	start2 <- getCurrentTime
	pixarr2 <- initPixArray testWidth testHeight
	mapM_ (\r -> movePoints 0.002 0.8 r mi pixarr2) [0.1, 0.11 .. 0.3]
	end2 <- getCurrentTime
	putStrLn $ show (end2 `diffUTCTime` start2) ++ " elapsed."
