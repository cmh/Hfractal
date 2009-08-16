import Mandcomp
import Data.Time.Clock
import Data.Array.IO
import Mandstate

main = do
	start <- getCurrentTime
	pixarr <- newArray (0, width*height-1) 0.0 :: IO Pix
	mapM_ (\r -> compPoints 0.002 0.8 r pixarr) [0.1, 0.15 .. 1.0]
	end <- getCurrentTime
	putStrLn $ show (end `diffUTCTime` start) ++ " elapsed."
