module Main where

import FracComp
import FracState
import Data.Time.Clock
import Data.Array.IO

(px, py) = (0.002, 0.8)
(testWidth, testHeight) = (800, 800)
mi = 400

profileComputeArray width height num = do
	create <- getCurrentTime
	pixarr <- initPixArray width height
	start <- getCurrentTime
	mapM_ (\range -> compPoints px py range mi pixarr) (take num [0.1, 0.11 ..])
	end <- getCurrentTime
	return (create, start, end)
	
profileMoveArray width height num = do
	create <- getCurrentTime
	pixarr <- initPixArray width height
	start <- getCurrentTime
	mapM_ (\range -> movePoints px py range mi pixarr) (take num [0.1, 0.11 ..])
	end <- getCurrentTime
	return (create, start, end)
	
display (create, start, end) num = do
	let createTime  = start `diffUTCTime` create
	let compTime	= end `diffUTCTime` start
	putStrLn $ (show num) ++ " computations took " ++ (show compTime) ++ " (creation took " ++ (show createTime) ++ ")"

main = do
	putStrLn "Testing compute array"
	(c, s, e) <- profileComputeArray testWidth testHeight 10
	display (c, s, e) 10
	--
	putStrLn "Testing move array"
	(c, s, e) <- profileMoveArray testWidth testHeight 10
	display (c, s, e) 10
