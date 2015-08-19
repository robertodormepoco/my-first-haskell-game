module Main where

import System.IO
import System.Random
import Data.Char

main = game

-- pick the random number

pickNumber :: IO Int
pickNumber = randomRIO (0, 100)

-- given the random pick and a guess it says if they match

guess :: Int -> IO Bool
guess pick = do
 putStr "make your choice [u/d]? "
 flush
 input <- getLine
 case head (map toUpper input) of
  'U' -> return (pick >= 50)
  'D' -> return (pick < 50)
  _ -> do
    putStrLn "wrong input"
    guess pick

flush :: IO ()
flush = hFlush stdout

endGame :: Int -> IO ()
endGame score = do
	blankLine
	blankLine
	putStrLn $ "game over: your final score is " ++ (show score)

blankLine :: IO ()
blankLine = putStrLn ""

game :: IO()
game = do
	putStrLn "-- THE GAME --"
	putStrLn "every round a number from 0 to 100 is picked, you have to guess if it is \"up\" or \"down\" the middle point (50)"
	putStrLn "hint: up goes for equality with 50"
	putStrLn "are you ready to play?"
	loop 0 1
		where loop score round = 
			if score == 5 
			then endGame score
			else do
				putStrLn ("your current score is " ++ (show score) ++ ", " ++ (show (5 - score)) ++ " left to reach 5 and win the game")
				number <- pickNumber
				win <- guess number
				if win
				then putStrLn ("good job! the guessed number was " ++ (show number) ++ ", +1 point") >> loop (score + 1) (round + 1)
				else putStrLn ("bad news :( the guessed number was " ++ (show number) ++ ", -1 point") >> loop (if score > 0 then score - 1 else 0) (round + 1)
