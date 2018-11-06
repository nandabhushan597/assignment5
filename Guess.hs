{- Author: Elizabeth Chan and Nanda Bhushan
 File: Guess.hs
 Guessing game
 -}
 
module Guess where
 
import Data.Char
import Data.Maybe
import System.Environment
import System.Exit
import System.Random
 
main :: IO ()
main = do 
  putStrLn $ "\nGuess the number (between 0 and 100)"
  target <- randomRIO (1,100)
  guessFor target 
  showAnswer target


showAnswer :: Int -> IO ()
showAnswer answer = putStrLn $ "The answer was " ++ show answer

guessFor :: Int -> IO ()
guessFor target  =  do 
  guess <- getNum "Current guess? "
  if target == guess
     then putStrLn $ "You got the right answer!"
     else guessWrong target guess
 
 
guessWrong :: Int -> Int -> IO ()
guessWrong target guess = do
  if target > guess
     then putStrLn $ "Too Low"
     else putStrLn $ "Too high"
  guessFor target

isNum :: String -> Bool
isNum [] = False 
isNum (x:xs) = all isDigit xs && (x == '-' || isDigit x)

-- Prompt until a valid number is read, and return it
getNum :: String -> IO Int
getNum query = do
  putStrLn query
  answer <- getLine
  if isNum answer
    then return $ read answer
    else getNum "Not a number, try again with another guess: "
