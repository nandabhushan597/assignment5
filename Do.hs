{- Author: Elizabeth Chan and Nanda Bhushan
 File: Do.hs
 Practice exercises with do-notation
 -}



module Main where

import Text.Read


main :: IO ()
main = do
    -- question 5
--  putStrLn "Hello, world!"
--  name <- prompt "What is your name?"
--  putStrLn $ "Hello, " ++ name


    -- question 6
  twoLines <- prompt2 "Enter two lines of text:"
  putStrLn $ twoLines

prompt :: String -> IO String
prompt query = do
  putStrLn query
  answer <- getLine
  pure answer


prompt2 :: String -> IO String
prompt2 query = do 
    putStrLn query 
    answer1 <- getLine
    answer2 <- getLine
    let together = answer1 ++ answer2
    pure together
  




-- main :: IO ()
-- main = do
-- str1 <- prompt "Enter a number:"
-- str2 <- prompt "Enter another number:"
-- case readMaybe str1 of
--   Nothing -> putStrLn "The first input isn't a number."
--   Just n1 -> case readMaybe str2 of
--     Nothing -> putStrLn "The second input isn't a number."
--     Just n2 -> do
--       putStrLn "You have entered two numbers."
--       let sum = n1 + n2
--       putStrLn $ "Your sum is " ++ show sum ++ "."
