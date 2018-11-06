module Main ( main ) where

import Test.Tasty
import Test.Tasty.Golden

import Data.ByteString.Lazy ( ByteString )
import Data.ByteString.Lazy.Char8 ( pack )

import System.FilePath
import System.Process

import Language.Java.Lexer ( lexJavaEither )

main :: IO ()
main = do
  lexerTests <- mkTests "lexer" runLexerTest
  replTests <- mkTests "repl" runReplTest
  defaultMain $ testGroup "tests" [lexerTests, replTests]

mkTests :: String                        -- name of the test group / directory
        -> (FilePath -> IO ByteString)   -- function to run test and return result
        -> IO TestTree
mkTests name test_fun = do
  input_files <- findByExtension [".java"] ("tests" </> name)
  return $ testGroup name
             [ goldenVsStringDiff test_name diff golden_file (test_fun input_file)
             | input_file <- input_files
             , let test_name   = takeBaseName input_file
                   golden_file = replaceExtensions input_file "golden" ]
    where
      diff l r = ["diff", "-u", l, r]

runLexerTest :: FilePath -> IO ByteString
runLexerTest path = do
  input <- readFile path
  return $ pack $ map comma_to_newline $ show $ lexJavaEither input
  where
    -- improves diffing
    comma_to_newline ','   = '\n'
    comma_to_newline other = other

runReplTest :: FilePath -> IO ByteString
runReplTest path = do
  input <- readFile path
  (exit_code, stdout, stderr) <- readProcessWithExitCode
                                   ("dist" </> "build" </> "javai" </> "javai") []
                                   input
  let result = "Exit code: " ++ show exit_code ++ "\n" ++
               "Stdout:\n" ++ stdout ++ "\n" ++
               "Stderr:\n" ++ stderr
  return (pack result)
