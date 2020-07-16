{-# LANGUAGE OverloadedStrings #-}

module Lib (libMain) where

import           Data.Char
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.Environment
import           System.Exit
import           System.IO

libMain :: IO ()
libMain = getArgs >>= run

run :: [String] -> IO ()
run [] = runREPL
run [x] = runFile x
run _ = putStrLn "Usage: hlox [script]" *> exitWith (ExitFailure 64)

runFile :: String -> IO ()
runFile x = TIO.readFile x >>= execute

prompt :: String -> IO (Maybe Text)
prompt text = do
  putStr text
  hFlush stdout
  eof <- isEOF
  if eof
    then return Nothing
    else Just <$> TIO.getLine

runREPL :: IO ()
runREPL = do
  line <- prompt "> "
  maybe (putStrLn "Bye!") (\a -> execute a *> runREPL) line

execute :: Text -> IO ()
execute a = mapM_ (putStrLn . T.unpack) $ filter ("" /=) $ T.split isSpace a

data InterpreterError =
  InterpreterError { line :: Int, location :: Text, message :: Text }
  deriving (Show)
