module Main where

import System.Environment
import Graphics.HsExif

main :: IO ()
main = do
  args <- getArgs
  let filePath = head args
  eitherExif <- parseFileExif filePath
  putStrLn (show eitherExif)
  return ()
