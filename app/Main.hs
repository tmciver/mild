module Main where

import System.Environment
import Graphics.HsExif
import Data.RDF
import Data.Map as Map

exifToRdf :: Map.Map ExifTag ExifValue -> RDF TList
exifToRdf = undefined

main :: IO ()
main = do
  args <- getArgs
  let filePath = head args
  eitherExif <- parseFileExif filePath
  putStrLn (show eitherExif)
  return ()
