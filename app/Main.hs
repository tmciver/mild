{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Bifunctor (first)
import           Data.Map as Map
import           Data.RDF
import qualified Data.Text as T
import           Graphics.HsExif
import           Mild
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  let filePath = head args
      imageNode = unode $ T.pack filePath
  eitherExif <- parseFileExif filePath
  let eitherExifData = removeMakerNote . ExifData <$> eitherExif
      eitherGraph :: Either String (RDF TList)
      eitherGraph = do
        exifData <- eitherExifData
        first show (exifDataToGraph imageNode exifData)
  case eitherGraph of
    Left e -> putStrLn $ show e
    Right graph -> let mappings = PrefixMappings $ Map.fromList [("schema", "http://schema.org/")]
                       docUrl = T.concat ["localhost:3000/images/123"]
                       serializer = TurtleSerializer (Just docUrl) mappings
                   in writeRdf serializer graph
