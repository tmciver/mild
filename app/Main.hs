module Main where

import Data.Coerce
import Data.List (intercalate)
import Data.Map as Map
import Data.RDF
import Data.Set as Set
import Graphics.HsExif
import System.Environment

newtype ExifData = ExifData (Map.Map ExifTag ExifValue)

instance Show ExifData where
  show (ExifData edm) = intercalate "\n" $ show <$> Map.toList edm

exifToRdf :: Map.Map ExifTag ExifValue -> RDF TList
exifToRdf = undefined

-- |Remove the `makerNote` EXIF property from the given 'ExifData'.  This is
-- typically done because the value of this property is a long string of what
-- appears to be garbage.
removeMakerNote :: ExifData -> ExifData
removeMakerNote = coerce . removeMakerNoteMap . coerce
 where removeMakerNoteMap :: Map.Map ExifTag ExifValue -> Map.Map ExifTag ExifValue
       removeMakerNoteMap = flip Map.withoutKeys (Set.singleton makerNote)

main :: IO ()
main = do
  args <- getArgs
  let filePath = head args
  eitherExif <- parseFileExif filePath
  let eitherExifData = removeMakerNote . ExifData <$> eitherExif
  --let eitherTags = Map.keys <$> eitherExif
  --putStrLn (show eitherTags)
  putStrLn (show eitherExifData)
  return ()
