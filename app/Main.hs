{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Error.Util (note)
import Data.Coerce
import Data.List (intercalate)
import Data.Map as Map
import Prelude as P
import Data.RDF
import Data.Set as Set
import qualified Data.Text as T
import Graphics.HsExif
import System.Environment

newtype ExifData = ExifData (Map.Map ExifTag ExifValue)

instance Show ExifData where
  show (ExifData edm) = intercalate "\n" $ show <$> Map.toList edm

rdfPrefix :: T.Text
rdfPrefix = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

schemaPrefix :: T.Text
schemaPrefix = "http://schema.org/"

mkPrefixedNode :: T.Text -> T.Text -> Node
mkPrefixedNode prefix term = (unode (T.concat [prefix, term]))

-- |Attempts to convert the given EXIF data value to an RDF graph.
-- Returns a tuple of the root node (blank) and the graph.
exifTagToGraph :: Rdf a => (ExifTag, ExifValue) -> Maybe (Node, RDF a)
exifTagToGraph (tag, val) = do
  desc <- tagDesc tag
  let bn = bnode $ T.pack desc
      pm = PrefixMappings mempty
      nameNode = lnode (plainL (T.pack desc))
      valueNode = lnode (plainL (T.pack (show val)))
      triples = [ triple bn (mkPrefixedNode rdfPrefix "type") (mkPrefixedNode schemaPrefix "PropertyValue")
                , triple bn (mkPrefixedNode schemaPrefix "name") nameNode
                , triple bn (mkPrefixedNode schemaPrefix "value") valueNode
                ]
      graph = mkRdf triples Nothing pm
  Just (bn, graph)

-- |Returns a graph of the the given 'ExifData' data attached to the given image
-- node by the `additionalProperty` predicate.
exifDataToGraph :: Rdf a
                => Node          -- node for an image.
                -> ExifData      -- EXIF data
                -> Maybe (RDF a)
exifDataToGraph imageNode (ExifData ed) = graph
  where maybeExifProps = traverse exifTagToGraph (Map.toList ed)
        g :: Rdf a => Node -> (Node, RDF a) -> RDF a -> RDF a
        g imageNode (n, propGraph) imageGraph = let triples = (triplesOf imageGraph) <> (triplesOf propGraph) <> [triple imageNode (mkPrefixedNode schemaPrefix "hasProperty") n]
                                                    baseUrl' = baseUrl imageGraph
                                                    pm = prefixMappings imageGraph
                                                in mkRdf triples baseUrl' pm
        f :: Rdf a => Node -> [(Node, RDF a)] -> RDF a
        f imageNode propGraphs = P.foldr (g imageNode) Data.RDF.empty propGraphs
        graph = (f imageNode) <$> maybeExifProps

-- |Adds the EXIF data as properties of the given (blank) node.
-- exifToRdf :: Rdf r => ExifData -> Node -> Maybe (RDF r)
-- exifToRdf (ExifData edm) n = mkRdf triples Nothing prefixMaps
--   where triples = exifTagToGraph n <$> edm
--         prefixMaps = PrefixMappings Map.empty

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
      imageNode = unode $ T.pack filePath
  eitherExif <- parseFileExif filePath
  let eitherExifData = removeMakerNote . ExifData <$> eitherExif
      eitherGraph :: Either String (RDF TList)
      eitherGraph = do
        exifData <- eitherExifData
        note "Could not create graph from EXIF data." (exifDataToGraph imageNode exifData)
  case eitherGraph of
    Left e -> putStrLn $ show e
    Right graph -> print $ showGraph graph
    
  --let eitherTags = Map.keys <$> eitherExif
  --putStrLn (show eitherTags)
      --putStrLn (show eitherExifData)
  return ()
