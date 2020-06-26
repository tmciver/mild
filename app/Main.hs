{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe (catMaybes)
import Control.Error.Util (note)
import Control.Monad.Catch (MonadThrow, throwM, Exception)
import Data.Bifunctor (first)
import Data.Coerce
import Data.List (intercalate)
import Data.Map as Map
import Data.RDF
import Data.Set as Set
import qualified Data.Text as T
import Graphics.HsExif
import Prelude as P
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

data ExifError = TagDescriptionNotFound ExifTag
               | NoExifDataFound
instance Show ExifError where
  show (TagDescriptionNotFound tag) = "Could not find description for tag " <> show tag
  show NoExifDataFound = "No EXIF data found."
instance Exception ExifError

-- |Attempts to convert the given EXIF data value to an RDF graph.
-- Returns a tuple of the root node (blank) and the graph.
exifTagToGraph :: (Rdf a, MonadThrow m) => (ExifTag, ExifValue) -> m (Node, RDF a)
exifTagToGraph (tag, val) = do
  desc <- exifTagDesc tag
  let bn = bnode $ T.pack desc
      pm = PrefixMappings mempty
      nameNode = lnode (plainL (T.pack desc))
      valueNode = lnode (plainL (T.pack (show val)))
      triples = [ triple bn (mkPrefixedNode rdfPrefix "type") (mkPrefixedNode schemaPrefix "PropertyValue")
                , triple bn (mkPrefixedNode schemaPrefix "name") nameNode
                , triple bn (mkPrefixedNode schemaPrefix "value") valueNode
                ]
      graph = mkRdf triples Nothing pm
  pure (bn, graph)

  where exifTagDesc :: MonadThrow m => ExifTag -> m String
        exifTagDesc tag = case tagDesc tag of
          Nothing -> throwM (TagDescriptionNotFound tag)
          Just desc -> pure desc

-- |Returns a graph of the the given 'ExifData' data attached to the given image
-- node by the `additionalProperty` predicate.
exifDataToGraph :: (Rdf a, MonadThrow m)
                => Node          -- node for an image.
                -> ExifData      -- EXIF data
                -> m (RDF a)
exifDataToGraph imageNode (ExifData ed) = graph
  where exifProps = catMaybes $ exifTagToGraph <$> (Map.toList ed)
        g :: Rdf a => Node -> (Node, RDF a) -> RDF a -> RDF a
        g imageNode (n, propGraph) imageGraph = let triples = (triplesOf imageGraph) <> (triplesOf propGraph) <> [triple imageNode (mkPrefixedNode schemaPrefix "hasProperty") n]
                                                    baseUrl' = baseUrl imageGraph
                                                    pm = prefixMappings imageGraph
                                                in mkRdf triples baseUrl' pm
        f :: Rdf a => Node -> [(Node, RDF a)] -> RDF a
        f imageNode propGraphs = P.foldr (g imageNode) Data.RDF.empty propGraphs
        graph = case exifProps of
          [] -> throwM NoExifDataFound
          exif -> pure $ f imageNode exifProps

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
        first show (exifDataToGraph imageNode exifData)
  case eitherGraph of
    Left e -> putStrLn $ show e
    Right graph -> let mappings = PrefixMappings $ Map.fromList [("schema", "http://schema.org/")]
                       docUrl = T.concat ["localhost:3000/images/123"]
                       serializer = TurtleSerializer (Just docUrl) mappings
                   in writeRdf serializer graph
