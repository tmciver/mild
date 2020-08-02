{-# LANGUAGE OverloadedStrings #-}

module Mild where

import           Control.Applicative ((<|>))
import           Control.Monad.Catch (MonadThrow, throwM, Exception)
import           Data.Coerce
import           Data.List (intercalate)
import           Data.Map (union)
import           Data.Map as Map
import           Data.Maybe (catMaybes)
import           Data.RDF
import           Data.RDF.Namespace (mkUri, schema, standard_ns_mappings)
import           Data.Set as Set
import qualified Data.Text as T
import           Graphics.HsExif
import           Network.URI (URI, parseURI)
import           Prelude as P

newtype ExifData = ExifData (Map.Map ExifTag ExifValue)

instance Show ExifData where
  show (ExifData edm) = intercalate "\n" $ show <$> Map.toList edm

data ExifError = TagDescriptionNotFound ExifTag
               | NoExifDataFound
instance Show ExifError where
  show (TagDescriptionNotFound tag) = "Could not find description for tag " <> show tag
  show NoExifDataFound = "No EXIF data found."
instance Exception ExifError

data ImageError = ConvertToURIError
instance Show ImageError where
  show ConvertToURIError = "Could not convert path to URI. Supply full path to file."
instance Exception ImageError

-- |Attempts to convert the given EXIF data value to an RDF graph.
-- Returns a tuple of the root node (blank) and the graph.
exifTagToGraph :: (Rdf a, MonadThrow m) => (ExifTag, ExifValue) -> m (Node, RDF a)
exifTagToGraph (tag, val) = do
  desc <- exifTagDesc tag
  let bn = BNodeGen 1--bn = bnode $ T.pack desc
      pm = PrefixMappings mempty
      nameNode = lnode (plainL (T.pack desc))
      valueNode = lnode (plainL (T.pack (show val)))
      triples = [ triple bn (unode "a") (unode (mkUri schema "PropertyValue"))
                , triple bn (unode (mkUri schema "name")) nameNode
                , triple bn (unode (mkUri schema "value")) valueNode
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
        g imageNode (n, propGraph) imageGraph = let triples = (triplesOf imageGraph) <> (triplesOf propGraph) <> [triple imageNode (unode (mkUri schema "hasProperty")) n]
                                                    baseUrl' = baseUrl imageGraph
                                                    pm = prefixMappings imageGraph
                                                in mkRdf triples baseUrl' pm
        f :: Rdf a => Node -> [(Node, RDF a)] -> RDF a
        f imageNode propGraphs = P.foldr (g imageNode) Data.RDF.empty propGraphs
        graph = case exifProps of
          [] -> throwM NoExifDataFound
          exif -> pure $ f imageNode exifProps

-- |Merge two PrefixMappings to produce a third.
-- Takes the value from the latter PrefixMapping when there are duplicates.
mergePrefixMappings :: PrefixMappings -> PrefixMappings -> PrefixMappings
mergePrefixMappings (PrefixMappings m1) (PrefixMappings m2) =
  PrefixMappings (Map.union m2 m1)

-- |Merge the two given RDF graphs to produce a new graph containing the triples from both.
mergeRdfGraphs :: Rdf a => RDF a -> RDF a -> RDF a
mergeRdfGraphs g1 g2 =
  let triples = triplesOf g1 <> triplesOf g2
      pms = mergePrefixMappings (prefixMappings g1) (prefixMappings g2)
      base = (baseUrl g1) <|> (baseUrl g2)
  in mkRdf triples base pms

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

-- |Convert a FilePath to a URI in the MonadThrow monad.
imagePathToUri :: MonadThrow m => FilePath -> m URI
imagePathToUri fp = let
  maybeUri = filePathToUri fp >>= parseURI
  in case maybeUri of
       Nothing -> throwM ConvertToURIError
       Just uri -> pure uri

-- |Convert a URI to an RDF Node
imageURIToNode :: URI -> Node
imageURIToNode = unode . T.pack . show

-- |Creates a graph for the image but without triples for EXIF data.
mkImageGraphNoExif :: (Rdf a, MonadThrow m)
                   => Node  -- ^ URI Node for image file.
                   -> m (RDF a)
mkImageGraphNoExif imageNode =
  let triples = [triple imageNode (unode "a") (unode (mkUri schema "ImageObject"))]
      base = Nothing
  in pure $ mkRdf triples base standard_ns_mappings

mkImageGraph :: (Rdf a, MonadThrow m)
             => FilePath  -- ^ Path to image file.
             -> IO (m (RDF a))
mkImageGraph fp = do
  eitherExif <- parseFileExif fp
  let graphThrow = do
        imageUri <- imagePathToUri fp
        let imageNode = imageURIToNode imageUri
        exifGraph <- case eitherExif of
          Left _ -> throwM NoExifDataFound
          Right exifMap ->
            let exif = removeMakerNote (ExifData exifMap)
            in exifDataToGraph imageNode exif
        imageGraph <- mkImageGraphNoExif imageNode
        pure $ mergeRdfGraphs imageGraph exifGraph
  pure graphThrow
