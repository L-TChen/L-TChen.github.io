{-# LANGUAGE OverloadedStrings #-}

module Forester
  ( ForesterPost (..)
  , decodeForesterPosts
  , deduplicatePostTags
  , loadForesterPosts
  , normaliseTagValue
  ) where

import Control.Exception (displayException)
import Control.Monad (foldM)
import Data.Aeson
  ( FromJSON (parseJSON)
  , eitherDecode
  , withObject
  , (.:)
  )
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum, isSpace, toLower)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Ord (Down (Down))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LazyText
import Data.Time.Calendar (Day, fromGregorianValid)
import Hakyll
  ( Compiler
  , Identifier
  , Item (Item)
  , Pattern
  , fromFilePath
  , itemBody
  , itemIdentifier
  , loadAll
  , loadBody
  , hasVersion
  , toFilePath
  , (.&&.)
  )
import Text.Read (readMaybe)
import Text.XML
  ( Document
  , Element (elementName)
  , Name (Name)
  , Node (NodeElement)
  , def
  , parseText
  )
import Text.XML.Cursor
  ( Cursor
  , content
  , element
  , fromDocument
  , node
  , ($/)
  )

data ForesterPost = ForesterPost
  { foresterPostTitle :: String
  , foresterPostUrl :: String
  , foresterPostDate :: Day
  , foresterPostTaxon :: String
  , foresterPostTags :: [String]
  }
  deriving (Eq, Show)

data ManifestEntry = ManifestEntry
  { manifestTitle :: Text
  , manifestUri :: Text
  , manifestTaxon :: Maybe Text
  , manifestTags :: [Text]
  , manifestRoute :: Text
  , manifestMetas :: Map.Map Text Text
  }
  deriving (Eq, Show)

instance FromJSON ManifestEntry where
  parseJSON = withObject "Forester manifest entry" $ \value ->
    ManifestEntry
      <$> value .: "title"
      <*> value .: "uri"
      <*> value .: "taxon"
      <*> value .: "tags"
      <*> value .: "route"
      <*> value .: "metas"

data XmlEntry = XmlEntry
  { xmlUri :: Text
  , xmlRoute :: Text
  , xmlCanonicalUri :: Text
  , xmlDates :: [Day]
  }
  deriving (Eq, Show)

foresterNamespace :: Text
foresterNamespace = "http://www.forester-notes.org"

foresterCanonicalBase :: Text
foresterCanonicalBase = "https://l-tchen.github.io"

foresterName :: Text -> Name
foresterName localName = Name localName (Just foresterNamespace) Nothing

loadForesterPosts :: Identifier -> Pattern -> Compiler [Item ForesterPost]
loadForesterPosts manifestIdentifier xmlPattern = do
  manifest <- loadBody manifestIdentifier
  xmlItems <- loadAll $ xmlPattern .&&. hasVersion "metadata"
  let xmlSources =
        map
          (\item -> (toFilePath $ itemIdentifier item, itemBody item))
          xmlItems
  case decodeForesterPosts manifest xmlSources of
    Left message -> fail message
    Right posts ->
      return $
        map
          (\post -> Item (fromFilePath $ "forester-posts/" ++ normaliseTagValue (foresterPostUrl post)) post)
          posts

decodeForesterPosts :: String -> [(FilePath, String)] -> Either String [ForesterPost]
decodeForesterPosts manifestSource xmlSources = do
  manifestEntries <-
    either
      (Left . ("Could not parse Forester manifest: " ++))
      Right
      (eitherDecode $ BL.fromStrict $ Text.encodeUtf8 $ Text.pack manifestSource)
  parsedXml <- mapM (uncurry parseXmlEntry) xmlSources
  xmlByUri <- foldM insertXmlEntry Map.empty parsedXml
  publishedEntries <- mapM publicationStatus manifestEntries
  posts <- mapM (entryToPost xmlByUri) [entry | Just entry <- publishedEntries]
  return $ sortOn (Down . foresterPostDate) posts

publicationStatus :: ManifestEntry -> Either String (Maybe ManifestEntry)
publicationStatus entry =
  case Map.lookup "site-publish" (manifestMetas entry) of
    Nothing -> Right Nothing
    Just "true" -> Right $ Just entry
    Just value ->
      Left $
        "Invalid site-publish value for Forester tree "
          ++ Text.unpack (manifestUri entry)
          ++ ": expected true, got "
          ++ Text.unpack value

entryToPost :: Map.Map Text XmlEntry -> ManifestEntry -> Either String ForesterPost
entryToPost xmlByUri entry = do
  title <- requireText "title" (manifestUri entry) $ Just $ manifestTitle entry
  taxon <- requireText "taxon" (manifestUri entry) $ manifestTaxon entry
  validateRoute entry
  xmlEntry <-
    maybe
      (Left $ "No generated Forester XML found for published tree " ++ Text.unpack (manifestUri entry))
      Right
      (Map.lookup (manifestUri entry) xmlByUri)
  validateXmlEntry entry xmlEntry
  publicationDate <-
    maybe
      (Left $ "Published Forester tree has no complete date: " ++ Text.unpack (manifestUri entry))
      Right
      (listToMaybe $ sortOn id $ xmlDates xmlEntry)
  return
    ForesterPost
      { foresterPostTitle = Text.unpack title
      , foresterPostUrl = Text.unpack $ manifestRoute entry
      , foresterPostDate = publicationDate
      , foresterPostTaxon = Text.unpack taxon
      , foresterPostTags = deduplicatePostTags $ map Text.unpack $ manifestTags entry
      }

requireText :: String -> Text -> Maybe Text -> Either String Text
requireText fieldName uri value =
  case Text.strip <$> value of
    Just result | not (Text.null result) -> Right result
    _ -> Left $ "Published Forester tree " ++ Text.unpack uri ++ " is missing " ++ fieldName

validateRoute :: ManifestEntry -> Either String ()
validateRoute entry
  | not ("/posts/" `Text.isPrefixOf` route) = invalid
  | not ("/" `Text.isSuffixOf` route) = invalid
  | ".." `elem` Text.splitOn "/" route = invalid
  | "//" `Text.isInfixOf` route = invalid
  | otherwise = Right ()
  where
    route = manifestRoute entry
    invalid =
      Left $
        "Published Forester tree "
          ++ Text.unpack (manifestUri entry)
          ++ " has a non-local or unsafe route: "
          ++ Text.unpack route

validateXmlEntry :: ManifestEntry -> XmlEntry -> Either String ()
validateXmlEntry manifestEntry xmlEntry
  | manifestUri manifestEntry /= xmlUri xmlEntry = mismatch "display URI"
  | manifestRoute manifestEntry /= xmlRoute xmlEntry = mismatch "route"
  | foresterCanonicalBase <> manifestRoute manifestEntry /= xmlCanonicalUri xmlEntry = mismatch "canonical URI"
  | otherwise = Right ()
  where
    mismatch fieldName =
      Left $
        "Forester manifest/XML "
          ++ fieldName
          ++ " mismatch for tree "
          ++ Text.unpack (manifestUri manifestEntry)

parseXmlEntry :: FilePath -> String -> Either String XmlEntry
parseXmlEntry path source = do
  document <-
    either
      (Left . (("Could not parse generated Forester XML " ++ path ++ ": ") ++) . displayException)
      Right
      (parseText def $ LazyText.fromStrict $ Text.pack source)
  parseXmlDocument path document

parseXmlDocument :: FilePath -> Document -> Either String XmlEntry
parseXmlDocument path document = do
  let root = fromDocument document
  case node root of
    NodeElement rootElement
      | elementName rootElement == foresterName "tree" -> Right ()
    _ -> Left $ "Generated XML does not have a Forester root tree: " ++ path
  let frontmatters = root $/ element (foresterName "frontmatter")
  frontmatter <- exactlyOne path "frontmatter" frontmatters
  displayUri <- exactlyOneText path "display-uri" frontmatter
  route <- exactlyOneText path "route" frontmatter
  canonicalUri <- exactlyOneText path "uri" frontmatter
  dates <- mapM (parseDate path) $ frontmatter $/ element (foresterName "date")
  return
    XmlEntry
      { xmlUri = displayUri
      , xmlRoute = route
      , xmlCanonicalUri = canonicalUri
      , xmlDates = dates
      }

parseDate :: FilePath -> Cursor -> Either String Day
parseDate path cursor = do
  year <- datePart "year"
  month <- datePart "month"
  day <- datePart "day"
  maybe
    (Left $ "Invalid complete date in generated Forester XML " ++ path)
    Right
    (fromGregorianValid year month day)
  where
    datePart name = do
      value <- exactlyOneText path name cursor
      maybe
        (Left $ "Invalid " ++ Text.unpack name ++ " in generated Forester XML " ++ path)
        Right
        (readMaybe $ Text.unpack value)

exactlyOneText :: FilePath -> Text -> Cursor -> Either String Text
exactlyOneText path localName cursor = do
  matchedNode <- exactlyOne path (Text.unpack localName) $ cursor $/ element (foresterName localName)
  let value = Text.strip $ Text.concat $ matchedNode $/ content
  if Text.null value
    then Left $ "Empty " ++ Text.unpack localName ++ " in generated Forester XML " ++ path
    else Right value

exactlyOne :: FilePath -> String -> [a] -> Either String a
exactlyOne _ _ [value] = Right value
exactlyOne path fieldName values =
  Left $
    "Expected exactly one "
      ++ fieldName
      ++ " in generated Forester XML "
      ++ path
      ++ ", found "
      ++ show (length values)

insertXmlEntry :: Map.Map Text XmlEntry -> XmlEntry -> Either String (Map.Map Text XmlEntry)
insertXmlEntry entries entry
  | Map.member (xmlUri entry) entries = Left $ "Duplicate generated Forester XML for tree " ++ Text.unpack (xmlUri entry)
  | otherwise = Right $ Map.insert (xmlUri entry) entry entries

deduplicatePostTags :: [String] -> [String]
deduplicatePostTags =
  foldr keepFirst [] . sortOn normaliseTagValue
  where
    keepFirst label [] = [label]
    keepFirst label acc@(existing : _)
      | normaliseTagValue label == normaliseTagValue existing = acc
      | otherwise = label : acc

normaliseTagValue :: String -> String
normaliseTagValue =
  trimChar '-' .
  collapseRepeated '-' .
  map simplify .
  trimWhitespace
  where
    simplify character
      | isAlphaNum character = toLower character
      | otherwise = '-'

trimWhitespace :: String -> String
trimWhitespace = reverse . dropWhile isSpace . reverse . dropWhile isSpace

collapseRepeated :: Eq a => a -> [a] -> [a]
collapseRepeated _ [] = []
collapseRepeated marker (x : xs) = x : go x xs
  where
    go _ [] = []
    go previous (y : ys)
      | previous == marker && y == marker = go previous ys
      | otherwise = y : go y ys

trimChar :: Eq a => a -> [a] -> [a]
trimChar marker =
  reverse . dropWhile (== marker) . reverse . dropWhile (== marker)
