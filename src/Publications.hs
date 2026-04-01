module Publications
  ( loadPublicationsPageCtx
  ) where

import Data.Char (isAlpha, toLower, toUpper)
import Data.List (intercalate, sortOn)
import Data.Maybe (listToMaybe)
import Data.Ord (Down (..))

import BibTeX
  ( BibEntry,
    breakOn,
    firstJust,
    lookupBibField,
    maybeToList,
    nonEmpty,
    normalizeBibText,
    parseBibEntries,
    prefixOf,
    splitOn,
    takeBalancedBraces,
    trimWhitespace,
  )
import Hakyll
  ( Compiler,
    Context,
    Identifier,
    Item (..),
    defaultContext,
    field,
    getMetadata,
    listField,
    lookupString,
    makeItem,
    noResult,
    unsafeCompiler,
  )

publishedBibPath :: FilePath
publishedBibPath = "assets/bib/published.bib"

data Publication = Publication
  { publicationTitle :: String
  , publicationAuthors :: [String]
  , publicationVenue :: String
  , publicationYear :: Int
  , publicationDoi :: Maybe String
  , publicationSlideUrl :: Maybe String
  , publicationPreprintUrl :: Maybe String
  , publicationAbstract :: Maybe String
  , publicationOrder :: Int
  }

publicationCtx :: Context Publication
publicationCtx =
  field "title" (return . escapePublicationHtml . publicationTitle . itemBody)
    <> field "authors" (return . escapePublicationHtml . formatAuthorList . publicationAuthors . itemBody)
    <> field "venueAndYear" (return . escapePublicationHtml . formatVenueAndYear . itemBody)
    <> field "abstractId" (return . publicationAbstractId . itemBody)
    <> field "abstractText" publicationAbstractField
    <> field "doiUrl" (optionalEscapedField publicationDoi)
    <> field "slideUrl" (optionalEscapedField publicationSlideUrl)
    <> field "preprintUrl" (optionalEscapedField publicationPreprintUrl)

loadPublicationsPageCtx :: Identifier -> Compiler (Context String)
loadPublicationsPageCtx page = do
  limit <- loadPageListLimit "publications-limit" page
  publications <- loadPublications publishedBibPath
  let boundedPublications = maybe publications (`take` publications) limit
  return $
    listField "publications" publicationCtx (return boundedPublications)
      <> defaultContext

loadPublications :: FilePath -> Compiler [Item Publication]
loadPublications bibPath =
  mapM makeItem
    =<< ( sortOn (\publication -> (Down (publicationYear publication), publicationOrder publication))
            . zipWith bibEntryToPublication [0 ..]
            . parseBibEntries
            <$> unsafeCompiler (readFile bibPath)
        )

bibEntryToPublication :: Int -> BibEntry -> Publication
bibEntryToPublication order entry =
  Publication
    { publicationTitle = normalizePublicationTitle $ fieldOrEmpty "title"
    , publicationAuthors = map normalizeBibText $ splitBibAuthors $ fieldOrEmpty "author"
    , publicationVenue = normalizeBibText $ firstNonEmpty ["booktitle", "journal", "series", "publisher"]
    , publicationYear = parseYear $ fieldOrEmpty "year"
    , publicationDoi = normalizeDoi <$> nonEmpty (fieldOrEmpty "doi")
    , publicationSlideUrl = extractLocalAssetLink ["slides", "slide"] noteLinks entry
    , publicationPreprintUrl = extractLocalAssetLink ["preprint", "pdf"] noteLinks entry
    , publicationAbstract = normalizeBibText <$> nonEmpty (fieldOrEmpty "abstract")
    , publicationOrder = order
    }
  where
    fieldOrEmpty fieldName = maybe "" id (lookupBibField fieldName entry)
    firstNonEmpty [] = ""
    firstNonEmpty (fieldName : remaining) =
      case lookupBibField fieldName entry >>= nonEmpty of
        Just value -> value
        Nothing -> firstNonEmpty remaining
    noteLinks =
      case lookupBibField "note" entry of
        Just noteValue -> extractHrefLinks noteValue
        Nothing -> []

extractLocalAssetLink :: [String] -> [(String, String)] -> BibEntry -> Maybe String
extractLocalAssetLink fieldNames noteLinks entry =
  firstJust (map fieldLink fieldNames ++ [noteLink])
  where
    fieldLink fieldName = lookupBibField fieldName entry >>= normalizeLocalUrl
    noteLink =
      listToMaybe
        [ normalizedUrl
        | (rawUrl, label) <- noteLinks
        , let normalizedLabel = map toLower (normalizeBibText label)
        , any (\expected -> isLabelMatch expected normalizedLabel) fieldNames
        , normalizedUrl <- maybeToList (normalizeLocalUrl rawUrl)
        ]

    isLabelMatch expected label =
      label == expected || label == expected ++ "s"

publicationAbstractId :: Publication -> String
publicationAbstractId publication =
  "publication-abstract-" ++ show (publicationOrder publication)

publicationAbstractField :: Item Publication -> Compiler String
publicationAbstractField item =
  case publicationAbstract (itemBody item) of
    Just abstractText -> return $ escapePublicationHtml abstractText
    Nothing -> noResult "publication has no abstract"

optionalEscapedField :: (Publication -> Maybe String) -> Item Publication -> Compiler String
optionalEscapedField selector item =
  case selector (itemBody item) of
    Just value -> return $ escapePublicationHtml value
    Nothing -> noResult "publication field is missing"

loadPageListLimit :: String -> Identifier -> Compiler (Maybe Int)
loadPageListLimit fieldName page = do
  metadata <- getMetadata page
  case lookupString fieldName metadata of
    Nothing -> return Nothing
    Just value ->
      case reads value of
        [(count, "")] | count >= 0 -> return $ Just count
        _ ->
          fail $
            "Could not parse " ++ fieldName ++ " in " ++ show page ++ ": " ++ value

formatAuthorList :: [String] -> String
formatAuthorList [] = ""
formatAuthorList [author] = author
formatAuthorList [authorA, authorB] = authorA ++ " and " ++ authorB
formatAuthorList authors =
  intercalate ", " (init authors) ++ ", and " ++ last authors

formatVenueAndYear :: Publication -> String
formatVenueAndYear publication =
  case nonEmpty (publicationVenue publication) of
    Just venue -> venue ++ ", " ++ show (publicationYear publication)
    Nothing -> show (publicationYear publication)

splitBibAuthors :: String -> [String]
splitBibAuthors rawAuthors =
  filter (not . null)
    . map trimWhitespace
    $ splitOn " and " rawAuthors

extractHrefLinks :: String -> [(String, String)]
extractHrefLinks [] = []
extractHrefLinks input =
  case breakOn "\\href{" input of
    Just (_, remainder) ->
      let (rawUrl, afterUrl) = takeBalancedBraces remainder
       in case afterUrl of
            '{' : afterOpeningLabel ->
              let (rawLabel, afterLabel) = takeBalancedBraces afterOpeningLabel
               in (trimWhitespace rawUrl, trimWhitespace rawLabel) : extractHrefLinks afterLabel
            _ -> extractHrefLinks afterUrl
    Nothing -> []

normalizePublicationTitle :: String -> String
normalizePublicationTitle =
  normalizeBibText . sentenceCaseBibTitle . stripOuterTitleBraces

sentenceCaseBibTitle :: String -> String
sentenceCaseBibTitle = go True (0 :: Int)
  where
    go _ _ [] = []
    go capitalizeNext braceDepth (character : remaining)
      | character == '{' = character : go capitalizeNext (braceDepth + 1) remaining
      | character == '}' = character : go capitalizeNext (max 0 (braceDepth - 1)) remaining
      | braceDepth > 0 =
          character
            : go
                (if isAlpha character then False else capitalizeNext)
                braceDepth
                remaining
      | isAlpha character =
          let normalizedCharacter =
                if capitalizeNext
                  then toUpper character
                  else toLower character
           in normalizedCharacter : go False braceDepth remaining
      | otherwise = character : go (capitalizeNext || startsSentence character) braceDepth remaining

    startsSentence character = character == ':'

stripOuterTitleBraces :: String -> String
stripOuterTitleBraces rawTitle =
  case trimWhitespace rawTitle of
    '{' : remaining ->
      let (inside, rest) = takeBalancedBraces remaining
       in if null (trimWhitespace rest)
            then stripOuterTitleBraces inside
            else trimWhitespace rawTitle
    trimmedTitle -> trimmedTitle

normalizeDoi :: String -> String
normalizeDoi rawDoi
  | "http://" `prefixOf` trimmedDoi = trimmedDoi
  | "https://" `prefixOf` trimmedDoi = trimmedDoi
  | otherwise = "https://doi.org/" ++ trimmedDoi
  where
    trimmedDoi = trimWhitespace rawDoi

normalizeLocalUrl :: String -> Maybe String
normalizeLocalUrl rawUrl
  | null trimmedUrl = Nothing
  | "http://" `prefixOf` trimmedUrl = Nothing
  | "https://" `prefixOf` trimmedUrl = Nothing
  | "./" `prefixOf` trimmedUrl = Just ('/' : drop 2 trimmedUrl)
  | "/" `prefixOf` trimmedUrl = Just trimmedUrl
  | otherwise = Just ('/' : trimmedUrl)
  where
    trimmedUrl = trimWhitespace rawUrl

escapePublicationHtml :: String -> String
escapePublicationHtml = concatMap escapeCharacter
  where
    escapeCharacter '&' = "&amp;"
    escapeCharacter '<' = "&lt;"
    escapeCharacter '>' = "&gt;"
    escapeCharacter '"' = "&quot;"
    escapeCharacter '\'' = "&#39;"
    escapeCharacter character = [character]

parseYear :: String -> Int
parseYear yearText =
  case reads yearText of
    [(year, "")] -> year
    _ -> error $ "Could not parse publication year: " ++ yearText
