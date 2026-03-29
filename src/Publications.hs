module Publications
  ( loadPublicationsPageCtx
  ) where

import Data.Char (isAlpha, isAlphaNum, isSpace, toLower, toUpper)
import Data.List (intercalate, sortOn)
import Data.Maybe (listToMaybe)
import Data.Ord (Down (..))

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

data BibEntry = BibEntry
  { bibEntryFields :: [(String, String)]
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
        Nothing    -> firstNonEmpty remaining
    noteLinks =
      case lookupBibField "note" entry of
        Just noteValue -> extractHrefLinks noteValue
        Nothing        -> []

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
    Nothing           -> noResult "publication has no abstract"

optionalEscapedField :: (Publication -> Maybe String) -> Item Publication -> Compiler String
optionalEscapedField selector item =
  case selector (itemBody item) of
    Just value -> return $ escapePublicationHtml value
    Nothing    -> noResult "publication field is missing"

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
    Nothing    -> show (publicationYear publication)

lookupBibField :: String -> BibEntry -> Maybe String
lookupBibField fieldName = lookup (map toLower fieldName) . bibEntryFields

parseBibEntries :: String -> [BibEntry]
parseBibEntries input =
  case dropWhile (/= '@') input of
    [] -> []
    '@' : rest ->
      let (_, afterType) = span isBibTypeChar (dropWhile isSpace rest)
       in case dropWhile isSpace afterType of
            '{' : remaining ->
              let (entryBody, restInput) = takeBalancedBraces remaining
               in prependMaybe (parseBibEntry entryBody) (parseBibEntries restInput)
            '(' : remaining ->
              let (entryBody, restInput) = takeBalancedParens remaining
               in prependMaybe (parseBibEntry entryBody) (parseBibEntries restInput)
            _ -> parseBibEntries rest
    _ -> []
  where
    isBibTypeChar character = isAlphaNum character || character `elem` ("-_:" :: String)

parseBibEntry :: String -> Maybe BibEntry
parseBibEntry entryBody = do
  (_, fieldsBody) <- splitTopLevelComma entryBody
  pure BibEntry {bibEntryFields = parseBibFields fieldsBody}

parseBibFields :: String -> [(String, String)]
parseBibFields = go . dropSeparators
  where
    go [] = []
    go remaining =
      let (rawFieldName, afterFieldName) = span isFieldNameChar remaining
          fieldName = map toLower $ trimWhitespace rawFieldName
          afterEquals =
            case dropWhile isSpace afterFieldName of
              '=' : rest -> dropWhile isSpace rest
              rest       -> rest
       in if null fieldName
            then []
            else
              case parseBibValue afterEquals of
                Nothing -> []
                Just (value, rest) ->
                  (fieldName, value) : go (dropSeparators rest)

    isFieldNameChar character = isAlphaNum character || character `elem` ("-_:" :: String)
    dropSeparators = dropWhile (\character -> isSpace character || character == ',')

parseBibValue :: String -> Maybe (String, String)
parseBibValue [] = Nothing
parseBibValue ('{' : remaining) =
  let (value, rest) = takeBalancedBraces remaining
   in Just (value, rest)
parseBibValue ('"' : remaining) =
  let (value, rest) = takeBalancedQuotes remaining
   in Just (value, rest)
parseBibValue remaining =
  let (value, rest) = span (\character -> character /= ',' && character /= '\n' && character /= '\r') remaining
   in Just (trimWhitespace value, rest)

takeBalancedBraces :: String -> (String, String)
takeBalancedBraces = takeBalancedDelimited '{' '}'

takeBalancedParens :: String -> (String, String)
takeBalancedParens = takeBalancedDelimited '(' ')'

takeBalancedDelimited :: Char -> Char -> String -> (String, String)
takeBalancedDelimited open close = go (1 :: Int) []
  where
    go _ acc [] = (reverse acc, [])
    go depth acc (character : remaining)
      | character == open = go (depth + 1) (character : acc) remaining
      | character == close =
          if depth == 1
            then (reverse acc, remaining)
            else go (depth - 1) (character : acc) remaining
      | otherwise = go depth (character : acc) remaining

takeBalancedQuotes :: String -> (String, String)
takeBalancedQuotes = go False (0 :: Int) []
  where
    go _ _ acc [] = (reverse acc, [])
    go escaped braceDepth acc (character : remaining)
      | escaped = go False braceDepth (character : acc) remaining
      | character == '\\' = go True braceDepth (character : acc) remaining
      | character == '{' = go False (braceDepth + 1) (character : acc) remaining
      | character == '}' && braceDepth > 0 = go False (braceDepth - 1) (character : acc) remaining
      | character == '"' && braceDepth == 0 = (reverse acc, remaining)
      | otherwise = go False braceDepth (character : acc) remaining

splitTopLevelComma :: String -> Maybe (String, String)
splitTopLevelComma = go (0 :: Int) False []
  where
    go _ _ _ [] = Nothing
    go braceDepth inQuotes acc (character : remaining)
      | character == '"' = go braceDepth (not inQuotes) (character : acc) remaining
      | not inQuotes && character == '{' = go (braceDepth + 1) False (character : acc) remaining
      | not inQuotes && character == '}' && braceDepth > 0 = go (braceDepth - 1) False (character : acc) remaining
      | not inQuotes && braceDepth == 0 && character == ',' = Just (reverse acc, remaining)
      | otherwise = go braceDepth inQuotes (character : acc) remaining

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

normalizeBibText :: String -> String
normalizeBibText =
  unwords
    . words
    . stripSimpleBraces
    . replaceAllLiterals bibTextReplacements
  where
    bibTextReplacements =
      [ ("\\'{a}", "á")
      , ("\\'{A}", "Á")
      , ("\\'{e}", "é")
      , ("\\'{E}", "É")
      , ("\\'{i}", "í")
      , ("\\'{I}", "Í")
      , ("\\'{o}", "ó")
      , ("\\'{O}", "Ó")
      , ("\\'{u}", "ú")
      , ("\\'{U}", "Ú")
      , ("\\'{\\i}", "í")
      , ("\\\"{a}", "ä")
      , ("\\\"{A}", "Ä")
      , ("\\\"{o}", "ö")
      , ("\\\"{O}", "Ö")
      , ("\\\"{u}", "ü")
      , ("\\\"{U}", "Ü")
      , ("\\v{c}", "č")
      , ("\\v{C}", "Č")
      , ("\\v{r}", "ř")
      , ("\\v{R}", "Ř")
      , ("\\c{t}", "ţ")
      , ("\\c{T}", "Ţ")
      , ("\\&", "&")
      , ("~", " ")
      ]

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

stripSimpleBraces :: String -> String
stripSimpleBraces [] = []
stripSimpleBraces (character : remaining)
  | character == '{' || character == '}' = stripSimpleBraces remaining
  | otherwise = character : stripSimpleBraces remaining

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

replaceAllLiterals :: [(String, String)] -> String -> String
replaceAllLiterals replacements input =
  foldl (\value (needle, replacement) -> replaceLiteral needle replacement value) input replacements

replaceLiteral :: Eq a => [a] -> [a] -> [a] -> [a]
replaceLiteral needle replacement = go
  where
    go haystack
      | needle `isPrefixOf` haystack = replacement ++ go (drop (length needle) haystack)
      | otherwise =
          case haystack of
            [] -> []
            x : xs -> x : go xs

    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys

breakOn :: Eq a => [a] -> [a] -> Maybe ([a], [a])
breakOn needle haystack = search [] haystack
  where
    search _ [] = Nothing
    search acc rest
      | needle `isPrefixOf` rest = Just (reverse acc, drop (length needle) rest)
      | otherwise =
          case rest of
            x : xs -> search (x : acc) xs

    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn delimiter input =
  go input
  where
    go remaining =
      case breakOn delimiter remaining of
        Just (before, after) -> before : go after
        Nothing              -> [remaining]

trimWhitespace :: String -> String
trimWhitespace = reverse . dropWhile isSpace . reverse . dropWhile isSpace

parseYear :: String -> Int
parseYear yearText =
  case reads yearText of
    [(year, "")] -> year
    _            -> error $ "Could not parse publication year: " ++ yearText

nonEmpty :: String -> Maybe String
nonEmpty value
  | null (trimWhitespace value) = Nothing
  | otherwise = Just value

prefixOf :: Eq a => [a] -> [a] -> Bool
prefixOf [] _ = True
prefixOf _ [] = False
prefixOf (needleHead : needleTail) (haystackHead : haystackTail) =
  needleHead == haystackHead && prefixOf needleTail haystackTail

prependMaybe :: Maybe a -> [a] -> [a]
prependMaybe Nothing values = values
prependMaybe (Just value) values = value : values

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Nothing : remaining) = firstJust remaining
firstJust (Just value : _) = Just value

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just value) = [value]
