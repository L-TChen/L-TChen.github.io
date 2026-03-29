--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (foldM)
import Data.Char (isAlphaNum, isSpace, toLower)
import Data.List (intercalate, sortOn)
import Data.Maybe (listToMaybe)
import Data.Monoid (mappend)
import Data.Ord (Down (..))
import Data.Time.Format

import System.FilePath
  ( dropExtension,
    joinPath,
    splitDirectories,
    splitPath,
    takeBaseName,
    takeDirectory,
  )
import Text.Pandoc
  ( Extension (..),
    HTMLMathMethod (..),
    ReaderOptions (..),
    WriterOptions (..),
    extensionsFromList,
    writerHTMLMathMethod,
  )

import Hakyll hiding (pandocBiblioCompiler)
import Hakyll.Web.Sass ( sassCompiler )
--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  summerInternsDependency <- makePatternDependency "content/interns/interns.md"
  summerInternsTemplateDependency <- makePatternDependency "templates/summer-interns.html"

  match "assets/html/**" $ do
    route $ gsubRoute "assets/html/" (const "")
    compile copyFileCompiler

  match "assets/img/*" $ do
    route $ gsubRoute "assets/" (const "")
    compile copyFileCompiler

  match "content/interns/pdf/*.pdf" $ do
    route $ gsubRoute "content/interns/" (const "")
    compile copyFileCompiler

  scssDependency <- makePatternDependency "bootstrap/package.json"
  rulesExtraDependencies [scssDependency] $ match "assets/scss/default.scss" $ do
      route $ setExtension "css" `composeRoutes` gsubRoute "assets/scss/" (const "css/")
      compile (fmap compressCss <$> sassCompiler)

  match "assets/bib/*" $ compile biblioCompiler
  match "assets/csl/*" $ compile cslCompiler

  match "templates/*.html" $ compile templateBodyCompiler

  rulesExtraDependencies [summerInternsDependency, summerInternsTemplateDependency] $ do
    match "content/index.md" $ do
      route $ gsubRoute "content/" (const "") `composeRoutes` setExtension "html"
      compile $ do
        recentInterns <- take 8 <$> loadSummerInterns summerInternsSource
        publications <- loadPublications publishedBibPath
        pandocBiblioCompiler "assets/csl/elsevier-with-titles.csl" "assets/bib/*.bib"
          >>= replaceSummerInternsPlaceholder recentInterns
          >>= replacePublicationsPlaceholder publications
          >>= loadAndApplyTemplates defaultContext defaultTemplate
          >>= relativizeUrls

    match "content/interns/interns.md" $ do
      route $ constRoute "interns.html"
      compile $ do
        interns <- loadSummerInterns summerInternsSource
        pandocBiblioCompiler "assets/csl/elsevier-with-titles.csl" "assets/bib/*.bib"
          >>= replaceSummerInternsPlaceholder interns
          >>= loadAndApplyTemplates defaultContext defaultTemplate
          >>= relativizeUrls

  match ("content/*.md" .&&. complement "content/index.md") $ do
    route $ gsubRoute "content/" (const "") `composeRoutes` setExtension "html"
    let indexCtx = defaultContext
    compile $
      pandocBiblioCompiler "assets/csl/elsevier-with-titles.csl" "assets/bib/*.bib"
        >>= loadAndApplyTemplates indexCtx defaultTemplate
        >>= relativizeUrls

  match "content/posts/**.md" $ do
    route $
      postURL `composeRoutes` setExtension "html"

    let indexCtx = postCtx
    compile $
      pandocBiblioCompiler "assets/csl/elsevier-with-titles.csl" "assets/bib/*.bib"
        >>= loadAndApplyTemplates indexCtx postTemplate
        >>= relativizeUrls

  create ["posts.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "content/posts/**"
      let archiveCtx =
            listField "posts" postCtx (return posts) `mappend`
            constField "title" "Posts"               `mappend`
            defaultContext

      makeItem ""
        >>= loadAndApplyTemplates archiveCtx postsTemplate
        >>= relativizeUrls

  where
    baseTemplate =
      [ "templates/footer.html"
      , "templates/navbar.html"
      , "templates/head.html"
      ]
    postsTemplate   = "templates/posts.html" : "templates/default.html" : baseTemplate
    postTemplate    = "templates/post.html" : baseTemplate
    defaultTemplate = "templates/default.html" : baseTemplate

--------------------------------------------------------------------------------
summerInternsSource :: Identifier
summerInternsSource = "content/interns/interns.md"

data SummerIntern = SummerIntern
  { summerInternYear :: Int
  , summerInternName :: String
  , summerInternProject :: String
  , summerInternAbstract :: Maybe FilePath
  , summerInternOrder :: Int
  }

summerInternCtx :: Context SummerIntern
summerInternCtx =
  field "year" (return . show . summerInternYear . itemBody) `mappend`
  field "name" (return . summerInternName . itemBody) `mappend`
  field "project" (return . summerInternProject . itemBody) `mappend`
  field "abstractUrl" (\item ->
    case summerInternAbstract (itemBody item) of
      Just pdf -> return $ "/pdf/" ++ pdf
      Nothing  -> noResult "summer intern has no abstract"
  )

loadSummerInterns :: Identifier -> Compiler [Item SummerIntern]
loadSummerInterns source = do
  metadata <- getMetadata source
  case lookupString "interns" metadata of
    Nothing ->
      fail $ "Missing 'interns' metadata in " ++ toFilePath source
    Just rawInterns ->
      mapM makeItem $
      sortOn (\intern -> (Down (summerInternYear intern), summerInternOrder intern)) $
      zipWith parseSummerIntern [0 ..] $
      filter (not . null) $
      map trimWhitespace $
      lines rawInterns

parseSummerIntern :: Int -> String -> SummerIntern
parseSummerIntern order line =
  case map trimWhitespace (splitOn "||" line) of
    [yearText, name, project, pdfText] ->
      SummerIntern
        { summerInternYear = parseYear yearText
        , summerInternName = name
        , summerInternProject = project
        , summerInternAbstract =
            case trimWhitespace pdfText of
              "" -> Nothing
              pdf -> Just pdf
        , summerInternOrder = order
        }
    _ ->
      error $ "Could not parse summer intern entry: " ++ line

parseYear :: String -> Int
parseYear yearText =
  case reads yearText of
    [(year, "")] -> year
    _            -> error $ "Could not parse summer intern year: " ++ yearText

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn delimiter input =
  go input
  where
    go remaining =
      case breakOn delimiter remaining of
        Just (before, after) -> before : go after
        Nothing              -> [remaining]

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

trimWhitespace :: String -> String
trimWhitespace = reverse . dropWhile isSpace . reverse . dropWhile isSpace

summerInternsPlaceholder :: String
summerInternsPlaceholder = "<!--SUMMER_INTERNS-->"

renderSummerInterns :: [Item SummerIntern] -> Compiler String
renderSummerInterns interns =
  itemBody <$>
  ( makeItem ""
      >>= loadAndApplyTemplate
            "templates/summer-interns.html"
            (listField "interns" summerInternCtx (return interns) `mappend` defaultContext)
  )

replaceSummerInternsPlaceholder :: [Item SummerIntern] -> Item String -> Compiler (Item String)
replaceSummerInternsPlaceholder interns item = do
  renderedInterns <- renderSummerInterns interns
  return $ fmap (replaceLiteral summerInternsPlaceholder renderedInterns) item

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

--------------------------------------------------------------------------------
publishedBibPath :: FilePath
publishedBibPath = "assets/bib/published.bib"

publicationsPlaceholder :: String
publicationsPlaceholder = "<!--PUBLICATIONS-->"

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

loadPublications :: FilePath -> Compiler [Publication]
loadPublications bibPath =
  sortOn (\publication -> (Down (publicationYear publication), publicationOrder publication))
    . zipWith bibEntryToPublication [0 ..]
    . parseBibEntries
    <$> unsafeCompiler (readFile bibPath)

replacePublicationsPlaceholder :: [Publication] -> Item String -> Compiler (Item String)
replacePublicationsPlaceholder publications item =
  pure $ fmap (replaceLiteral publicationsPlaceholder (renderPublications publications)) item

bibEntryToPublication :: Int -> BibEntry -> Publication
bibEntryToPublication order entry =
  Publication
    { publicationTitle = normalizeBibText $ fieldOrEmpty "title"
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

renderPublications :: [Publication] -> String
renderPublications publications =
  "<div class=\"publications-list\">"
    ++ concatMap renderPublication publications
    ++ "</div>"

renderPublication :: Publication -> String
renderPublication publication =
  "<article class=\"publication-entry\">"
    ++ "<div class=\"publication-year\">" ++ escapePublicationHtml (show (publicationYear publication)) ++ "</div>"
    ++ "<h4 class=\"publication-title\">" ++ escapePublicationHtml (publicationTitle publication) ++ "</h4>"
    ++ renderPublicationLine "publication-authors" (formatAuthorList $ publicationAuthors publication)
    ++ renderPublicationLine "publication-meta" (formatVenueAndYear publication)
    ++ renderPublicationLinks publication
    ++ renderPublicationAbstract publication
    ++ "</article>"

renderPublicationLine :: String -> String -> String
renderPublicationLine className value
  | null value = ""
  | otherwise =
      "<p class=\"" ++ className ++ "\">" ++ escapePublicationHtml value ++ "</p>"

renderPublicationLinks :: Publication -> String
renderPublicationLinks publication =
  case links of
    [] -> ""
    _  ->
      "<div class=\"publication-links\">"
        ++ concatMap renderLink links
        ++ "</div>"
  where
    links = concat
      [ maybeToList ((\url -> ("DOI", url)) <$> publicationDoi publication)
      , maybeToList ((\url -> ("Slides", url)) <$> publicationSlideUrl publication)
      , maybeToList ((\url -> ("Preprint", url)) <$> publicationPreprintUrl publication)
      ]

    renderLink (label, url) =
      "<a class=\"publication-link\" href=\""
        ++ escapePublicationHtml url
        ++ "\">"
        ++ escapePublicationHtml label
        ++ "</a>"

renderPublicationAbstract :: Publication -> String
renderPublicationAbstract publication =
  case publicationAbstract publication of
    Nothing -> ""
    Just abstractText ->
      "<details class=\"publication-abstract\">"
        ++ "<summary>Abstract</summary>"
        ++ "<p>"
        ++ escapePublicationHtml abstractText
        ++ "</p>"
        ++ "</details>"

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
takeBalancedDelimited open close = go 1 []
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
takeBalancedQuotes = go False 0 []
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
splitTopLevelComma = go 0 False []
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

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" `mappend`
  defaultContext `mappend`
  field "category" (\it -> do
    let paths = drop 2 $ splitPath $ takeDirectory $ toFilePath $ itemIdentifier it
    if null paths
      then noResult "no category name is found"
      else return (joinPath paths))

postURL :: Routes
postURL = customRoute $ \id' ->
  let base = takeBaseName $ toFilePath id'
      (date, title) = splitAt 3 $ splitAll "-" base
   in joinPath $ "posts" : date ++ [intercalate "-" title]

--------------------------------------------------------------------------------
ropt :: ReaderOptions
ropt =
  defaultHakyllReaderOptions
    { -- The following option enables citation rendering
      readerExtensions = extensionsFromList
        [ Ext_superscript
        , Ext_subscript
        , Ext_citations 
        , Ext_grid_tables 
        ] <> readerExtensions defaultHakyllReaderOptions
    }

wopt :: WriterOptions
wopt =
  defaultHakyllWriterOptions
    { writerHTMLMathMethod = MathJax ""
    }

pandocBiblioCompiler :: String -> String -> Compiler (Item String)
pandocBiblioCompiler cslFileName bibFileName = do
  csl <- load $ fromFilePath cslFileName
  bibs <- loadAll $ fromGlob bibFileName
  writePandocWith wopt
    <$> (getResourceBody >>= readPandocBiblios ropt csl bibs)

loadAndApplyTemplates :: Foldable t => Context String -> t Identifier -> Item String -> Compiler (Item String)
loadAndApplyTemplates ctx ids it =
    foldM (\item tpl -> loadAndApplyTemplate tpl ctx item) it ids
