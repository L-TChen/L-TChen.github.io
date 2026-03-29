--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (foldM)
import Data.Char (isSpace)
import Data.List (intercalate, sortOn)
import Data.Ord (Down (..))

import System.FilePath
  ( joinPath,
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
import Hakyll.Core.Dependencies (DependencyKind (KindContent))
import Hakyll.Web.Sass ( sassCompiler )
import Publications (replacePublicationsFromBib)
--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  summerInternsDependency <- makePatternDependency KindContent "content/interns/interns.md"
  summerInternsTemplateDependency <- makePatternDependency KindContent "templates/summer-interns.html"

  match "assets/html/**" $ do
    route $ gsubRoute "assets/html/" (const "")
    compile copyFileCompiler

  match "assets/img/*" $ do
    route $ gsubRoute "assets/" (const "")
    compile copyFileCompiler

  match "content/interns/pdf/*.pdf" $ do
    route $ gsubRoute "content/interns/" (const "")
    compile copyFileCompiler

  scssDependency <- makePatternDependency KindContent "bootstrap/package.json"
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
        pandocBiblioCompiler "assets/csl/elsevier-with-titles.csl" "assets/bib/*.bib"
          >>= replaceSummerInternsPlaceholder recentInterns
          >>= replacePublicationsFromBib
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
