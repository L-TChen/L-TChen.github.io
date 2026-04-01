--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (foldM)
import Data.Char (isAlphaNum, toLower)
import Data.List (intercalate, sortOn)

import BibTeX (splitOn, trimWhitespace)
import System.FilePath
  ( joinPath,
    splitDirectories,
    takeBaseName,
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
import Publications (loadPublicationsPageCtx)
import SummerInterns (loadSummerInternsPageCtx, summerInternsBibPath)
--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  summerInternsDependency <- makePatternDependency KindContent (fromGlob summerInternsBibPath)
  summerInternsTemplateDependency <- makePatternDependency KindContent "templates/summer-interns.html"
  publicationsDependency <- makePatternDependency KindContent "assets/bib/published.bib"
  publicationsTemplateDependency <- makePatternDependency KindContent "templates/publications.html"
  recentPostsTemplateDependency <- makePatternDependency KindContent "templates/recent-posts.html"

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

  rulesExtraDependencies
    [ summerInternsDependency
    , summerInternsTemplateDependency
    , publicationsDependency
    , publicationsTemplateDependency
    , recentPostsTemplateDependency
    ] $ do
    match "content/index.md" $ do
      route $ gsubRoute "content/" (const "") `composeRoutes` setExtension "html"
      compile $ do
        page <- getUnderlying
        summerInternsCtx <- loadSummerInternsPageCtx page
        publicationsCtx <- loadPublicationsPageCtx page
        postsCtx <- loadPostsPageCtx page
        let pageCtx = summerInternsCtx `mappend` publicationsCtx `mappend` postsCtx
        pandocBiblioTemplateCompiler pageCtx "assets/csl/elsevier-with-titles.csl" "assets/bib/*.bib"
          >>= loadAndApplyTemplates (pageCtx `mappend` defaultContext) defaultTemplate
          >>= relativizeUrls

    match "content/interns/interns.md" $ do
      route $ constRoute "interns.html"
      compile $ do
        pageCtx <- loadSummerInternsPageCtx =<< getUnderlying
        pandocBiblioTemplateCompiler pageCtx "assets/csl/elsevier-with-titles.csl" "assets/bib/*.bib"
          >>= loadAndApplyTemplates (pageCtx `mappend` defaultContext) defaultTemplate
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
      archiveCtx <- loadPostsArchiveCtx
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
            "Could not parse " ++ fieldName ++ " in " ++ toFilePath page ++ ": " ++ value

data PostTag = PostTag
  { postTagLabel :: String
  , postTagValue :: String
  }

postTagCtx :: Context PostTag
postTagCtx =
  field "label" (return . postTagLabel . itemBody) `mappend`
  field "value" (return . postTagValue . itemBody)

loadPostsPageCtx :: Identifier -> Compiler (Context String)
loadPostsPageCtx page = do
  limit <- loadPageListLimit "posts-limit" page
  posts <- loadPosts
  let boundedPosts = maybe posts (`take` posts) limit
  return $
    listField "posts" postCtx (return boundedPosts) `mappend`
    constField "postsUrl" "/posts.html"

loadPostsArchiveCtx :: Compiler (Context String)
loadPostsArchiveCtx = do
  posts <- loadPosts
  tags <- loadPostTagItems posts
  return $
    listField "posts" postCtx (return posts) `mappend`
    listField "tags" postTagCtx (return tags) `mappend`
    constField "title" "Posts" `mappend`
    defaultContext

loadPosts :: Compiler [Item String]
loadPosts = recentFirst =<< loadAll "content/posts/**"

loadPostTagItems :: [Item String] -> Compiler [Item PostTag]
loadPostTagItems posts = do
  tagNames <- concat <$> mapM postTags posts
  mapM makeItem $
    map (\label -> PostTag label (normaliseTagValue label)) $
    deduplicatePostTags tagNames

deduplicatePostTags :: [String] -> [String]
deduplicatePostTags =
  foldr keepFirst [] . sortOn normaliseTagValue
  where
    keepFirst label [] = [label]
    keepFirst label acc@(existing : _)
      | normaliseTagValue label == normaliseTagValue existing = acc
      | otherwise = label : acc

postTags :: Item a -> Compiler [String]
postTags item = do
  metadata <- getMetadata $ itemIdentifier item
  return $
    maybe [] parseTagList $ lookupString "tags" metadata

parseTagList :: String -> [String]
parseTagList =
  filter (not . null) .
  map trimWhitespace .
  splitOn ","

renderPostTags :: Item a -> Compiler String
renderPostTags item =
  intercalate "\n" . map renderPostTagLink <$> postTags item

renderPostTagLink :: String -> String
renderPostTagLink tag =
  "<a class=\"post-tag\" href=\"/posts.html?tag=" ++ normaliseTagValue tag ++ "\">" ++ tag ++ "</a>"

normaliseTagValue :: String -> String
normaliseTagValue =
  trimChar '-' .
  collapseRepeated '-' .
  map simplify
  where
    simplify c
      | isAlphaNum c = toLower c
      | otherwise = '-'

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

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" `mappend`
  field "category" (\it ->
    case postCategory (itemIdentifier it) of
      Just category -> return category
      Nothing       -> noResult "no category name is found"
  ) `mappend`
  field "tags" renderPostTags `mappend`
  field "tagFilter" (\it -> intercalate "|" . map normaliseTagValue <$> postTags it) `mappend`
  defaultContext

postCategory :: Identifier -> Maybe String
postCategory identifier =
  case dropWhile (/= "posts") $ splitDirectories $ toFilePath identifier of
    "posts" : category : _ -> Just category
    _                      -> Nothing

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

pandocBiblioTemplateCompiler :: Context String -> String -> String -> Compiler (Item String)
pandocBiblioTemplateCompiler ctx cslFileName bibFileName = do
  csl <- load $ fromFilePath cslFileName
  bibs <- loadAll $ fromGlob bibFileName
  markdown <- getResourceBody >>= applyAsTemplate ctx
  writePandocWith wopt
    <$> readPandocBiblios ropt csl bibs markdown

loadAndApplyTemplates :: Foldable t => Context String -> t Identifier -> Item String -> Compiler (Item String)
loadAndApplyTemplates ctx ids it =
    foldM (\item tpl -> loadAndApplyTemplate tpl ctx item) it ids
