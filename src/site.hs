--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (foldM)
import Data.List (intercalate)
import Data.Time.Format (defaultTimeLocale, formatTime)
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
import Forester
  ( ForesterPost (..)
  , deduplicatePostTags
  , loadForesterPosts
  , normaliseTagValue
  )
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
  foresterOutputDependency <- makePatternDependency KindContent foresterOutputPattern

  match foresterManifestPattern $ do
    route $ gsubRoute "forest/output/" (const "")
    compile getResourceBody

  -- Keep a textual, non-routed version for the adapter.  The normal version
  -- below remains an opaque CopyFile so Forester's XML is deployed verbatim.
  match foresterXmlPattern $ version "metadata" $ compile getResourceBody

  match
    ( foresterOutputPattern
        .&&. complement foresterManifestPattern
        .&&. complement foresterDefaultXslPattern
    ) $ do
    route $ gsubRoute "forest/output/" (const "")
    compile copyFileCompiler

  match "forest/site-theme/default.xsl" $ do
    route $ constRoute "posts/default.xsl"
    compile copyFileCompiler

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
  rulesExtraDependencies [scssDependency] $ do
    match "assets/scss/default.scss" $ do
      route $ setExtension "css" `composeRoutes` gsubRoute "assets/scss/" (const "css/")
      compile (fmap compressCss <$> sassCompiler)

    match "assets/scss/forester.scss" $ do
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
    , foresterOutputDependency
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

  rulesExtraDependencies
    [ summerInternsDependency
    , summerInternsTemplateDependency
    ] $ do
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

  rulesExtraDependencies [foresterOutputDependency] $ do
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
    defaultTemplate = "templates/default.html" : baseTemplate

foresterManifestPattern :: Pattern
foresterManifestPattern = "forest/output/posts/forest.json"

foresterOutputPattern :: Pattern
foresterOutputPattern = "forest/output/posts/**"

foresterDefaultXslPattern :: Pattern
foresterDefaultXslPattern = "forest/output/posts/default.xsl"

foresterXmlPattern :: Pattern
foresterXmlPattern = "forest/output/posts/**/index.xml"

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

loadPosts :: Compiler [Item ForesterPost]
loadPosts = loadForesterPosts (fromFilePath "forest/output/posts/forest.json") foresterXmlPattern

loadPostTagItems :: [Item ForesterPost] -> Compiler [Item PostTag]
loadPostTagItems posts = do
  let tagNames = concatMap (foresterPostTags . itemBody) posts
  mapM makeItem $
    map (\label -> PostTag label (normaliseTagValue label)) $
    deduplicatePostTags tagNames

renderPostTags :: Item ForesterPost -> Compiler String
renderPostTags =
  return . intercalate "\n" . map renderPostTagLink . foresterPostTags . itemBody

renderPostTagLink :: String -> String
renderPostTagLink tag =
  "<a class=\"badge rounded-pill text-bg-secondary text-decoration-none\" href=\"/posts.html?tag=" ++ normaliseTagValue tag ++ "\">" ++ tag ++ "</a>"

postCtx :: Context ForesterPost
postCtx =
  field "title" (return . foresterPostTitle . itemBody) `mappend`
  field "url" (return . foresterPostUrl . itemBody) `mappend`
  field "date" (return . formatTime defaultTimeLocale "%B %e, %Y" . foresterPostDate . itemBody) `mappend`
  field "taxon" (return . foresterPostTaxon . itemBody) `mappend`
  field "tags" renderPostTags `mappend`
  field "tagFilter" (return . intercalate "|" . map normaliseTagValue . foresterPostTags . itemBody)

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
