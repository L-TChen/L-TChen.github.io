--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (liftM, foldM)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.Monoid (mappend)
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
    Pandoc,
    ReaderOptions (..),
    WriterOptions (..),
    enableExtension,
    extensionsFromList,
    writerHTMLMathMethod,
  )
import System.Info (arch)

import Hakyll hiding (pandocBiblioCompiler)
import Hakyll.Web.Sass ( sassCompiler )
--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  match "assets/html/**" $ do
    route $ gsubRoute "assets/html/" (const "")
    compile copyFileCompiler

  match "assets/img/*" $ do
    route $ gsubRoute "assets/" (const "")
    compile copyFileCompiler

  scssDependency <- makePatternDependency "bootstrap/package.json"
  rulesExtraDependencies [scssDependency] $ match "assets/scss/default.scss" $ do
      route $ setExtension "css" `composeRoutes` gsubRoute "assets/scss/" (const "css/")
      compile (fmap compressCss <$> sassCompiler)

  match "assets/bib/*" $ compile biblioCompiler
  match "assets/csl/*" $ compile cslCompiler

  match "templates/*.html" $ compile templateBodyCompiler

  match "content/*.md" $ do
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