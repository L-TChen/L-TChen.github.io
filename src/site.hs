--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (liftM)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.Monoid (mappend)
import Data.Time.Format
import Hakyll hiding (pandocBiblioCompiler)
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

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  match "assets/html/**" $ do
    route $ gsubRoute "assets/html/" (const "")
    compile copyFileCompiler

  match "assets/img/*" $ do
    route $ gsubRoute "assets/" (const "")
    compile copyFileCompiler

  match "assets/css/*" $ do
    route $ gsubRoute "assets/" (const "")
    compile compressCssCompiler

  match "assets/bib/*" $ compile biblioCompiler
  match "assets/csl/*" $ compile cslCompiler

  match "templates/*.html" $ compile templateBodyCompiler

  match "content/*.md" $ do
    route $ gsubRoute "content/" (const "") `composeRoutes` setExtension "html"
    let indexCtx = defaultContext
    compile $
      pandocBiblioCompiler "assets/csl/elsevier-with-titles.csl" "assets/bib/*.bib"
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= loadAndApplyTemplate "templates/footer.html" indexCtx
        >>= loadAndApplyTemplate "templates/navbar.html" indexCtx
        >>= loadAndApplyTemplate "templates/head.html" indexCtx
        >>= relativizeUrls

  match "content/posts/**.md" $ do
    route $
      postURL `composeRoutes` setExtension "html"

    let indexCtx = postCtx
    compile $
      pandocBiblioCompiler "assets/csl/elsevier-with-titles.csl" "assets/bib/*.bib"
        >>= loadAndApplyTemplate "templates/post.html" indexCtx
        >>= loadAndApplyTemplate "templates/footer.html" indexCtx
        >>= loadAndApplyTemplate "templates/navbar.html" indexCtx
        >>= loadAndApplyTemplate "templates/head.html" indexCtx
        >>= relativizeUrls

  create ["posts.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let archiveCtx =
            listField "posts" postCtx (return posts) `mappend`
            constField "title" "Posts"               `mappend`
            defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/post-list.html" archiveCtx
        >>= loadAndApplyTemplate "templates/footer.html" archiveCtx
        >>= loadAndApplyTemplate "templates/navbar.html" archiveCtx
        >>= loadAndApplyTemplate "templates/head.html" archiveCtx
        >>= relativizeUrls

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
