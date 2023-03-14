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
    writerHTMLMathMethod,
  )

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
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
      readerExtensions =
        enableExtension Ext_superscript $
          enableExtension Ext_subscript $
            enableExtension Ext_citations $
              enableExtension Ext_grid_tables $
                readerExtensions defaultHakyllReaderOptions
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
