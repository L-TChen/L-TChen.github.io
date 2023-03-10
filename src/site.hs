--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll hiding (pandocBiblioCompiler)

import           Control.Monad                 (liftM)
import           Text.Pandoc                   (Extension (..), Pandoc,
                                                ReaderOptions (..),
                                                WriterOptions (..),
                                                enableExtension,
                                                writerHTMLMathMethod,
                                                HTMLMathMethod(..))
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

  match "content/*.md" $ do
      route $ gsubRoute "content/" (const "") `composeRoutes` setExtension "html"
      let indexCtx = defaultContext
      compile $ pandocBiblioCompiler "assets/csl/elsevier-with-titles.csl" "assets/bib/*.bib"
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= loadAndApplyTemplate "templates/navbar.html" indexCtx
        >>= loadAndApplyTemplate "templates/head.html" indexCtx
        >>= relativizeUrls

  match "content/posts/**.md" $ do
      route $ gsubRoute "content/" (const "") `composeRoutes` setExtension "html"

      let indexCtx = defaultContext
      compile $ pandocBiblioCompiler "assets/csl/elsevier-with-titles.csl" "assets/bib/*.bib"
        >>= loadAndApplyTemplate "templates/post.html"   indexCtx
        >>= loadAndApplyTemplate "templates/navbar.html" indexCtx
        >>= loadAndApplyTemplate "templates/head.html"   indexCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

pandocBiblioCompiler :: String -> String -> Compiler (Item String)
pandocBiblioCompiler cslFileName bibFileName = do
  csl <- load $ fromFilePath cslFileName
  bibs <- loadAll $ fromGlob bibFileName
  liftM (writePandocWith wopt)
    (getResourceBody >>= readPandocBiblios ropt csl bibs)
  where
    ropt = defaultHakyllReaderOptions
      { -- The following option enables citation rendering
        readerExtensions = enableExtension Ext_citations $ readerExtensions defaultHakyllReaderOptions
      }
    wopt = defaultHakyllWriterOptions
      {
        writerHTMLMathMethod = MathJax "" 
      }
  
