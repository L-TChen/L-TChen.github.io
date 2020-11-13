--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Hakyll.Web.Pandoc.Biblio
--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  match "assets/images/*" $ do
      route $ gsubRoute "assets/" (const "")
      compile copyFileCompiler

  match "assets/css/*" $ do
      route $ gsubRoute "assets/" (const "")
      compile compressCssCompiler

  match "assets/bib/*" $ compile biblioCompiler
  match "assets/csl/*" $ compile cslCompiler

  match "content/publications.md" $ do
      route   $ gsubRoute "content/" (const "") `composeRoutes` setExtension "html"
      compile $ pandocBiblioCompiler "assets/csl/elsevier-with-titles.csl" "assets/bib/published.bib"
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= loadAndApplyTemplate "templates/navbar.html" defaultContext
        >>= loadAndApplyTemplate "templates/head.html"  defaultContext
        >>= relativizeUrls

  match "content/**.md" $ do
      route $ gsubRoute "content/" (const "") `composeRoutes` setExtension "html"
      let indexCtx = defaultContext
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= loadAndApplyTemplate "templates/navbar.html" indexCtx
        >>= loadAndApplyTemplate "templates/head.html" indexCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
