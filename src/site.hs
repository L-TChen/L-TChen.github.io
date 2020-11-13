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

    match "content/cv.md" $ do
        route   $ gsubRoute "content/" (const "") `composeRoutes` setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "content/publications.md" $ do
        route   $ gsubRoute "content/" (const "") `composeRoutes` setExtension "html"
        compile $ pandocBiblioCompiler "assets/csl/elsevier-with-titles.csl" "assets/bib/published.bib"
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls
{-
    match "content/posts/*" $ do
        route $ gsubRoute "content/" (const "/")
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
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
                >>= loadAndApplyTemplate "templates/posts.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls
-}

    match "content/index.html" $ do
        route $ gsubRoute "content/" (const "")
        compile $ do
            --posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
            --        listField "posts" postCtx (return posts) `mappend`
                    constField "title" "About"               `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
