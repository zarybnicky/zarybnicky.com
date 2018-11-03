{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Data.List (isSuffixOf)
import Data.Monoid ((<>))
import Hakyll
import Hakyll.Web.Series
import System.FilePath ((</>), takeBaseName, takeDirectory)

hakyllConf :: Configuration
hakyllConf = defaultConfiguration
  { providerDirectory = "provider"
  }

main :: IO ()
main =
  hakyllWith hakyllConf $ do
    series <- buildSeries "posts/*" (fromCapture "series/*.html")
    let postCtx = seriesField series <>
                  dateField "date" "%B %e, %Y" <>
                  defaultContext

    match "img/*" $ do
      route idRoute
      compile copyFileCompiler

    match "script/*" $ do
      route idRoute
      compile copyFileCompiler

    match "style/**" $ do
      route idRoute
      compile copyFileCompiler

    create ["archive.html"] $ do
      route niceRoute
      let ctx = constField "title" "Writings" <>
                listField "posts" postCtx (recentFirst =<< loadAll "posts/*") <>
                defaultContext
      compile $
        makeItem "" >>=
        loadAndApplyTemplate "templates/archive.html" ctx >>=
        loadAndApplyTemplate "templates/default.html" ctx >>=
        cleanIndexUrls

    create ["portfolio.html"] $ do
      route niceRoute
      let ctx = constField "title" "Portfolio" <> defaultContext
      compile $
        makeItem "" >>=
        loadAndApplyTemplate "templates/portfolio.html" ctx >>=
        loadAndApplyTemplate "templates/default.html" ctx >>=
        cleanIndexUrls

    tagsRules series $ \title pattrn -> do
      route $ gsubRoute "[' ]" (const "-") `composeRoutes` niceRoute
      let ctx = constField "title" title <>
                listField "posts" postCtx (chronological =<< loadAll pattrn) <>
                defaultContext
      compile $
        makeItem "" >>=
        loadAndApplyTemplate "templates/series.html" ctx >>=
        loadAndApplyTemplate "templates/default.html" ctx >>=
        cleanIndexUrls

    match "posts/*" $ do
      route niceRoute
      compile $
        (fmap demoteHeaders <$> pandocCompiler) >>=
        loadAndApplyTemplate "templates/post.html" postCtx >>=
        loadAndApplyTemplate "templates/default.html" postCtx >>=
        cleanIndexUrls

    match "index.html" $ do
      route idRoute
      let ctx = constField "title" "Home" <>
                listField "posts" postCtx (fmap (take 3) . recentFirst =<< loadAll "posts/*") <>
                defaultContext
      compile $
        getResourceBody >>=
        applyAsTemplate ctx >>=
        loadAndApplyTemplate "templates/default.html" ctx >>=
        cleanIndexUrls

    match "templates/*" $ compile templateCompiler


niceRoute :: Routes
niceRoute = customRoute createIndexRoute
  where
    createIndexRoute (toFilePath -> p) =
      takeDirectory p </> takeBaseName p </> "index.html"

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls clean)
  where
    idx = "index.html" :: String
    clean url
      | idx `isSuffixOf` url = take (length url - length idx) url
      | otherwise = url
