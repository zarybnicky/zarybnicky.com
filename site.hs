{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Data.List (isSuffixOf)
import Hakyll
import System.FilePath ((</>), takeBaseName, takeDirectory)

hakyllConf :: Configuration
hakyllConf = defaultConfiguration
  { providerDirectory = "provider"
  }

main :: IO ()
main =
  hakyllWith hakyllConf $ do
    match "img/*" $ do
      route idRoute
      compile copyFileCompiler

    match "script/*" $ do
      route idRoute
      compile copyFileCompiler

    match "style/**" $ do
      route idRoute
      compile copyFileCompiler

    match (fromList ["about.org", "contact.org"]) $ do
      route (niceRoute `composeRoutes` setExtension "html")
      compile $
        pandocCompiler >>=
        loadAndApplyTemplate "templates/menu.html" archiveCtx >>=
        loadAndApplyTemplate "templates/default.html" defaultContext >>=
        cleanIndexUrls

    create ["archive.html"] $ do
      route niceRoute
      compile $ makeItem "" >>=
        loadAndApplyTemplate "templates/archive.html" archiveCtx >>=
        loadAndApplyTemplate "templates/menu.html" archiveCtx >>=
        loadAndApplyTemplate "templates/default.html" archiveCtx >>=
        cleanIndexUrls

    match "posts/*" $ do
      route niceRoute
      compile $
        pandocCompiler >>=
        loadAndApplyTemplate "templates/post.html" postCtx >>=
        loadAndApplyTemplate "templates/menu.html" archiveCtx >>=
        loadAndApplyTemplate "templates/default.html" postCtx >>=
        cleanIndexUrls

    match "index.html" $ do
      route idRoute
      compile $
        getResourceBody >>=
          applyAsTemplate indexCtx >>=
          loadAndApplyTemplate "templates/default.html" indexCtx >>=
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

postCtx :: Context String
postCtx = mconcat
  [ dateField "date" "%B %e, %Y"
  , defaultContext
  ]

indexCtx :: Context String
indexCtx = mconcat
  [ constField "title" "Home"
  , listField "posts" postCtx (fmap (take 3) . recentFirst =<< loadAll "posts/*")
  , defaultContext
  ]

archiveCtx :: Context String
archiveCtx = mconcat
  [ constField "title" "Archives"
  , listField "posts" postCtx (recentFirst =<< loadAll "posts/*")
  , defaultContext
  ]
