{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Applicative (Alternative, empty)
import Control.Arrow ((&&&), (>>>))
import Control.Monad ((>=>), foldM)
import Data.List (isSuffixOf, elemIndex)
import qualified Data.Map as Map
import Data.Monoid ((<>), getAlt)
import qualified Data.Set as Set
import Hakyll
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

    match "drafts/*" $ do
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

-- Hakyll.Web.Series:
getSeries :: MonadMetadata m => Identifier -> m (Maybe String)
getSeries = flip getMetadataField "series"

toAlt :: (Foldable f, Alternative m) => f a -> m a
toAlt = getAlt . foldMap pure

infixr 1 >->
(>->) :: Functor f =>  (a -> f b) -> (b -> c) -> a -> f c
f >-> g = f >>> fmap g

seriesField :: Tags -> Context a
seriesField tags =
  Context $
  const . \case
    "series" -> seriesName >-> StringField
    "seriesCurPos" ->
      itemIdentifier &&& otherPostsInSeries >>>sequence >>>
      fmap (uncurry elemIndex) >=> toAlt >-> succ >>> show >>> StringField
    "seriesLength" -> otherPostsInSeries >-> length >>> show >>> StringField
    "seriesUrl" ->
      seriesName >=>
      tagsMakeId tags >>> getRoute >=> toAlt >-> toUrl >>> StringField
    _ -> const empty
  where
    seriesName :: Item a -> Compiler String
    seriesName = itemIdentifier >>> getSeries >=> toAlt
    otherPostsInSeries = seriesName >=> flip lookup (tagsMap tags) >>> toAlt

buildSeries :: MonadMetadata m => Pattern -> (String -> Identifier) -> m Tags
buildSeries pattrn makeId = do
  ids <- getMatches pattrn
  tagMap <- foldM addTags Map.empty ids
  inOrder <- (traverse . traverse) sortChronological (Map.assocs tagMap)
  pure $ Tags inOrder makeId (PatternDependency pattrn $ Set.fromList ids)
  where
    addTags tagMap id' =
      maybe tagMap (\k -> Map.insertWith (++) k [id'] tagMap) <$> getSeries id'
