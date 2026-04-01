module SummerInterns
  ( loadSummerInternsPageCtx,
    summerInternsBibPath,
  )
where

import Data.List (sortOn)
import Data.Ord (Down (..))

import BibTeX (BibEntry, lookupBibField, nonEmpty, normalizeBibText, parseBibEntries)
import Hakyll
  ( Compiler,
    Context,
    Identifier,
    Item (..),
    constField,
    field,
    getMetadata,
    listField,
    lookupString,
    makeItem,
    noResult,
    toFilePath,
    unsafeCompiler,
  )

summerInternsBibPath :: FilePath
summerInternsBibPath = "assets/bib/interns.bib"

data SummerIntern = SummerIntern
  { summerInternYear :: Int
  , summerInternName :: String
  , summerInternProject :: String
  , summerInternAbstract :: Maybe FilePath
  , summerInternOrder :: Int
  }

summerInternCtx :: Context SummerIntern
summerInternCtx =
  field "year" (return . show . summerInternYear . itemBody)
    <> field "name" (return . summerInternName . itemBody)
    <> field "project" (return . summerInternProject . itemBody)
    <> field "abstractUrl" (\item ->
      case summerInternAbstract (itemBody item) of
        Just pdf -> return $ "/pdf/" ++ pdf
        Nothing -> noResult "summer intern has no abstract"
    )

loadSummerInternsPageCtx :: Identifier -> Compiler (Context String)
loadSummerInternsPageCtx page = do
  limit <- loadPageListLimit "interns-limit" page
  interns <- loadSummerInterns summerInternsBibPath
  let boundedInterns = maybe interns (`take` interns) limit
  return $
    listField "interns" summerInternCtx (return boundedInterns)
      <> constField "internsUrl" "/interns.html"

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

loadSummerInterns :: FilePath -> Compiler [Item SummerIntern]
loadSummerInterns bibPath =
  mapM makeItem
    =<< ( sortOn (\intern -> (Down (summerInternYear intern), summerInternOrder intern))
            . zipWith bibEntryToSummerIntern [0 ..]
            . parseBibEntries
            <$> unsafeCompiler (readFile bibPath)
        )

bibEntryToSummerIntern :: Int -> BibEntry -> SummerIntern
bibEntryToSummerIntern order entry =
  SummerIntern
    { summerInternYear = parseYear $ fieldOrFail "year"
    , summerInternName = normalizeBibText $ fieldOrFail "author"
    , summerInternProject = normalizeBibText $ fieldOrFail "title"
    , summerInternAbstract = nonEmpty =<< lookupBibField "pdf" entry
    , summerInternOrder = order
    }
  where
    fieldOrFail fieldName =
      case lookupBibField fieldName entry of
        Just value -> value
        Nothing ->
          error $
            "Missing '" ++ fieldName ++ "' field in " ++ summerInternsBibPath

parseYear :: String -> Int
parseYear yearText =
  case reads yearText of
    [(year, "")] -> year
    _ -> error $ "Could not parse summer intern year: " ++ yearText
