module BibTeX
  ( BibEntry (..),
    breakOn,
    firstJust,
    lookupBibField,
    maybeToList,
    nonEmpty,
    normalizeBibText,
    parseBibEntries,
    prefixOf,
    splitOn,
    takeBalancedBraces,
    trimWhitespace,
  )
where

import Data.Char (isAlphaNum, isSpace)

data BibEntry = BibEntry
  { bibEntryFields :: [(String, String)]
  }

lookupBibField :: String -> BibEntry -> Maybe String
lookupBibField fieldName = lookup (map toLowerAscii fieldName) . bibEntryFields

parseBibEntries :: String -> [BibEntry]
parseBibEntries input =
  case dropWhile (/= '@') input of
    [] -> []
    '@' : rest ->
      let (_, afterType) = span isBibTypeChar (dropWhile isSpace rest)
       in case dropWhile isSpace afterType of
            '{' : remaining ->
              let (entryBody, restInput) = takeBalancedBraces remaining
               in prependMaybe (parseBibEntry entryBody) (parseBibEntries restInput)
            '(' : remaining ->
              let (entryBody, restInput) = takeBalancedParens remaining
               in prependMaybe (parseBibEntry entryBody) (parseBibEntries restInput)
            _ -> parseBibEntries rest
    _ -> []
  where
    isBibTypeChar character = isAlphaNum character || character `elem` ("-_:" :: String)

parseBibEntry :: String -> Maybe BibEntry
parseBibEntry entryBody = do
  (_, fieldsBody) <- splitTopLevelComma entryBody
  pure BibEntry {bibEntryFields = parseBibFields fieldsBody}

parseBibFields :: String -> [(String, String)]
parseBibFields = go . dropSeparators
  where
    go [] = []
    go remaining =
      let (rawFieldName, afterFieldName) = span isFieldNameChar remaining
          fieldName = map toLowerAscii $ trimWhitespace rawFieldName
          afterEquals =
            case dropWhile isSpace afterFieldName of
              '=' : rest -> dropWhile isSpace rest
              rest -> rest
       in if null fieldName
            then []
            else
              case parseBibValue afterEquals of
                Nothing -> []
                Just (value, rest) ->
                  (fieldName, value) : go (dropSeparators rest)

    isFieldNameChar character = isAlphaNum character || character `elem` ("-_:" :: String)
    dropSeparators = dropWhile (\character -> isSpace character || character == ',')

parseBibValue :: String -> Maybe (String, String)
parseBibValue [] = Nothing
parseBibValue ('{' : remaining) =
  let (value, rest) = takeBalancedBraces remaining
   in Just (value, rest)
parseBibValue ('"' : remaining) =
  let (value, rest) = takeBalancedQuotes remaining
   in Just (value, rest)
parseBibValue remaining =
  let (value, rest) = span (\character -> character /= ',' && character /= '\n' && character /= '\r') remaining
   in Just (trimWhitespace value, rest)

takeBalancedBraces :: String -> (String, String)
takeBalancedBraces = takeBalancedDelimited '{' '}'

takeBalancedParens :: String -> (String, String)
takeBalancedParens = takeBalancedDelimited '(' ')'

takeBalancedDelimited :: Char -> Char -> String -> (String, String)
takeBalancedDelimited open close = go (1 :: Int) []
  where
    go _ acc [] = (reverse acc, [])
    go depth acc (character : remaining)
      | character == open = go (depth + 1) (character : acc) remaining
      | character == close =
          if depth == 1
            then (reverse acc, remaining)
            else go (depth - 1) (character : acc) remaining
      | otherwise = go depth (character : acc) remaining

takeBalancedQuotes :: String -> (String, String)
takeBalancedQuotes = go False (0 :: Int) []
  where
    go _ _ acc [] = (reverse acc, [])
    go escaped braceDepth acc (character : remaining)
      | escaped = go False braceDepth (character : acc) remaining
      | character == '\\' = go True braceDepth (character : acc) remaining
      | character == '{' = go False (braceDepth + 1) (character : acc) remaining
      | character == '}' && braceDepth > 0 = go False (braceDepth - 1) (character : acc) remaining
      | character == '"' && braceDepth == 0 = (reverse acc, remaining)
      | otherwise = go False braceDepth (character : acc) remaining

splitTopLevelComma :: String -> Maybe (String, String)
splitTopLevelComma = go (0 :: Int) False []
  where
    go _ _ _ [] = Nothing
    go braceDepth inQuotes acc (character : remaining)
      | character == '"' = go braceDepth (not inQuotes) (character : acc) remaining
      | not inQuotes && character == '{' = go (braceDepth + 1) False (character : acc) remaining
      | not inQuotes && character == '}' && braceDepth > 0 = go (braceDepth - 1) False (character : acc) remaining
      | not inQuotes && braceDepth == 0 && character == ',' = Just (reverse acc, remaining)
      | otherwise = go braceDepth inQuotes (character : acc) remaining

normalizeBibText :: String -> String
normalizeBibText =
  unwords
    . words
    . stripSimpleBraces
    . replaceAllLiterals bibTextReplacements
  where
    bibTextReplacements =
      [ ("\\'{a}", "á")
      , ("\\'{A}", "Á")
      , ("\\'{e}", "é")
      , ("\\'{E}", "É")
      , ("\\'{i}", "í")
      , ("\\'{I}", "Í")
      , ("\\'{o}", "ó")
      , ("\\'{O}", "Ó")
      , ("\\'{u}", "ú")
      , ("\\'{U}", "Ú")
      , ("\\'{\\i}", "í")
      , ("\\\"{a}", "ä")
      , ("\\\"{A}", "Ä")
      , ("\\\"{o}", "ö")
      , ("\\\"{O}", "Ö")
      , ("\\\"{u}", "ü")
      , ("\\\"{U}", "Ü")
      , ("\\v{c}", "č")
      , ("\\v{C}", "Č")
      , ("\\v{r}", "ř")
      , ("\\v{R}", "Ř")
      , ("\\c{t}", "ţ")
      , ("\\c{T}", "Ţ")
      , ("\\&", "&")
      , ("~", " ")
      ]

stripSimpleBraces :: String -> String
stripSimpleBraces [] = []
stripSimpleBraces (character : remaining)
  | character == '{' || character == '}' = stripSimpleBraces remaining
  | otherwise = character : stripSimpleBraces remaining

replaceAllLiterals :: [(String, String)] -> String -> String
replaceAllLiterals replacements input =
  foldl (\value (needle, replacement) -> replaceLiteral needle replacement value) input replacements

replaceLiteral :: Eq a => [a] -> [a] -> [a] -> [a]
replaceLiteral needle replacement = go
  where
    go haystack
      | needle `isPrefixOf` haystack = replacement ++ go (drop (length needle) haystack)
      | otherwise =
          case haystack of
            [] -> []
            x : xs -> x : go xs

    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys

breakOn :: Eq a => [a] -> [a] -> Maybe ([a], [a])
breakOn needle haystack = search [] haystack
  where
    search _ [] = Nothing
    search acc rest
      | needle `isPrefixOf` rest = Just (reverse acc, drop (length needle) rest)
      | otherwise =
          case rest of
            x : xs -> search (x : acc) xs

    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn delimiter input =
  go input
  where
    go remaining =
      case breakOn delimiter remaining of
        Just (before, after) -> before : go after
        Nothing -> [remaining]

trimWhitespace :: String -> String
trimWhitespace = reverse . dropWhile isSpace . reverse . dropWhile isSpace

nonEmpty :: String -> Maybe String
nonEmpty value
  | null (trimWhitespace value) = Nothing
  | otherwise = Just value

prefixOf :: Eq a => [a] -> [a] -> Bool
prefixOf [] _ = True
prefixOf _ [] = False
prefixOf (needleHead : needleTail) (haystackHead : haystackTail) =
  needleHead == haystackHead && prefixOf needleTail haystackTail

prependMaybe :: Maybe a -> [a] -> [a]
prependMaybe Nothing values = values
prependMaybe (Just value) values = value : values

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Nothing : remaining) = firstJust remaining
firstJust (Just value : _) = Just value

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just value) = [value]

toLowerAscii :: Char -> Char
toLowerAscii character
  | 'A' <= character && character <= 'Z' = toEnum (fromEnum character + 32)
  | otherwise = character
