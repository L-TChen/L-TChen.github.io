{-# LANGUAGE OverloadedStrings #-}

module ForesterStyle (stripLegacyForesterStyles) where

import Control.Monad (foldM)
import qualified Data.Text as T

stripLegacyForesterStyles :: T.Text -> Either String T.Text
stripLegacyForesterStyles css = foldM removeRule css legacyRules
  where
    removeRule currentCss (name, rule) =
      case T.splitOn rule currentCss of
        [before, after] -> Right $ before <> after
        [_] -> Left $ "Missing legacy Forester CSS rule: " ++ name
        _ -> Left $ "Duplicate legacy Forester CSS rule: " ++ name

legacyRules :: [(String, T.Text)]
legacyRules =
  [ ( "global vertical overflow clipping"
    , T.unlines
        [ "pre,"
        , "img,"
        , ".katex-display,"
        , "section,"
        , "center {"
        , "  overflow-y: hidden;"
        , "}"
        , ""
        ]
    )
  , ( "small-screen body margins"
    , T.unlines
        [ "  body {"
        , "    margin-top: 1em;"
        , "    margin-left: .5em;"
        , "    margin-right: .5em;"
        , "    transition: ease all .2s;"
        , "  }"
        , ""
        ]
    )
  , ( "large-screen body margins"
    , T.unlines
        [ "  body {"
        , "    margin-top: 2em;"
        , "    margin-left: 2em;"
        , "    transition: ease all .2s;"
        , "  }"
        , ""
        ]
    )
  , ( "table-of-contents outer margin"
    , T.unlines
        [ "nav#toc {"
        , "  margin-left: 1em;"
        , "}"
        , ""
        ]
    )
  , ( "automatic body hyphenation"
    , T.unlines
        [ "body {"
        , "  hyphens: auto;"
        , "}"
        , ""
        ]
    )
  , ( "global list bottom padding"
    , T.unlines
        [ "ul, ol {"
        , " padding-bottom: .5em;"
        , "}"
        , ""
        ]
    )
  ]
