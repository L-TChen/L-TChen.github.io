module Main (main) where

import Data.List (isInfixOf)
import Data.Time.Calendar (fromGregorian)
import Forester
  ( ForesterPost (..)
  , decodeForesterPosts
  , deduplicatePostTags
  , normaliseTagValue
  )
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "decodeForesterPosts" $ do
    it "joins published manifest entries to XML and uses the earliest date" $ do
      let result = decodeForesterPosts (manifest [publishedEntry "first" "/posts/first/" "A title" "Blog post" ["english"]]) [("first.xml", treeXml "first" "/posts/first/" ["2026-04-02", "2026-03-29"])]
      result `shouldBe`
        Right
          [ ForesterPost
              { foresterPostTitle = "A title"
              , foresterPostUrl = "/posts/first/"
              , foresterPostDate = fromGregorian 2026 3 29
              , foresterPostTaxon = "Blog post"
              , foresterPostTags = ["english"]
              }
          ]

    it "sorts published posts newest first" $ do
      let entries =
            [ publishedEntry "older" "/posts/older/" "Older" "Note" []
            , publishedEntry "newer" "/posts/newer/" "Newer" "Announcement" []
            ]
          xml =
            [ ("older.xml", treeXml "older" "/posts/older/" ["2025-01-01"])
            , ("newer.xml", treeXml "newer" "/posts/newer/" ["2026-01-01"])
            ]
      fmap (map foresterPostTitle) (decodeForesterPosts (manifest entries) xml)
        `shouldBe` Right ["Newer", "Older"]

    it "ignores entries without the explicit publish marker" $ do
      decodeForesterPosts (manifest [unpublishedEntry "draft"]) [("draft.xml", treeXml "draft" "/posts/draft/" [])]
        `shouldBe` Right []

    it "rejects invalid publish marker values" $ do
      let source = "[{\"title\":\"Draft\",\"uri\":\"draft\",\"taxon\":null,\"tags\":[],\"route\":\"/posts/draft/\",\"metas\":{\"site-publish\":\"yes\"}}]"
      decodeForesterPosts source [] `shouldFailWith` "Invalid site-publish value"

    it "rejects malformed manifests" $
      decodeForesterPosts "not JSON" [] `shouldFailWith` "Could not parse Forester manifest"

    it "requires title, taxon, and a complete date for published entries" $ do
      let missingTitle = publishedEntry "post" "/posts/post/" "" "Note" []
      let missingTaxon = "[{\"title\":\"Post\",\"uri\":\"post\",\"taxon\":null,\"tags\":[],\"route\":\"/posts/post/\",\"metas\":{\"site-publish\":\"true\"}}]"
      decodeForesterPosts (manifest [missingTitle]) [] `shouldFailWith` "missing title"
      decodeForesterPosts missingTaxon [] `shouldFailWith` "missing taxon"
      decodeForesterPosts (manifest [publishedEntry "post" "/posts/post/" "Post" "Note" []]) [("post.xml", treeXml "post" "/posts/post/" [])]
        `shouldFailWith` "no complete date"

    it "rejects missing XML, malformed XML, and unsafe routes" $ do
      let entry = publishedEntry "post" "/posts/post/" "Post" "Note" []
      decodeForesterPosts (manifest [entry]) [] `shouldFailWith` "No generated Forester XML"
      decodeForesterPosts (manifest [entry]) [("post.xml", "<not-xml")]
        `shouldFailWith` "Could not parse generated Forester XML"
      decodeForesterPosts (manifest [publishedEntry "post" "https://example.com/post" "Post" "Note" []]) [("post.xml", treeXml "post" "/posts/post/" ["2026-01-01"])]
        `shouldFailWith` "non-local or unsafe route"

    it "rejects manifest/XML identity mismatches" $ do
      let entry = publishedEntry "post" "/posts/post/" "Post" "Note" []
      decodeForesterPosts (manifest [entry]) [("post.xml", treeXml "post" "/posts/other/" ["2026-01-01"])]
        `shouldFailWith` "route mismatch"
      decodeForesterPosts (manifest [entry]) [("post.xml", treeXmlWithCanonical "post" "/posts/post/" "https://example.com/posts/post/" ["2026-01-01"])]
        `shouldFailWith` "canonical URI mismatch"

  describe "tag handling" $ do
    it "normalises labels for filtering" $
      normaliseTagValue " Type  Theory! " `shouldBe` "type-theory"

    it "deduplicates labels by their normalised value" $
      length (deduplicatePostTags ["English", "english", "type theory"]) `shouldBe` 2

shouldFailWith :: Either String a -> String -> Expectation
shouldFailWith result expected =
  case result of
    Left message -> message `shouldSatisfy` (expected `isInfixOf`)
    Right _ -> expectationFailure $ "Expected failure containing: " ++ expected

manifest :: [String] -> String
manifest entries = "[" ++ joinWithComma entries ++ "]"

publishedEntry :: String -> String -> String -> String -> [String] -> String
publishedEntry uri route title taxon tags =
  "{\"title\":\"" ++ title
    ++ "\",\"uri\":\"" ++ uri
    ++ "\",\"taxon\":\"" ++ taxon
    ++ "\",\"tags\":[" ++ joinWithComma (map quote tags)
    ++ "],\"route\":\"" ++ route
    ++ "\",\"metas\":{\"site-publish\":\"true\"}}"

unpublishedEntry :: String -> String
unpublishedEntry uri =
  "{\"title\":\"Draft\",\"uri\":\"" ++ uri
    ++ "\",\"taxon\":null,\"tags\":[],\"route\":\"/posts/" ++ uri
    ++ "/\",\"metas\":{}}"

treeXml :: String -> String -> [String] -> String
treeXml uri route = treeXmlWithCanonical uri route ("https://l-tchen.github.io" ++ route)

treeXmlWithCanonical :: String -> String -> String -> [String] -> String
treeXmlWithCanonical uri route canonical dates =
  "<?xml version=\"1.0\"?><fr:tree xmlns:fr=\"http://www.forester-notes.org\"><fr:frontmatter>"
    ++ concatMap xmlDate dates
    ++ "<fr:uri>" ++ canonical ++ "</fr:uri>"
    ++ "<fr:display-uri>" ++ uri ++ "</fr:display-uri>"
    ++ "<fr:route>" ++ route ++ "</fr:route>"
    ++ "</fr:frontmatter><fr:mainmatter/></fr:tree>"

xmlDate :: String -> String
xmlDate value =
  case splitOn '-' value of
    [year, month, day] ->
      "<fr:date><fr:year>" ++ year ++ "</fr:year><fr:month>" ++ month
        ++ "</fr:month><fr:day>" ++ day ++ "</fr:day></fr:date>"
    _ -> error "invalid date fixture"

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn delimiter value =
  case break (== delimiter) value of
    (prefix, []) -> [prefix]
    (prefix, _ : suffix) -> prefix : splitOn delimiter suffix

quote :: String -> String
quote value = "\"" ++ value ++ "\""

joinWithComma :: [String] -> String
joinWithComma [] = ""
joinWithComma (value : values) = value ++ concatMap (',' :) values
