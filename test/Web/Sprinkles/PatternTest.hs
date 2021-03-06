{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
module Web.Sprinkles.PatternTest
where

import Web.Sprinkles.Prelude hiding (Any)
import Web.Sprinkles.Pattern
import Test.Tasty
import Test.Tasty.HUnit
import qualified Text.Regex.PCRE as RE

patternTests =
    testGroup "Web.Sprinkles.Pattern"
        [ matchPatternTests
        , parsePatternTests
        ]

-- * Testing matchPattern

matchPatternTests = testGroup "matchPattern"
    [ matchPatternSingleExactlyTest
    , matchPatternSingleAnyTest
    , matchPatternSingleRegexTest
    , matchPatternCombinedExactlyAnyTest
    , matchFailExcessiveTest
    , matchRealWorldExampleTest1
    ]

matchPatternSingleExactlyTest =
    testCase "single Exactly" $ do
        let pattern = Pattern
                [PatternPathItem Nothing (Exactly "hello") MatchOne]
                []
            path = ["hello"]
            query = []
            actual = matchPattern pattern path query
            expected = Just $ mapFromList []
        assertEqual "" expected actual

matchPatternSingleAnyTest =
    testCase "single Any" $ do
        let pattern = Pattern [PatternPathItem (Just "who") Any MatchOne] []
            path = ["hello"]
            query = []
            actual = matchPattern pattern path query
            expected = Just $ mapFromList [("who", "hello")]
        assertEqual "" expected actual

matchPatternSingleRegexTest =
    testCase "single Regex" $ do
        let pattern = Pattern
                        [ PatternPathItem
                            (Just "who")
                            (Regex "hello, [a-z]*" RE.compBlank)
                            MatchOne
                        ]
                        []
            path = ["hello, world"]
            query = []
            actual = matchPattern pattern path query
            expected = Just $ mapFromList [("who", "hello, world")]
        assertEqual "" expected actual

matchPatternCombinedExactlyAnyTest =
    testCase "combined Exactly Any" $ do
        let pattern = Pattern
                            [ PatternPathItem Nothing (Exactly "hello") MatchOne
                            , PatternPathItem (Just "who") Any MatchOne
                            , PatternPathItem Nothing (Exactly "world") MatchOne
                            ]
                            []
            path = ["hello", "who", "world"]
            query = []
            actual = matchPattern pattern path query
            expected = Just $ mapFromList [("who", "who")]
        assertEqual "" expected actual

matchFailExcessiveTest =
    testCase "fail excessive" $ do
        let pattern = Pattern
                            [ PatternPathItem Nothing (Exactly "hello") MatchOne
                            , PatternPathItem (Just "who") Any MatchOne
                            ]
                            []
            path = ["hello", "world", "me"]
            query = []
            actual = matchPattern pattern path query
            expected = Nothing
        assertEqual "" expected actual

matchRealWorldExampleTest1 =
    testCase "real world example 1" $ do
        let pattern = Pattern
                            [ PatternPathItem Nothing (Exactly "country") MatchOne
                            ]
                            [ PatternQueryItem (Just "lang") "lang" Any True
                            ]
            path = ["country"]
            query =
                [ ("lang", Just "de")
                , ("country", Just "DE")
                , ("username", Just "demo")
                , ("style", Just "full")
                ]
            actual = matchPattern pattern path query
            expected = Just $ mapFromList [("lang", "de")]
        assertEqual "" expected actual

-- * Testing parsePattern

parsePatternTests =
    testGroup "parsePattern"
        [ parseSimpleExactlyTest
        , parseNamedExactlyTest
        , parseSimpleAnyTest
        , parseNamedAnyTest
        , parseNamedRegexTest
        , parseShorthandAnyTest
        , parseSimpleAnyMultiTest
        , parseNamedAnyMultiTest
        , parseShorthandAnyMultiTest
        , parseNamedWithUnderscore
        , parseRealWorldExampleTest1
        ]

parseSimpleExactlyTest =
    testCase "parse simple Exactly" $ do
        let src = "/hello"
            expected = Right $ Pattern
                        [ PatternPathItem Nothing (Exactly "hello") MatchOne
                        ]
                        []
            actual = parsePattern src
        assertEqual "" expected actual

parseNamedExactlyTest =
    testCase "parse named Exactly" $ do
        let src = "/{{who:hello}}"
            expected = Right $ Pattern
                        [ PatternPathItem (Just "who") (Exactly "hello") MatchOne
                        ]
                        []
            actual = parsePattern src
        assertEqual "" expected actual

parseSimpleAnyTest =
    testCase "parse simple Any" $ do
        let src = "/{{*}}"
            expected = Right $ Pattern
                        [ PatternPathItem Nothing Any MatchOne
                        ]
                        []
            actual = parsePattern src
        assertEqual "" expected actual

parseShorthandAnyTest =
    testCase "parse shorthand Any" $ do
        let src = "/*"
            expected = Right $ Pattern
                        [ PatternPathItem Nothing Any MatchOne
                        ]
                        []
            actual = parsePattern src
        assertEqual "" expected actual

parseNamedAnyTest =
    testCase "parse named Any" $ do
        let src = "/{{who:*}}"
            expected = Right $ Pattern
                        [ PatternPathItem (Just "who") Any MatchOne
                        ]
                        []
            actual = parsePattern src
        assertEqual "" expected actual

parseNamedRegexTest =
    testCase "parse named Regex" $ do
        let src = "/{{who:/foo\\./}}"
            expected = Right $ Pattern
                        [ PatternPathItem (Just "who") (Regex "foo\\." RE.compBlank) MatchOne
                        ]
                        []
            actual = parsePattern src
        assertEqual "" expected actual

parseSimpleAnyMultiTest =
    testCase "parse simple Any" $ do
        let src = "/{{**}}"
            expected = Right $ Pattern
                        [ PatternPathItem Nothing Any MatchMany
                        ]
                        []
            actual = parsePattern src
        assertEqual "" expected actual

parseShorthandAnyMultiTest =
    testCase "parse shorthand Any" $ do
        let src = "/**"
            expected = Right $ Pattern
                        [ PatternPathItem Nothing Any MatchMany
                        ]
                        []
            actual = parsePattern src
        assertEqual "" expected actual

parseNamedAnyMultiTest =
    testCase "parse named Any" $ do
        let src = "/{{who:**}}"
            expected = Right $ Pattern
                        [ PatternPathItem (Just "who") Any MatchMany
                        ]
                        []
            actual = parsePattern src
        assertEqual "" expected actual

parseNamedWithUnderscore =
    testCase "parse named Any" $ do
        let src = "/{{who_is:*}}"
            expected = Right $ Pattern
                        [ PatternPathItem (Just "who_is") Any MatchOne
                        ]
                        []
            actual = parsePattern src
        assertEqual "" expected actual


parseRealWorldExampleTest1 =
    testCase "parse real world example 1" $ do
        let src = "/country?query={{query:*}}"
            expected = Right $ Pattern
                        [ PatternPathItem Nothing (Exactly "country") MatchOne
                        ]
                        [ PatternQueryItem (Just "query") "query" Any True
                        ]
            actual = parsePattern src
        assertEqual "" expected actual

