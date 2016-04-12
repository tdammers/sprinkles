{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
module Web.Templar.PatternTest
where

import ClassyPrelude
import Web.Templar.Pattern
import Test.Tasty
import Test.Tasty.HUnit
import qualified Text.Regex.PCRE as RE

patternTests =
    testGroup "Web.Templar.Pattern"
        [ matchPatternTests
        , parsePatternTests
        ]

-- * Testing matchPattern

matchPatternTests = testGroup "matchPattern"
    [ matchPatternSingleExactlyTest
    , matchPatternSingleAnyOneTest
    , matchPatternSingleRegexTest
    , matchPatternCombinedExactlyAnyOneTest
    , matchFailExcessiveTest
    , matchRealWorldExampleTest1
    ]

matchPatternSingleExactlyTest =
    testCase "single Exactly" $ do
        let pattern = Pattern [PatternItem Nothing (Exactly "/hello")]
            query = "/hello"
            actual = matchPattern pattern query
            expected = Just $ mapFromList []
        assertEqual "" expected actual

matchPatternSingleAnyOneTest =
    testCase "single AnyOne" $ do
        let pattern = Pattern [PatternItem (Just "who") AnyOne]
            query = "hello"
            actual = matchPattern pattern query
            expected = Just $ mapFromList [("who", "hello")]
        assertEqual "" expected actual

matchPatternSingleRegexTest =
    testCase "single Regex" $ do
        let pattern = Pattern
                        [ PatternItem
                            (Just "who")
                            (Regex "hello, [a-z]*" RE.compBlank)
                        ]
            query = "hello, world"
            actual = matchPattern pattern query
            expected = Just $ mapFromList [("who", "hello, world")]
        assertEqual "" expected actual

matchPatternCombinedExactlyAnyOneTest =
    testCase "combined Exactly AnyOne" $ do
        let pattern = Pattern
                            [ PatternItem Nothing (Exactly "/")
                            , PatternItem (Just "who") AnyOne
                            , PatternItem Nothing (Exactly "/world")
                            ]
            query = "/hello/world"
            actual = matchPattern pattern query
            expected = Just $ mapFromList [("who", "hello")]
        assertEqual "" expected actual

matchFailExcessiveTest =
    testCase "combined Exactly AnyOne" $ do
        let pattern = Pattern
                            [ PatternItem Nothing (Exactly "/")
                            , PatternItem (Just "who") AnyOne
                            ]
            query = "/hello/"
            actual = matchPattern pattern query
            expected = Nothing
        assertEqual "" expected actual

matchRealWorldExampleTest1 =
    testCase "real world example 1" $ do
        let pattern = Pattern
                            [ PatternItem Nothing (Exactly "/country?")
                            , PatternItem (Just "query") Any
                            ]
            query = "/country?lang=de&country=DE&username=demo&style=full"
            actual = matchPattern pattern query
            expected = Just $ mapFromList [("query", "lang=de&country=DE&username=demo&style=full")]
        assertEqual "" expected actual

-- * Testing parsePattern

parsePatternTests =
    testGroup "parsePattern"
        [ parseSimpleExactlyTest
        , parseNamedExactlyTest
        , parseSimpleAnyOneTest
        , parseNamedAnyOneTest
        , parseNamedRegexTest
        , parseShorthandAnyOneTest
        , parseSimpleAnyTest
        , parseNamedAnyTest
        , parseShorthandAnyTest
        , parseRealWorldExampleTest1
        ]

parseSimpleExactlyTest =
    testCase "parse simple Exactly" $ do
        let src = "/hello"
            expected = Just $ Pattern
                        [ PatternItem Nothing (Exactly "/hello")
                        ]
            actual = parsePattern src
        assertEqual "" expected actual

parseNamedExactlyTest =
    testCase "parse named Exactly" $ do
        let src = "{{who:hello}}"
            expected = Just $ Pattern
                        [ PatternItem (Just "who") (Exactly "hello")
                        ]
            actual = parsePattern src
        assertEqual "" expected actual

parseSimpleAnyOneTest =
    testCase "parse simple AnyOne" $ do
        let src = "{{*}}"
            expected = Just $ Pattern
                        [ PatternItem Nothing AnyOne
                        ]
            actual = parsePattern src
        assertEqual "" expected actual

parseShorthandAnyOneTest =
    testCase "parse shorthand AnyOne" $ do
        let src = "*"
            expected = Just $ Pattern
                        [ PatternItem Nothing AnyOne
                        ]
            actual = parsePattern src
        assertEqual "" expected actual

parseNamedAnyOneTest =
    testCase "parse named AnyOne" $ do
        let src = "{{who:*}}"
            expected = Just $ Pattern
                        [ PatternItem (Just "who") AnyOne
                        ]
            actual = parsePattern src
        assertEqual "" expected actual

parseNamedRegexTest =
    testCase "parse named Regex" $ do
        let src = "{{who:/foo/}}"
            expected = Just $ Pattern
                        [ PatternItem (Just "who") (Regex "foo" RE.compBlank)
                        ]
            actual = parsePattern src
        assertEqual "" expected actual

parseSimpleAnyTest =
    testCase "parse simple Any" $ do
        let src = "{{**}}"
            expected = Just $ Pattern
                        [ PatternItem Nothing Any
                        ]
            actual = parsePattern src
        assertEqual "" expected actual

parseShorthandAnyTest =
    testCase "parse shorthand Any" $ do
        let src = "**"
            expected = Just $ Pattern
                        [ PatternItem Nothing Any
                        ]
            actual = parsePattern src
        assertEqual "" expected actual

parseNamedAnyTest =
    testCase "parse named Any" $ do
        let src = "{{who:**}}"
            expected = Just $ Pattern
                        [ PatternItem (Just "who") Any
                        ]
            actual = parsePattern src
        assertEqual "" expected actual

parseRealWorldExampleTest1 =
    testCase "parse real world example 1" $ do
        let src = "/country?{{query:*}}"
            expected = Just $ Pattern
                        [ PatternItem Nothing (Exactly "/country?")
                        , PatternItem (Just "query") AnyOne
                        ]
            actual = parsePattern src
        assertEqual "" expected actual

