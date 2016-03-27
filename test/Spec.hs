import Test.Tasty

import Web.Templar.PatternTest (patternTests)

main :: IO ()
main = defaultMain allTests

allTests =
    testGroup "All Tests"
        [ patternTests
        ]
