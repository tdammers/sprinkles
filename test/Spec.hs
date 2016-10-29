import Test.Tasty

import Web.Sprinkles.PatternTest (patternTests)
import Web.Sprinkles.ApplicationTest (applicationTests)

main :: IO ()
main = defaultMain allTests

allTests =
    testGroup "All Tests"
        [ patternTests
        , applicationTests
        ]
