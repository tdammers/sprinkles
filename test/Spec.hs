import Test.Tasty

import Web.Templar.PatternTest (patternTests)
import Web.Templar.ApplicationTest (applicationTests)

main :: IO ()
main = defaultMain allTests

allTests =
    testGroup "All Tests"
        [ patternTests
        , applicationTests
        ]
