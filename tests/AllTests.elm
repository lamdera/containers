module AllTests exposing (main)

import BiSeqDictTests
import MultiBiSeqDictTests
import MultiSeqDictTests
import Test exposing (Test, describe)
import Test.Runner.Html
import Tests


suite : Test
suite =
    describe "All Tests"
        [ Tests.tests
        , BiSeqDictTests.tests
        , MultiSeqDictTests.tests
        , MultiBiSeqDictTests.tests
        ]


main : Test.Runner.Html.TestProgram
main =
    Test.Runner.Html.run suite
