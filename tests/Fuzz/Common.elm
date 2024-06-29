module Fuzz.Common exposing (expectEqual)

import Dict as CoreDict
import Expect exposing (Expectation)
import SeqDict as Dict exposing (SeqDict)


expectEqual : SeqDict comparable v -> SeqDict comparable v -> Expectation
expectEqual expected actual =
    actual
        |> Dict.toList
        |> CoreDict.fromList
        |> Expect.equalDicts (CoreDict.fromList <| Dict.toList expected)
