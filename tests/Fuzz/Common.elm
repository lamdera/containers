module Fuzz.Common exposing (expectEqual)

import Dict as CoreDict
import Expect exposing (Expectation)
import OrderedDict as Dict exposing (OrderedDict)


expectEqual : OrderedDict comparable v -> OrderedDict comparable v -> Expectation
expectEqual expected actual =
    actual
        |> Dict.toList
        |> CoreDict.fromList
        |> Expect.equalDicts (CoreDict.fromList <| Dict.toList expected)
