module Fuzz.Invariants exposing (respectsInvariants, respectsInvariantsFuzz)

import Expect
import Fuzz exposing (Fuzzer)
import SeqDict as Dict exposing (SeqDict)
import Test exposing (Test, describe, fuzz, test)


{-| Checks whether a dictionary respects one invariant:

1.  the cached size is the same as the length of `toList`

-}
respectsInvariants : SeqDict comparable value -> Test
respectsInvariants dict =
    describe "Respects the invariants"
        [ test "The cached size is correct" <|
            \_ ->
                dict
                    |> hasCorrectSize
                    |> Expect.equal True
        ]


{-| Checks whether a dictionary respects one invariant:

1.  the cached size is the same as the length of `toList`

-}
respectsInvariantsFuzz : Fuzzer (SeqDict comparable value) -> Test
respectsInvariantsFuzz fuzzer =
    describe "Respects the invariants"
        [ fuzz fuzzer "The cached size is correct" <|
            \dict ->
                dict
                    |> hasCorrectSize
                    |> Expect.equal True
        ]


hasCorrectSize : SeqDict comparable v -> Bool
hasCorrectSize dict =
    dict
        |> Dict.toList
        |> List.length
        |> (==) (Dict.size dict)
