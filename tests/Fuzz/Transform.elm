module Fuzz.Transform exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import Fuzz.Common exposing (expectEqual)
import Fuzz.Fuzzers exposing (Key, Value, dictFuzzer)
import Fuzz.Invariants exposing (respectsInvariantsFuzz)
import SeqDict as Dict exposing (SeqDict)
import Test exposing (Test, describe, fuzz)


suite : Test
suite =
    describe "transform"
        [ mapTest
        , foldlTest
        , foldrTest
        , filterTest
        , filterMapTest
        , partitionTest
        ]


mapTest : Test
mapTest =
    let
        f1 k v =
            k ++ " " ++ String.fromInt v

        f2 _ v =
            String.fromInt <| v + 1

        tests =
            [ ( "f1", f1 )
            , ( "f2", f2 )
            ]
                |> List.map
                    (\( flabel, f ) ->
                        [ fuzz dictFuzzer "Is equivalent to mapping on the list" <|
                            \dict ->
                                dict
                                    |> Dict.map f
                                    |> expectEqual
                                        (dict
                                            |> Dict.toList
                                            |> List.map (\( k, v ) -> ( k, f k v ))
                                            |> Dict.fromList
                                        )
                        , fuzz dictFuzzer "Doesn't change the size" <|
                            \dict ->
                                dict
                                    |> Dict.map f
                                    |> Dict.size
                                    |> Expect.equal (Dict.size dict)
                        , respectsInvariantsFuzz (Fuzz.map (Dict.map f) dictFuzzer)
                        ]
                            |> describe flabel
                    )
    in
    describe "map"
        (tests
            ++ [ fuzz dictFuzzer "map (always identity) == identity" <|
                    \dict ->
                        dict
                            |> Dict.map (always identity)
                            |> expectEqual dict
               ]
        )


foldlTest : Test
foldlTest =
    describe "foldl"
        [ fuzz dictFuzzer "foldl (::) is equivalent to toList >> reverse" <|
            \dict ->
                dict
                    |> Dict.foldl (\k v -> (::) ( k, v )) []
                    |> Expect.equalLists (List.reverse <| Dict.toList dict)
        , fuzz dictFuzzer "foldl insert is an identity" <|
            \dict ->
                dict
                    |> Dict.foldl Dict.insert Dict.empty
                    |> expectEqual dict
        ]


foldrTest : Test
foldrTest =
    describe "foldr"
        [ fuzz dictFuzzer "foldr (::) is equivalent to toList" <|
            \dict ->
                dict
                    |> Dict.foldr (\k v -> (::) ( k, v )) []
                    |> Expect.equalLists (Dict.toList dict)
        , fuzz dictFuzzer "foldr insert is an identity" <|
            \dict ->
                dict
                    |> Dict.foldr Dict.insert Dict.empty
                    |> expectEqual dict
        ]


filterTest : Test
filterTest =
    let
        f : Key -> Value -> Bool
        f _ v =
            modBy 2 v == 0

        filteredFuzzer =
            Fuzz.map (Dict.filter f) dictFuzzer
    in
    describe "filter"
        [ fuzz dictFuzzer "Is equivalent to toList >> List.filter >> fromList" <|
            \dict ->
                dict
                    |> Dict.filter f
                    |> expectEqual
                        (dict
                            |> Dict.toList
                            |> List.filter (\( k, v ) -> f k v)
                            |> Dict.fromList
                        )
        , respectsInvariantsFuzz filteredFuzzer
        ]


filterMapTest : Test
filterMapTest =
    let
        f : Key -> Value -> Maybe Value
        f _ v =
            if modBy 2 v == 0 then
                Just v

            else
                Nothing

        filteredFuzzer =
            Fuzz.map (Dict.filterMap f) dictFuzzer
    in
    describe "filterMap"
        [ fuzz dictFuzzer "Is equivalent to toList >> List.filterMap >> fromList" <|
            \dict ->
                dict
                    |> Dict.filterMap f
                    |> expectEqual
                        (dict
                            |> Dict.toList
                            |> List.filterMap
                                (\( k, v ) ->
                                    case f k v of
                                        Just v2 ->
                                            Just ( k, v2 )

                                        Nothing ->
                                            Nothing
                                )
                            |> Dict.fromList
                        )
        , respectsInvariantsFuzz filteredFuzzer
        ]


partitionTest : Test
partitionTest =
    let
        f : Key -> Value -> Bool
        f _ v =
            modBy 2 v == 0

        partitionedFuzzer : Fuzzer ( SeqDict Key Value, SeqDict Key Value )
        partitionedFuzzer =
            Fuzz.map (Dict.partition f) dictFuzzer
    in
    describe "partition"
        [ fuzz dictFuzzer "Is equivalent to toList >> List.partition >> fromList" <|
            \dict ->
                let
                    ( l, r ) =
                        Dict.partition f dict

                    ( el, er ) =
                        dict
                            |> Dict.toList
                            |> List.partition (\( k, v ) -> f k v)
                            |> Tuple.mapBoth Dict.fromList Dict.fromList
                in
                Expect.all
                    [ \_ -> expectEqual el l
                    , \_ -> expectEqual er r
                    ]
                    ()
        , describe "first" [ respectsInvariantsFuzz (Fuzz.map Tuple.first partitionedFuzzer) ]
        , describe "second" [ respectsInvariantsFuzz (Fuzz.map Tuple.second partitionedFuzzer) ]
        ]
