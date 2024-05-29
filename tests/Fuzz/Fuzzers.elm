module Fuzz.Fuzzers exposing (Key, Value, dictFuzzer, keyFuzzer, pairListFuzzer, valueFuzzer, veryBalanced, veryUnbalanced)

import Fuzz exposing (Fuzzer)
import OrderedDict as Dict exposing (OrderedDict)


type alias Key =
    String


type alias Value =
    Int


dictFuzzer : Fuzzer (OrderedDict Key Value)
dictFuzzer =
    Fuzz.oneOf
        [ fromListFuzzer
        , fromOpsFuzzer
        , Fuzz.map veryBalanced (Fuzz.intRange 0 1024)
        , Fuzz.map veryUnbalanced (Fuzz.intRange 0 1024)
        ]


fromOpsFuzzer : Fuzzer (OrderedDict Key Value)
fromOpsFuzzer =
    opFuzzer
        |> Fuzz.listOfLengthBetween 0 100
        |> Fuzz.map (List.foldl applyOp Dict.empty)


applyOp : Op -> OrderedDict Key Value -> OrderedDict Key Value
applyOp op acc =
    case op of
        Insert k v ->
            Dict.insert k v acc

        Delete index ->
            let
                listed : List ( Key, Value )
                listed =
                    Dict.toList acc

                fixedIndex : Int
                fixedIndex =
                    -- the *2 makes it a 50% chance of deleting
                    -- the +1 avoids a division by zero
                    modBy (List.length listed * 2 + 1) index
            in
            case List.drop fixedIndex (Dict.keys acc) of
                key :: _ ->
                    Dict.remove key acc

                _ ->
                    acc


opFuzzer : Fuzzer Op
opFuzzer =
    Fuzz.oneOf
        [ Fuzz.map2 Insert keyFuzzer valueFuzzer
        , Fuzz.map Delete Fuzz.int
        ]


type Op
    = Insert Key Value
    | Delete Int


fromListFuzzer : Fuzzer (OrderedDict Key Value)
fromListFuzzer =
    pairListFuzzer
        |> Fuzz.map Dict.fromList


pairListFuzzer : Fuzzer (List ( Key, Value ))
pairListFuzzer =
    Fuzz.pair keyFuzzer valueFuzzer
        |> Fuzz.listOfLengthBetween 1 100


keyFuzzer : Fuzzer Key
keyFuzzer =
    Fuzz.oneOf
        [ Fuzz.intRange 0 10 -- provoke more collisions
        , Fuzz.int
        ]
        |> Fuzz.map String.fromInt


valueFuzzer : Fuzzer Value
valueFuzzer =
    Fuzz.int


veryBalanced : Int -> OrderedDict Key Value
veryBalanced n =
    let
        insert : Int -> OrderedDict Key Value -> OrderedDict Key Value
        insert k =
            Dict.insert (String.fromInt k) k

        go : Int -> Int -> OrderedDict Key Value -> OrderedDict Key Value
        go low high acc =
            if low >= high then
                insert low acc

            else
                let
                    mid =
                        low + (high - low) // 2
                in
                acc
                    |> insert mid
                    |> go low (mid - 1)
                    |> go (mid + 1) high
    in
    go 1 n Dict.empty


veryUnbalanced : Int -> OrderedDict Key Value
veryUnbalanced n =
    List.range 1 n
        |> List.map (\k -> ( String.fromInt k, k ))
        |> Dict.fromList
