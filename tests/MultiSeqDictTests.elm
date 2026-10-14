module MultiSeqDictTests exposing (tests)

import Expect
import Fuzz exposing (Fuzzer, int, list, tuple)
import MultiSeqDict exposing (MultiSeqDict)
import SeqSet exposing (SeqSet)
import Test exposing (..)


{-| Non-comparable custom type to prove no comparable constraint
-}
type CustomType
    = Foo
    | Bar
    | Baz


type alias CustomRecord =
    { name : String
    , value : Int
    }


{-| Example dictionary: one key can have multiple values
-}
properties : MultiSeqDict String String
properties =
    MultiSeqDict.empty
        |> MultiSeqDict.insert "colors" "red"
        |> MultiSeqDict.insert "colors" "blue"
        |> MultiSeqDict.insert "numbers" "1"
        |> MultiSeqDict.insert "numbers" "2"
        |> MultiSeqDict.insert "letters" "a"


fuzzPairs : Fuzzer (List ( Int, Int ))
fuzzPairs =
    list (tuple ( int, int ))


tests : Test
tests =
    describe "MultiSeqDict"
        [ buildTests
        , queryTests
        , listsTests
        , transformTests
        , combineTests
        , customTypeTests
        , fuzzTests
        ]


buildTests : Test
buildTests =
    describe "Build Tests"
        [ test "empty" <|
            \() -> Expect.equal (MultiSeqDict.fromFlatList []) MultiSeqDict.empty
        , test "singleton" <|
            \() ->
                let
                    dict =
                        MultiSeqDict.singleton "k" "v"

                    values =
                        MultiSeqDict.getAll "k" dict
                in
                Expect.equal (SeqSet.size values) 1
        , test "insert" <|
            \() ->
                let
                    dict =
                        MultiSeqDict.insert "k" "v" MultiSeqDict.empty

                    values =
                        MultiSeqDict.getAll "k" dict
                in
                Expect.equal (SeqSet.size values) 1
        , test "insert multiple values same key" <|
            \() ->
                let
                    dict =
                        MultiSeqDict.empty
                            |> MultiSeqDict.insert "k" "v1"
                            |> MultiSeqDict.insert "k" "v2"
                            |> MultiSeqDict.insert "k" "v3"

                    values =
                        MultiSeqDict.getAll "k" dict
                in
                Expect.equal (SeqSet.size values) 3
        , test "insert duplicate value" <|
            \() ->
                let
                    dict =
                        MultiSeqDict.empty
                            |> MultiSeqDict.insert "k" "v"
                            |> MultiSeqDict.insert "k" "v"

                    values =
                        MultiSeqDict.getAll "k" dict
                in
                Expect.equal (SeqSet.size values) 1
        , test "update" <|
            \() ->
                let
                    dict =
                        MultiSeqDict.singleton "k" "v1"
                            |> MultiSeqDict.update "k" (SeqSet.insert "v2")

                    values =
                        MultiSeqDict.getAll "k" dict
                in
                Expect.equal (SeqSet.size values) 2
        , test "update to empty removes key" <|
            \() ->
                let
                    dict =
                        MultiSeqDict.singleton "k" "v"
                            |> MultiSeqDict.update "k" (\_ -> SeqSet.empty)
                in
                Expect.equal (MultiSeqDict.member "k" dict) False
        , test "remove single value" <|
            \() ->
                let
                    dict =
                        MultiSeqDict.empty
                            |> MultiSeqDict.insert "k" "v1"
                            |> MultiSeqDict.insert "k" "v2"
                            |> MultiSeqDict.remove "k" "v1"

                    values =
                        MultiSeqDict.getAll "k" dict
                in
                Expect.equal (SeqSet.size values) 1
        , test "remove last value removes key" <|
            \() ->
                let
                    dict =
                        MultiSeqDict.singleton "k" "v"
                            |> MultiSeqDict.remove "k" "v"
                in
                Expect.equal (MultiSeqDict.member "k" dict) False
        , test "remove not found" <|
            \() ->
                let
                    original =
                        MultiSeqDict.singleton "k" "v"

                    modified =
                        MultiSeqDict.remove "k" "notfound" original
                in
                Expect.equal original modified
        , test "removeAll" <|
            \() ->
                let
                    dict =
                        MultiSeqDict.empty
                            |> MultiSeqDict.insert "k" "v1"
                            |> MultiSeqDict.insert "k" "v2"
                            |> MultiSeqDict.insert "k" "v3"
                            |> MultiSeqDict.removeAll "k"
                in
                Expect.equal (MultiSeqDict.member "k" dict) False
        , test "removeAll not found" <|
            \() ->
                let
                    original =
                        MultiSeqDict.singleton "k" "v"

                    modified =
                        MultiSeqDict.removeAll "notfound" original
                in
                Expect.equal original modified
        , test "size counts all key-value pairs" <|
            \() ->
                let
                    dict =
                        MultiSeqDict.empty
                            |> MultiSeqDict.insert "k1" "v1"
                            |> MultiSeqDict.insert "k1" "v2"
                            |> MultiSeqDict.insert "k2" "v3"
                            |> MultiSeqDict.remove "k1" "v1"
                in
                Expect.equal (MultiSeqDict.size dict) 2
        ]


queryTests : Test
queryTests =
    describe "Query Tests"
        [ test "member found" <|
            \() -> Expect.equal True (MultiSeqDict.member "colors" properties)
        , test "member not found" <|
            \() -> Expect.equal False (MultiSeqDict.member "shapes" properties)
        , test "get returns all values" <|
            \() ->
                let
                    colors =
                        MultiSeqDict.getAll "colors" properties
                in
                Expect.equal (SeqSet.size colors) 2
        , test "get not found returns empty" <|
            \() ->
                let
                    shapes =
                        MultiSeqDict.getAll "shapes" properties
                in
                Expect.equal (SeqSet.isEmpty shapes) True
        , test "size of empty dictionary" <|
            \() -> Expect.equal 0 (MultiSeqDict.size MultiSeqDict.empty)
        , test "size of example dictionary" <|
            \() -> Expect.equal 5 (MultiSeqDict.size properties)
        , test "isEmpty empty" <|
            \() -> Expect.equal True (MultiSeqDict.isEmpty MultiSeqDict.empty)
        , test "isEmpty non-empty" <|
            \() -> Expect.equal False (MultiSeqDict.isEmpty properties)
        ]


listsTests : Test
listsTests =
    describe "Lists Tests"
        [ test "keys" <|
            \() ->
                MultiSeqDict.keys properties
                    |> Expect.equal [ "colors", "numbers", "letters" ]
        , test "values preserves all values" <|
            \() ->
                let
                    vals =
                        MultiSeqDict.values properties
                in
                Expect.equal (List.length vals) 5
        , test "values order" <|
            \() ->
                MultiSeqDict.values properties
                    |> Expect.equal [ "red", "blue", "1", "2", "a" ]
        , test "toList" <|
            \() ->
                let
                    list =
                        MultiSeqDict.toList properties

                    keys =
                        List.map Tuple.first list
                in
                Expect.equal keys [ "colors", "numbers", "letters" ]
        , test "fromList" <|
            \() ->
                let
                    dict =
                        MultiSeqDict.fromList
                            [ ( "k1", SeqSet.fromList [ 1, 2 ] )
                            , ( "k2", SeqSet.fromList [ 3 ] )
                            ]
                in
                Expect.equal (MultiSeqDict.size dict) 3
        , test "fromFlatList" <|
            \() ->
                let
                    dict =
                        MultiSeqDict.fromFlatList
                            [ ( "k", 1 )
                            , ( "k", 2 )
                            , ( "j", 3 )
                            ]

                    values =
                        MultiSeqDict.getAll "k" dict
                in
                Expect.equal (SeqSet.size values) 2
        , test "fromFlatList excludes duplicates" <|
            \() ->
                let
                    dict =
                        MultiSeqDict.fromFlatList
                            [ ( "k", 1 )
                            , ( "k", 1 )
                            ]

                    values =
                        MultiSeqDict.getAll "k" dict
                in
                Expect.equal (SeqSet.size values) 1
        , test "toList/fromList roundtrip" <|
            \() ->
                let
                    dict =
                        MultiSeqDict.fromFlatList
                            [ ( "a", 1 )
                            , ( "a", 2 )
                            , ( "b", 3 )
                            ]

                    roundtrip =
                        dict
                            |> MultiSeqDict.toList
                            |> MultiSeqDict.fromList
                in
                Expect.equal dict roundtrip
        ]


transformTests : Test
transformTests =
    describe "Transform Tests"
        [ test "map" <|
            \() ->
                let
                    dict =
                        MultiSeqDict.fromFlatList [ ( "a", 1 ), ( "a", 2 ) ]

                    mapped =
                        MultiSeqDict.map (\k v -> v * 2) dict

                    values =
                        MultiSeqDict.getAll "a" mapped
                            |> SeqSet.toList
                            |> List.sort
                in
                Expect.equal values [ 2, 4 ]
        , test "foldl" <|
            \() ->
                let
                    dict =
                        MultiSeqDict.fromFlatList [ ( "a", 1 ), ( "a", 2 ), ( "b", 3 ) ]

                    sum =
                        MultiSeqDict.foldl (\k values acc -> acc + SeqSet.size values) 0 dict
                in
                Expect.equal sum 3
        , test "foldr" <|
            \() ->
                let
                    dict =
                        MultiSeqDict.fromFlatList [ ( "a", 1 ), ( "b", 2 ), ( "c", 3 ) ]

                    keys =
                        MultiSeqDict.foldr (\k _ acc -> k :: acc) [] dict
                in
                Expect.equal keys [ "c", "b", "a" ]
        , test "filter" <|
            \() ->
                let
                    dict =
                        MultiSeqDict.fromFlatList [ ( "a", 1 ), ( "a", 2 ), ( "b", 3 ) ]

                    filtered =
                        MultiSeqDict.filter (\k v -> v > 1) dict
                in
                Expect.equal (MultiSeqDict.size filtered) 2
        , test "filter removes empty keys" <|
            \() ->
                let
                    dict =
                        MultiSeqDict.fromFlatList [ ( "a", 1 ), ( "a", 2 ) ]

                    filtered =
                        MultiSeqDict.filter (\k v -> v > 5) dict
                in
                Expect.equal (MultiSeqDict.isEmpty filtered) True
        , test "partition" <|
            \() ->
                let
                    dict =
                        MultiSeqDict.fromFlatList [ ( "a", 1 ), ( "a", 2 ), ( "b", 3 ) ]

                    ( hasTwo, noTwo ) =
                        MultiSeqDict.partition (\k values -> SeqSet.size values == 2) dict
                in
                Expect.equal ( MultiSeqDict.size hasTwo, MultiSeqDict.size noTwo ) ( 2, 1 )
        ]


combineTests : Test
combineTests =
    describe "Combine Tests"
        [ test "union" <|
            \() ->
                let
                    d1 =
                        MultiSeqDict.fromFlatList [ ( "a", 1 ) ]

                    d2 =
                        MultiSeqDict.fromFlatList [ ( "b", 2 ) ]

                    result =
                        MultiSeqDict.union d1 d2
                in
                Expect.equal (MultiSeqDict.size result) 2
        , test "union prefers left" <|
            \() ->
                let
                    d1 =
                        MultiSeqDict.fromFlatList [ ( "a", 1 ) ]

                    d2 =
                        MultiSeqDict.fromFlatList [ ( "a", 2 ) ]

                    result =
                        MultiSeqDict.union d1 d2

                    values =
                        MultiSeqDict.getAll "a" result
                in
                Expect.equal (SeqSet.size values) 1
        , test "intersect" <|
            \() ->
                let
                    d1 =
                        MultiSeqDict.fromFlatList [ ( "a", 1 ), ( "b", 2 ) ]

                    d2 =
                        MultiSeqDict.fromFlatList [ ( "a", 3 ) ]

                    result =
                        MultiSeqDict.intersect d1 d2
                in
                Expect.equal (MultiSeqDict.size result) 1
        , test "diff" <|
            \() ->
                let
                    d1 =
                        MultiSeqDict.fromFlatList [ ( "a", 1 ), ( "b", 2 ) ]

                    d2 =
                        MultiSeqDict.fromFlatList [ ( "a", 3 ) ]

                    result =
                        MultiSeqDict.diff d1 d2
                in
                Expect.equal (MultiSeqDict.keys result) [ "b" ]
        , test "merge" <|
            \() ->
                let
                    d1 =
                        MultiSeqDict.fromFlatList [ ( "a", 1 ), ( "b", 2 ) ]

                    d2 =
                        MultiSeqDict.fromFlatList [ ( "b", 3 ), ( "c", 4 ) ]

                    result =
                        MultiSeqDict.merge
                            (\k _ acc -> acc + 1)
                            (\k _ _ acc -> acc + 10)
                            (\k _ acc -> acc + 100)
                            d1
                            d2
                            0
                in
                Expect.equal result 111
        ]


customTypeTests : Test
customTypeTests =
    describe "Custom Type Tests (no comparable constraint)"
        [ test "can create and insert custom types" <|
            \() ->
                let
                    dict =
                        MultiSeqDict.empty
                            |> MultiSeqDict.insert Foo 1
                            |> MultiSeqDict.insert Foo 2
                            |> MultiSeqDict.insert Bar 3

                    fooValues =
                        MultiSeqDict.getAll Foo dict
                in
                Expect.equal (SeqSet.size fooValues) 2
        , test "multiple custom type keys" <|
            \() ->
                let
                    dict =
                        MultiSeqDict.empty
                            |> MultiSeqDict.insert Foo "hello"
                            |> MultiSeqDict.insert Bar "world"
                            |> MultiSeqDict.insert Baz "hello"
                in
                Expect.equal (MultiSeqDict.size dict) 3
        , test "can use custom records as values" <|
            \() ->
                let
                    dict =
                        MultiSeqDict.empty
                            |> MultiSeqDict.insert "alice" { name = "Alice", value = 1 }
                            |> MultiSeqDict.insert "alice" { name = "Alice", value = 2 }
                            |> MultiSeqDict.insert "bob" { name = "Bob", value = 1 }

                    aliceValues =
                        MultiSeqDict.getAll "alice" dict
                in
                Expect.equal (SeqSet.size aliceValues) 2
        ]


fuzzTests : Test
fuzzTests =
    describe "Fuzz Tests"
        [ fuzz2 fuzzPairs int "get works" <|
            \pairs num ->
                let
                    dict =
                        MultiSeqDict.fromFlatList pairs

                    result =
                        MultiSeqDict.getAll num dict

                    expected =
                        pairs
                            |> List.filter (\( k, _ ) -> k == num)
                            |> List.map Tuple.second
                            |> SeqSet.fromList
                in
                Expect.equal result expected
        , fuzz fuzzPairs "fromFlatList creates valid dict" <|
            \pairs ->
                let
                    dict =
                        MultiSeqDict.fromFlatList pairs

                    uniqueKeys =
                        pairs
                            |> List.map Tuple.first
                            |> SeqSet.fromList
                            |> SeqSet.size

                    dictKeys =
                        MultiSeqDict.keys dict |> List.length
                in
                Expect.equal uniqueKeys dictKeys
        , fuzz2 fuzzPairs int "insert works" <|
            \pairs num ->
                let
                    dict =
                        MultiSeqDict.fromFlatList pairs
                            |> MultiSeqDict.insert num num

                    values =
                        MultiSeqDict.getAll num dict
                in
                Expect.equal (SeqSet.member num values) True
        , fuzz2 fuzzPairs int "remove works" <|
            \pairs num ->
                let
                    dict =
                        MultiSeqDict.fromFlatList pairs
                            |> MultiSeqDict.remove num num

                    values =
                        MultiSeqDict.getAll num dict
                in
                Expect.equal (SeqSet.member num values) False
        , fuzz2 fuzzPairs int "removeAll works" <|
            \pairs num ->
                let
                    dict =
                        MultiSeqDict.fromFlatList pairs
                            |> MultiSeqDict.removeAll num
                in
                Expect.equal (MultiSeqDict.member num dict) False
        , fuzz2 fuzzPairs fuzzPairs "union works" <|
            \pairs1 pairs2 ->
                let
                    d1 =
                        MultiSeqDict.fromFlatList pairs1

                    d2 =
                        MultiSeqDict.fromFlatList pairs2

                    result =
                        MultiSeqDict.union d1 d2

                    unionKeys =
                        (pairs1 ++ pairs2)
                            |> List.map Tuple.first
                            |> SeqSet.fromList
                            |> SeqSet.size

                    resultKeys =
                        MultiSeqDict.keys result |> List.length
                in
                Expect.equal unionKeys resultKeys
        , fuzz fuzzPairs "size counts all pairs" <|
            \pairs ->
                let
                    dict =
                        MultiSeqDict.fromFlatList pairs

                    uniquePairs =
                        pairs
                            |> SeqSet.fromList
                            |> SeqSet.size
                in
                Expect.equal (MultiSeqDict.size dict) uniquePairs
        ]
