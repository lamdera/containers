module MultiBiSeqDictTests exposing (tests)

import Expect
import Fuzz exposing (Fuzzer, int, list, tuple)
import MultiBiSeqDict exposing (MultiBiSeqDict)
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


{-| Example dictionary: many-to-many relationships
-}
chatDocuments : MultiBiSeqDict String String
chatDocuments =
    MultiBiSeqDict.empty
        |> MultiBiSeqDict.insert "chat1" "doc1"
        |> MultiBiSeqDict.insert "chat1" "doc2"
        |> MultiBiSeqDict.insert "chat2" "doc1"
        |> MultiBiSeqDict.insert "chat2" "doc3"
        |> MultiBiSeqDict.insert "chat3" "doc2"


fuzzPairs : Fuzzer (List ( Int, Int ))
fuzzPairs =
    list (tuple ( int, int ))


tests : Test
tests =
    describe "MultiBiSeqDict"
        [ buildTests
        , queryTests
        , reverseTests
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
            \() ->
                let
                    dict =
                        MultiBiSeqDict.empty
                in
                Expect.equal (MultiBiSeqDict.size dict) 0
        , test "singleton" <|
            \() ->
                let
                    dict =
                        MultiBiSeqDict.singleton "k" "v"

                    values =
                        MultiBiSeqDict.getAll "k" dict

                    keys =
                        MultiBiSeqDict.getKeys "v" dict
                in
                Expect.equal ( SeqSet.size values, SeqSet.size keys ) ( 1, 1 )
        , test "insert" <|
            \() ->
                let
                    dict =
                        MultiBiSeqDict.insert "k" "v" MultiBiSeqDict.empty

                    values =
                        MultiBiSeqDict.getAll "k" dict
                in
                Expect.equal (SeqSet.size values) 1
        , test "insert multiple values same key" <|
            \() ->
                let
                    dict =
                        MultiBiSeqDict.empty
                            |> MultiBiSeqDict.insert "k" "v1"
                            |> MultiBiSeqDict.insert "k" "v2"
                            |> MultiBiSeqDict.insert "k" "v3"

                    values =
                        MultiBiSeqDict.getAll "k" dict
                in
                Expect.equal (SeqSet.size values) 3
        , test "insert multiple keys same value" <|
            \() ->
                let
                    dict =
                        MultiBiSeqDict.empty
                            |> MultiBiSeqDict.insert "k1" "v"
                            |> MultiBiSeqDict.insert "k2" "v"
                            |> MultiBiSeqDict.insert "k3" "v"

                    keys =
                        MultiBiSeqDict.getKeys "v" dict
                in
                Expect.equal (SeqSet.size keys) 3
        , test "insert duplicate value" <|
            \() ->
                let
                    dict =
                        MultiBiSeqDict.empty
                            |> MultiBiSeqDict.insert "k" "v"
                            |> MultiBiSeqDict.insert "k" "v"

                    values =
                        MultiBiSeqDict.getAll "k" dict
                in
                Expect.equal (SeqSet.size values) 1
        , test "update" <|
            \() ->
                let
                    dict =
                        MultiBiSeqDict.singleton "k" "v1"
                            |> MultiBiSeqDict.update "k" (SeqSet.insert "v2")

                    values =
                        MultiBiSeqDict.getAll "k" dict
                in
                Expect.equal (SeqSet.size values) 2
        , test "update to empty removes key" <|
            \() ->
                let
                    dict =
                        MultiBiSeqDict.singleton "k" "v"
                            |> MultiBiSeqDict.update "k" (\_ -> SeqSet.empty)
                in
                Expect.equal (MultiBiSeqDict.member "k" dict) False
        , test "update maintains reverse index" <|
            \() ->
                let
                    dict =
                        MultiBiSeqDict.singleton "k" "v1"
                            |> MultiBiSeqDict.update "k" (SeqSet.insert "v2")

                    keysForV2 =
                        MultiBiSeqDict.getKeys "v2" dict
                in
                Expect.equal (SeqSet.member "k" keysForV2) True
        , test "remove single value" <|
            \() ->
                let
                    dict =
                        MultiBiSeqDict.empty
                            |> MultiBiSeqDict.insert "k" "v1"
                            |> MultiBiSeqDict.insert "k" "v2"
                            |> MultiBiSeqDict.remove "k" "v1"

                    values =
                        MultiBiSeqDict.getAll "k" dict
                in
                Expect.equal (SeqSet.size values) 1
        , test "remove updates reverse index" <|
            \() ->
                let
                    dict =
                        MultiBiSeqDict.empty
                            |> MultiBiSeqDict.insert "k1" "v"
                            |> MultiBiSeqDict.insert "k2" "v"
                            |> MultiBiSeqDict.remove "k1" "v"

                    keys =
                        MultiBiSeqDict.getKeys "v" dict
                in
                Expect.equal (SeqSet.size keys) 1
        , test "remove last value removes key" <|
            \() ->
                let
                    dict =
                        MultiBiSeqDict.singleton "k" "v"
                            |> MultiBiSeqDict.remove "k" "v"
                in
                Expect.equal (MultiBiSeqDict.member "k" dict) False
        , test "remove not found" <|
            \() ->
                let
                    original =
                        MultiBiSeqDict.singleton "k" "v"

                    modified =
                        MultiBiSeqDict.remove "k" "notfound" original
                in
                Expect.equal original modified
        , test "removeAll" <|
            \() ->
                let
                    dict =
                        MultiBiSeqDict.empty
                            |> MultiBiSeqDict.insert "k" "v1"
                            |> MultiBiSeqDict.insert "k" "v2"
                            |> MultiBiSeqDict.insert "k" "v3"
                            |> MultiBiSeqDict.removeAll "k"
                in
                Expect.equal (MultiBiSeqDict.member "k" dict) False
        , test "removeAll updates reverse index" <|
            \() ->
                let
                    dict =
                        MultiBiSeqDict.empty
                            |> MultiBiSeqDict.insert "k" "v1"
                            |> MultiBiSeqDict.insert "k" "v2"
                            |> MultiBiSeqDict.insert "j" "v1"
                            |> MultiBiSeqDict.removeAll "k"

                    keysForV1 =
                        MultiBiSeqDict.getKeys "v1" dict

                    keysForV2 =
                        MultiBiSeqDict.getKeys "v2" dict
                in
                Expect.equal ( SeqSet.size keysForV1, SeqSet.size keysForV2 ) ( 1, 0 )
        , test "removeAll not found" <|
            \() ->
                let
                    original =
                        MultiBiSeqDict.singleton "k" "v"

                    modified =
                        MultiBiSeqDict.removeAll "notfound" original
                in
                Expect.equal original modified
        , test "size counts all key-value pairs" <|
            \() ->
                let
                    dict =
                        MultiBiSeqDict.empty
                            |> MultiBiSeqDict.insert "k1" "v1"
                            |> MultiBiSeqDict.insert "k1" "v2"
                            |> MultiBiSeqDict.insert "k2" "v3"
                            |> MultiBiSeqDict.remove "k1" "v1"
                in
                Expect.equal (MultiBiSeqDict.size dict) 2
        ]


queryTests : Test
queryTests =
    describe "Query Tests"
        [ test "member found" <|
            \() -> Expect.equal True (MultiBiSeqDict.member "chat1" chatDocuments)
        , test "member not found" <|
            \() -> Expect.equal False (MultiBiSeqDict.member "chat99" chatDocuments)
        , test "get returns all values" <|
            \() ->
                let
                    docs =
                        MultiBiSeqDict.getAll "chat1" chatDocuments
                in
                Expect.equal (SeqSet.size docs) 2
        , test "get not found returns empty" <|
            \() ->
                let
                    docs =
                        MultiBiSeqDict.getAll "chat99" chatDocuments
                in
                Expect.equal (SeqSet.isEmpty docs) True
        , test "size of empty dictionary" <|
            \() -> Expect.equal 0 (MultiBiSeqDict.size MultiBiSeqDict.empty)
        , test "size of example dictionary" <|
            \() -> Expect.equal 5 (MultiBiSeqDict.size chatDocuments)
        , test "isEmpty empty" <|
            \() -> Expect.equal True (MultiBiSeqDict.isEmpty MultiBiSeqDict.empty)
        , test "isEmpty non-empty" <|
            \() -> Expect.equal False (MultiBiSeqDict.isEmpty chatDocuments)
        ]


reverseTests : Test
reverseTests =
    describe "Reverse Lookup Tests"
        [ test "getReverse returns all keys" <|
            \() ->
                let
                    chats =
                        MultiBiSeqDict.getKeys "doc1" chatDocuments
                in
                Expect.equal (SeqSet.size chats) 2
        , test "getReverse not found returns empty" <|
            \() ->
                let
                    chats =
                        MultiBiSeqDict.getKeys "doc99" chatDocuments
                in
                Expect.equal (SeqSet.isEmpty chats) True
        , test "getReverse after insert" <|
            \() ->
                let
                    dict =
                        MultiBiSeqDict.empty
                            |> MultiBiSeqDict.insert "k1" "v"
                            |> MultiBiSeqDict.insert "k2" "v"
                            |> MultiBiSeqDict.insert "k3" "v"

                    keys =
                        MultiBiSeqDict.getKeys "v" dict
                in
                Expect.equal (SeqSet.size keys) 3
        , test "getReverse after remove" <|
            \() ->
                let
                    dict =
                        MultiBiSeqDict.empty
                            |> MultiBiSeqDict.insert "k1" "v"
                            |> MultiBiSeqDict.insert "k2" "v"
                            |> MultiBiSeqDict.remove "k1" "v"

                    keys =
                        MultiBiSeqDict.getKeys "v" dict
                in
                Expect.equal (SeqSet.size keys) 1
        , test "reverse index consistency after replace" <|
            \() ->
                let
                    dict =
                        MultiBiSeqDict.singleton "k" "old"
                            |> MultiBiSeqDict.update "k" (\_ -> SeqSet.singleton "new")

                    oldKeys =
                        MultiBiSeqDict.getKeys "old" dict

                    newKeys =
                        MultiBiSeqDict.getKeys "new" dict
                in
                Expect.equal ( SeqSet.size oldKeys, SeqSet.size newKeys ) ( 0, 1 )
        , test "reverse consistency with multiple updates" <|
            \() ->
                let
                    dict =
                        MultiBiSeqDict.empty
                            |> MultiBiSeqDict.insert "k1" "v1"
                            |> MultiBiSeqDict.insert "k1" "v2"
                            |> MultiBiSeqDict.insert "k2" "v1"
                            |> MultiBiSeqDict.remove "k1" "v1"

                    keysForV1 =
                        MultiBiSeqDict.getKeys "v1" dict

                    keysForV2 =
                        MultiBiSeqDict.getKeys "v2" dict
                in
                Expect.equal ( SeqSet.size keysForV1, SeqSet.size keysForV2 ) ( 1, 1 )
        , test "uniqueValues returns all unique values" <|
            \() ->
                let
                    dict =
                        MultiBiSeqDict.empty
                            |> MultiBiSeqDict.insert "k1" "v1"
                            |> MultiBiSeqDict.insert "k1" "v2"
                            |> MultiBiSeqDict.insert "k2" "v1"

                    uniques =
                        MultiBiSeqDict.uniqueValues dict
                in
                Expect.equal (List.length uniques) 2
        , test "uniqueValuesCount" <|
            \() ->
                Expect.equal (MultiBiSeqDict.uniqueValuesCount chatDocuments) 3
        ]


listsTests : Test
listsTests =
    describe "Lists Tests"
        [ test "keys" <|
            \() ->
                MultiBiSeqDict.keys chatDocuments
                    |> Expect.equal [ "chat1", "chat2", "chat3" ]
        , test "values preserves all values" <|
            \() ->
                let
                    vals =
                        MultiBiSeqDict.values chatDocuments
                in
                Expect.equal (List.length vals) 5
        , test "values order" <|
            \() ->
                MultiBiSeqDict.values chatDocuments
                    |> Expect.equal [ "doc1", "doc2", "doc1", "doc3", "doc2" ]
        , test "toList" <|
            \() ->
                let
                    list =
                        MultiBiSeqDict.toList chatDocuments

                    keys =
                        List.map Tuple.first list
                in
                Expect.equal keys [ "chat1", "chat2", "chat3" ]
        , test "toReverseList" <|
            \() ->
                let
                    list =
                        MultiBiSeqDict.toReverseList chatDocuments

                    values =
                        List.map Tuple.first list
                in
                Expect.equal (List.length values) 3
        , test "fromList" <|
            \() ->
                let
                    dict =
                        MultiBiSeqDict.fromList
                            [ ( "k1", SeqSet.fromList [ 1, 2 ] )
                            , ( "k2", SeqSet.fromList [ 3 ] )
                            ]

                    keysFor3 =
                        MultiBiSeqDict.getKeys 3 dict
                in
                Expect.equal (SeqSet.size keysFor3) 1
        , test "toList/fromList roundtrip" <|
            \() ->
                let
                    roundtrip =
                        chatDocuments
                            |> MultiBiSeqDict.toList
                            |> MultiBiSeqDict.fromList
                in
                Expect.equal chatDocuments roundtrip
        ]


transformTests : Test
transformTests =
    describe "Transform Tests"
        [ test "map" <|
            \() ->
                let
                    dict =
                        MultiBiSeqDict.empty
                            |> MultiBiSeqDict.insert "a" 1
                            |> MultiBiSeqDict.insert "a" 2

                    mapped =
                        MultiBiSeqDict.map (\k v -> v * 2) dict

                    values =
                        MultiBiSeqDict.getAll "a" mapped
                            |> SeqSet.toList
                            |> List.sort
                in
                Expect.equal values [ 2, 4 ]
        , test "map maintains reverse index" <|
            \() ->
                let
                    dict =
                        MultiBiSeqDict.empty
                            |> MultiBiSeqDict.insert "a" 1
                            |> MultiBiSeqDict.insert "b" 1

                    mapped =
                        MultiBiSeqDict.map (\k v -> v * 10) dict

                    keysFor10 =
                        MultiBiSeqDict.getKeys 10 mapped
                in
                Expect.equal (SeqSet.size keysFor10) 2
        , test "foldl" <|
            \() ->
                let
                    sum =
                        MultiBiSeqDict.foldl (\k values acc -> acc + SeqSet.size values) 0 chatDocuments
                in
                Expect.equal sum 5
        , test "foldr" <|
            \() ->
                let
                    keys =
                        MultiBiSeqDict.foldr (\k _ acc -> k :: acc) [] chatDocuments
                in
                Expect.equal keys [ "chat3", "chat2", "chat1" ]
        , test "filter" <|
            \() ->
                let
                    dict =
                        MultiBiSeqDict.empty
                            |> MultiBiSeqDict.insert "a" 1
                            |> MultiBiSeqDict.insert "a" 2
                            |> MultiBiSeqDict.insert "b" 3

                    filtered =
                        MultiBiSeqDict.filter (\k v -> v > 1) dict
                in
                Expect.equal (MultiBiSeqDict.size filtered) 2
        , test "filter maintains reverse index" <|
            \() ->
                let
                    dict =
                        MultiBiSeqDict.empty
                            |> MultiBiSeqDict.insert "a" 1
                            |> MultiBiSeqDict.insert "b" 1
                            |> MultiBiSeqDict.insert "c" 2

                    filtered =
                        MultiBiSeqDict.filter (\k v -> v > 1) dict

                    keysFor1 =
                        MultiBiSeqDict.getKeys 1 filtered
                in
                Expect.equal (SeqSet.size keysFor1) 0
        , test "filter removes empty keys" <|
            \() ->
                let
                    dict =
                        MultiBiSeqDict.empty
                            |> MultiBiSeqDict.insert "a" 1
                            |> MultiBiSeqDict.insert "a" 2

                    filtered =
                        MultiBiSeqDict.filter (\k v -> v > 5) dict
                in
                Expect.equal (MultiBiSeqDict.isEmpty filtered) True
        , test "partition" <|
            \() ->
                let
                    dict =
                        MultiBiSeqDict.empty
                            |> MultiBiSeqDict.insert "a" 1
                            |> MultiBiSeqDict.insert "a" 2
                            |> MultiBiSeqDict.insert "b" 3

                    ( hasTwo, noTwo ) =
                        MultiBiSeqDict.partition (\k values -> SeqSet.size values == 2) dict
                in
                Expect.equal ( MultiBiSeqDict.size hasTwo, MultiBiSeqDict.size noTwo ) ( 2, 1 )
        , test "partition maintains reverse index" <|
            \() ->
                let
                    dict =
                        MultiBiSeqDict.empty
                            |> MultiBiSeqDict.insert "a" 1
                            |> MultiBiSeqDict.insert "a" 2
                            |> MultiBiSeqDict.insert "b" 1

                    ( hasTwo, noTwo ) =
                        MultiBiSeqDict.partition (\k values -> SeqSet.size values == 2) dict

                    keysFor1InHasTwo =
                        MultiBiSeqDict.getKeys 1 hasTwo

                    keysFor1InNoTwo =
                        MultiBiSeqDict.getKeys 1 noTwo
                in
                Expect.equal ( SeqSet.size keysFor1InHasTwo, SeqSet.size keysFor1InNoTwo ) ( 1, 1 )
        ]


combineTests : Test
combineTests =
    describe "Combine Tests"
        [ test "union" <|
            \() ->
                let
                    d1 =
                        MultiBiSeqDict.empty
                            |> MultiBiSeqDict.insert "a" 1

                    d2 =
                        MultiBiSeqDict.empty
                            |> MultiBiSeqDict.insert "b" 2

                    result =
                        MultiBiSeqDict.union d1 d2
                in
                Expect.equal (MultiBiSeqDict.size result) 2
        , test "union prefers left" <|
            \() ->
                let
                    d1 =
                        MultiBiSeqDict.empty
                            |> MultiBiSeqDict.insert "a" 1

                    d2 =
                        MultiBiSeqDict.empty
                            |> MultiBiSeqDict.insert "a" 2

                    result =
                        MultiBiSeqDict.union d1 d2

                    values =
                        MultiBiSeqDict.getAll "a" result
                in
                Expect.equal (SeqSet.size values) 1
        , test "union maintains reverse index" <|
            \() ->
                let
                    d1 =
                        MultiBiSeqDict.empty
                            |> MultiBiSeqDict.insert "a" 1

                    d2 =
                        MultiBiSeqDict.empty
                            |> MultiBiSeqDict.insert "b" 2

                    result =
                        MultiBiSeqDict.union d1 d2

                    keysFor1 =
                        MultiBiSeqDict.getKeys 1 result

                    keysFor2 =
                        MultiBiSeqDict.getKeys 2 result
                in
                Expect.equal ( SeqSet.size keysFor1, SeqSet.size keysFor2 ) ( 1, 1 )
        , test "intersect" <|
            \() ->
                let
                    d1 =
                        MultiBiSeqDict.empty
                            |> MultiBiSeqDict.insert "a" 1
                            |> MultiBiSeqDict.insert "b" 2

                    d2 =
                        MultiBiSeqDict.empty
                            |> MultiBiSeqDict.insert "a" 3

                    result =
                        MultiBiSeqDict.intersect d1 d2
                in
                Expect.equal (MultiBiSeqDict.size result) 1
        , test "intersect maintains reverse index" <|
            \() ->
                let
                    d1 =
                        MultiBiSeqDict.empty
                            |> MultiBiSeqDict.insert "a" 1
                            |> MultiBiSeqDict.insert "a" 2

                    d2 =
                        MultiBiSeqDict.empty
                            |> MultiBiSeqDict.insert "a" 3

                    result =
                        MultiBiSeqDict.intersect d1 d2

                    keysFor1 =
                        MultiBiSeqDict.getKeys 1 result
                in
                Expect.equal (SeqSet.size keysFor1) 1
        , test "diff" <|
            \() ->
                let
                    d1 =
                        MultiBiSeqDict.empty
                            |> MultiBiSeqDict.insert "a" 1
                            |> MultiBiSeqDict.insert "b" 2

                    d2 =
                        MultiBiSeqDict.empty
                            |> MultiBiSeqDict.insert "a" 3

                    result =
                        MultiBiSeqDict.diff d1 d2
                in
                Expect.equal (MultiBiSeqDict.keys result) [ "b" ]
        , test "diff maintains reverse index" <|
            \() ->
                let
                    d1 =
                        MultiBiSeqDict.empty
                            |> MultiBiSeqDict.insert "a" 1
                            |> MultiBiSeqDict.insert "b" 2

                    d2 =
                        MultiBiSeqDict.empty
                            |> MultiBiSeqDict.insert "a" 3

                    result =
                        MultiBiSeqDict.diff d1 d2

                    keysFor1 =
                        MultiBiSeqDict.getKeys 1 result

                    keysFor2 =
                        MultiBiSeqDict.getKeys 2 result
                in
                Expect.equal ( SeqSet.size keysFor1, SeqSet.size keysFor2 ) ( 0, 1 )
        , test "merge" <|
            \() ->
                let
                    d1 =
                        MultiBiSeqDict.empty
                            |> MultiBiSeqDict.insert "a" 1
                            |> MultiBiSeqDict.insert "b" 2

                    d2 =
                        MultiBiSeqDict.empty
                            |> MultiBiSeqDict.insert "b" 3
                            |> MultiBiSeqDict.insert "c" 4

                    result =
                        MultiBiSeqDict.merge
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
                        MultiBiSeqDict.empty
                            |> MultiBiSeqDict.insert Foo 1
                            |> MultiBiSeqDict.insert Foo 2
                            |> MultiBiSeqDict.insert Bar 2

                    fooValues =
                        MultiBiSeqDict.getAll Foo dict

                    keysFor2 =
                        MultiBiSeqDict.getKeys 2 dict
                in
                Expect.equal ( SeqSet.size fooValues, SeqSet.size keysFor2 ) ( 2, 2 )
        , test "getReverse works with custom types" <|
            \() ->
                let
                    dict =
                        MultiBiSeqDict.empty
                            |> MultiBiSeqDict.insert Foo "hello"
                            |> MultiBiSeqDict.insert Bar "world"
                            |> MultiBiSeqDict.insert Baz "hello"

                    keys =
                        MultiBiSeqDict.getKeys "hello" dict
                in
                Expect.equal (SeqSet.size keys) 2
        , test "can use custom records as values" <|
            \() ->
                let
                    dict =
                        MultiBiSeqDict.empty
                            |> MultiBiSeqDict.insert "alice" { name = "Alice", value = 1 }
                            |> MultiBiSeqDict.insert "alice" { name = "Alice", value = 2 }
                            |> MultiBiSeqDict.insert "bob" { name = "Alice", value = 1 }

                    record =
                        { name = "Alice", value = 1 }

                    keys =
                        MultiBiSeqDict.getKeys record dict
                in
                Expect.equal (SeqSet.size keys) 2
        ]


fuzzTests : Test
fuzzTests =
    describe "Fuzz Tests"
        [ fuzz2 fuzzPairs int "get works" <|
            \pairs num ->
                let
                    dict =
                        List.foldl (\( k, v ) acc -> MultiBiSeqDict.insert k v acc) MultiBiSeqDict.empty pairs

                    result =
                        MultiBiSeqDict.getAll num dict

                    expected =
                        pairs
                            |> List.filter (\( k, _ ) -> k == num)
                            |> List.map Tuple.second
                            |> SeqSet.fromList
                in
                Expect.equal result expected
        , fuzz2 fuzzPairs int "getReverse works" <|
            \pairs num ->
                let
                    dict =
                        List.foldl (\( k, v ) acc -> MultiBiSeqDict.insert k v acc) MultiBiSeqDict.empty pairs

                    result =
                        MultiBiSeqDict.getKeys num dict

                    expected =
                        pairs
                            |> List.filter (\( _, v ) -> v == num)
                            |> List.map Tuple.first
                            |> SeqSet.fromList
                in
                Expect.equal result expected
        , fuzz fuzzPairs "reverse index is consistent" <|
            \pairs ->
                let
                    dict =
                        List.foldl (\( k, v ) acc -> MultiBiSeqDict.insert k v acc) MultiBiSeqDict.empty pairs

                    checkConsistency ( k, values ) =
                        SeqSet.foldl
                            (\v acc ->
                                acc && SeqSet.member k (MultiBiSeqDict.getKeys v dict)
                            )
                            True
                            values
                in
                MultiBiSeqDict.toList dict
                    |> List.all checkConsistency
                    |> Expect.equal True
        , fuzz2 fuzzPairs int "insert works" <|
            \pairs num ->
                let
                    dict =
                        List.foldl (\( k, v ) acc -> MultiBiSeqDict.insert k v acc) MultiBiSeqDict.empty pairs
                            |> MultiBiSeqDict.insert num num

                    values =
                        MultiBiSeqDict.getAll num dict
                in
                Expect.equal (SeqSet.member num values) True
        , fuzz2 fuzzPairs int "remove works" <|
            \pairs num ->
                let
                    dict =
                        List.foldl (\( k, v ) acc -> MultiBiSeqDict.insert k v acc) MultiBiSeqDict.empty pairs
                            |> MultiBiSeqDict.remove num num

                    values =
                        MultiBiSeqDict.getAll num dict
                in
                Expect.equal (SeqSet.member num values) False
        , fuzz2 fuzzPairs int "remove maintains reverse index" <|
            \pairs num ->
                let
                    dict =
                        List.foldl (\( k, v ) acc -> MultiBiSeqDict.insert k v acc) MultiBiSeqDict.empty pairs
                            |> MultiBiSeqDict.remove num num

                    keys =
                        MultiBiSeqDict.getKeys num dict
                in
                Expect.equal (SeqSet.member num keys) False
        , fuzz2 fuzzPairs int "removeAll works" <|
            \pairs num ->
                let
                    dict =
                        List.foldl (\( k, v ) acc -> MultiBiSeqDict.insert k v acc) MultiBiSeqDict.empty pairs
                            |> MultiBiSeqDict.removeAll num
                in
                Expect.equal (MultiBiSeqDict.member num dict) False
        , fuzz2 fuzzPairs fuzzPairs "union works" <|
            \pairs1 pairs2 ->
                let
                    d1 =
                        List.foldl (\( k, v ) acc -> MultiBiSeqDict.insert k v acc) MultiBiSeqDict.empty pairs1

                    d2 =
                        List.foldl (\( k, v ) acc -> MultiBiSeqDict.insert k v acc) MultiBiSeqDict.empty pairs2

                    result =
                        MultiBiSeqDict.union d1 d2

                    unionKeys =
                        (pairs1 ++ pairs2)
                            |> List.map Tuple.first
                            |> SeqSet.fromList
                            |> SeqSet.size

                    resultKeys =
                        MultiBiSeqDict.keys result |> List.length
                in
                Expect.equal unionKeys resultKeys
        , fuzz fuzzPairs "size counts all pairs" <|
            \pairs ->
                let
                    dict =
                        List.foldl (\( k, v ) acc -> MultiBiSeqDict.insert k v acc) MultiBiSeqDict.empty pairs

                    uniquePairs =
                        pairs
                            |> SeqSet.fromList
                            |> SeqSet.size
                in
                Expect.equal (MultiBiSeqDict.size dict) uniquePairs
        , fuzz fuzzPairs "uniqueValuesCount is accurate" <|
            \pairs ->
                let
                    dict =
                        List.foldl (\( k, v ) acc -> MultiBiSeqDict.insert k v acc) MultiBiSeqDict.empty pairs

                    uniqueValues =
                        pairs
                            |> List.map Tuple.second
                            |> SeqSet.fromList
                            |> SeqSet.size
                in
                Expect.equal (MultiBiSeqDict.uniqueValuesCount dict) uniqueValues
        ]
