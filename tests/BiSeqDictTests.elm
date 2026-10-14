module BiSeqDictTests exposing (tests)

import BiSeqDict exposing (BiSeqDict)
import Expect
import Fuzz exposing (Fuzzer, int, list, tuple)
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


animals : BiSeqDict String String
animals =
    BiSeqDict.fromList [ ( "Tom", "cat" ), ( "Jerry", "mouse" ), ( "Spike", "cat" ) ]


fuzzPairs : Fuzzer (List ( Int, Int ))
fuzzPairs =
    list (tuple ( int, int ))


tests : Test
tests =
    describe "BiSeqDict"
        [ buildTests
        , queryTests
        , transformTests
        , combineTests
        , reverseTests
        , customTypeTests
        , fuzzTests
        ]


buildTests : Test
buildTests =
    describe "Build Tests"
        [ test "empty" <|
            \() -> Expect.equal (BiSeqDict.fromList []) BiSeqDict.empty
        , test "singleton" <|
            \() -> Expect.equal (BiSeqDict.fromList [ ( "k", "v" ) ]) (BiSeqDict.singleton "k" "v")
        , test "insert" <|
            \() -> Expect.equal (BiSeqDict.fromList [ ( "k", "v" ) ]) (BiSeqDict.insert "k" "v" BiSeqDict.empty)
        , test "insert replace" <|
            \() -> Expect.equal (BiSeqDict.fromList [ ( "k", "vv" ) ]) (BiSeqDict.insert "k" "vv" (BiSeqDict.singleton "k" "v"))
        , test "insert multiple keys same value" <|
            \() ->
                let
                    dict =
                        BiSeqDict.empty
                            |> BiSeqDict.insert "k1" "v"
                            |> BiSeqDict.insert "k2" "v"
                in
                Expect.equal (BiSeqDict.size dict) 2
        , test "remove" <|
            \() ->
                BiSeqDict.singleton "k" "v"
                    |> BiSeqDict.remove "k"
                    |> BiSeqDict.toList
                    |> Expect.equal []
        , test "remove not found" <|
            \() -> Expect.equal (BiSeqDict.singleton "k" "v") (BiSeqDict.remove "kk" (BiSeqDict.singleton "k" "v"))
        , test "fromList excludes duplicates" <|
            \() -> Expect.equal (BiSeqDict.singleton 1 1) (BiSeqDict.fromList [ ( 1, 1 ), ( 1, 1 ) ])
        , test "size" <|
            \() ->
                BiSeqDict.empty
                    |> BiSeqDict.insert "k1" "v"
                    |> BiSeqDict.insert "k2" "v"
                    |> BiSeqDict.insert "k1" "y"
                    |> BiSeqDict.remove "k2"
                    |> BiSeqDict.size
                    |> Expect.equal 1
        ]


queryTests : Test
queryTests =
    describe "Query Tests"
        [ test "member 1" <|
            \() -> Expect.equal True (BiSeqDict.member "Tom" animals)
        , test "member 2" <|
            \() -> Expect.equal False (BiSeqDict.member "NotThere" animals)
        , test "get 1" <|
            \() -> Expect.equal (Just "cat") (BiSeqDict.get "Tom" animals)
        , test "get 2" <|
            \() -> Expect.equal Nothing (BiSeqDict.get "NotThere" animals)
        , test "size of empty dictionary" <|
            \() -> Expect.equal 0 (BiSeqDict.size BiSeqDict.empty)
        , test "size of example dictionary" <|
            \() -> Expect.equal 3 (BiSeqDict.size animals)
        , test "isEmpty empty" <|
            \() -> Expect.equal True (BiSeqDict.isEmpty BiSeqDict.empty)
        , test "isEmpty non-empty" <|
            \() -> Expect.equal False (BiSeqDict.isEmpty animals)
        , test "keys" <|
            \() ->
                BiSeqDict.keys animals
                    |> Expect.equal [ "Tom", "Jerry", "Spike" ]
        , test "values" <|
            \() ->
                BiSeqDict.values animals
                    |> Expect.equal [ "cat", "mouse", "cat" ]
        , test "toList" <|
            \() ->
                BiSeqDict.toList animals
                    |> Expect.equal [ ( "Tom", "cat" ), ( "Jerry", "mouse" ), ( "Spike", "cat" ) ]
        ]


transformTests : Test
transformTests =
    describe "Transform Tests"
        [ test "map" <|
            \() ->
                BiSeqDict.map (\k v -> v ++ "!") animals
                    |> BiSeqDict.get "Tom"
                    |> Expect.equal (Just "cat!")
        , test "foldl" <|
            \() ->
                BiSeqDict.foldl (\k v acc -> acc ++ k) "" animals
                    |> Expect.equal "TomJerrySpike"
        , test "foldr" <|
            \() ->
                BiSeqDict.foldr (\k v acc -> acc ++ k) "" animals
                    |> Expect.equal "SpikJerryTom"
        , test "filter" <|
            \() ->
                BiSeqDict.filter (\k v -> v == "cat") animals
                    |> BiSeqDict.size
                    |> Expect.equal 2
        , test "partition" <|
            \() ->
                let
                    ( cats, others ) =
                        BiSeqDict.partition (\k v -> v == "cat") animals
                in
                Expect.equal ( BiSeqDict.size cats, BiSeqDict.size others ) ( 2, 1 )
        ]


combineTests : Test
combineTests =
    describe "Combine Tests"
        [ test "union" <|
            \() ->
                let
                    d1 =
                        BiSeqDict.singleton "a" "1"

                    d2 =
                        BiSeqDict.singleton "b" "2"

                    result =
                        BiSeqDict.union d1 d2
                in
                Expect.equal (BiSeqDict.size result) 2
        , test "union collision" <|
            \() ->
                let
                    d1 =
                        BiSeqDict.singleton "a" "1"

                    d2 =
                        BiSeqDict.singleton "a" "2"

                    result =
                        BiSeqDict.union d1 d2
                in
                Expect.equal (BiSeqDict.get "a" result) (Just "1")
        , test "intersect" <|
            \() ->
                let
                    d1 =
                        BiSeqDict.fromList [ ( "a", "1" ), ( "b", "2" ) ]

                    d2 =
                        BiSeqDict.singleton "a" "1"

                    result =
                        BiSeqDict.intersect d1 d2
                in
                Expect.equal (BiSeqDict.size result) 1
        , test "diff" <|
            \() ->
                let
                    d1 =
                        BiSeqDict.fromList [ ( "a", "1" ), ( "b", "2" ) ]

                    d2 =
                        BiSeqDict.singleton "a" "1"

                    result =
                        BiSeqDict.diff d1 d2
                in
                Expect.equal (BiSeqDict.toList result) [ ( "b", "2" ) ]
        ]


reverseTests : Test
reverseTests =
    describe "Reverse Lookup Tests"
        [ test "getReverse single key" <|
            \() ->
                let
                    dict =
                        BiSeqDict.singleton "Tom" "cat"

                    result =
                        BiSeqDict.getKeys "cat" dict
                in
                Expect.equal (SeqSet.size result) 1
        , test "getReverse multiple keys" <|
            \() ->
                let
                    result =
                        BiSeqDict.getKeys "cat" animals
                in
                Expect.equal (SeqSet.size result) 2
        , test "getReverse not found" <|
            \() ->
                let
                    result =
                        BiSeqDict.getKeys "dog" animals
                in
                Expect.equal (SeqSet.size result) 0
        , test "getReverse after insert" <|
            \() ->
                let
                    dict =
                        BiSeqDict.empty
                            |> BiSeqDict.insert "k1" "v"
                            |> BiSeqDict.insert "k2" "v"
                            |> BiSeqDict.insert "k3" "v"

                    result =
                        BiSeqDict.getKeys "v" dict
                in
                Expect.equal (SeqSet.size result) 3
        , test "getReverse after remove" <|
            \() ->
                let
                    dict =
                        BiSeqDict.fromList [ ( "Tom", "cat" ), ( "Spike", "cat" ) ]
                            |> BiSeqDict.remove "Tom"

                    result =
                        BiSeqDict.getKeys "cat" dict
                in
                Expect.equal (SeqSet.size result) 1
        , test "reverse index consistency after replace" <|
            \() ->
                let
                    dict =
                        BiSeqDict.singleton "k" "old"
                            |> BiSeqDict.insert "k" "new"

                    oldResult =
                        BiSeqDict.getKeys "old" dict

                    newResult =
                        BiSeqDict.getKeys "new" dict
                in
                Expect.equal ( SeqSet.size oldResult, SeqSet.size newResult ) ( 0, 1 )
        ]


customTypeTests : Test
customTypeTests =
    describe "Custom Type Tests (no comparable constraint)"
        [ test "can create and insert custom types" <|
            \() ->
                let
                    dict =
                        BiSeqDict.empty
                            |> BiSeqDict.insert Foo "hello"
                            |> BiSeqDict.insert Bar "world"
                            |> BiSeqDict.insert Baz "hello"
                in
                Expect.equal (BiSeqDict.size dict) 3
        , test "getReverse works with custom types" <|
            \() ->
                let
                    dict =
                        BiSeqDict.empty
                            |> BiSeqDict.insert Foo "hello"
                            |> BiSeqDict.insert Bar "world"
                            |> BiSeqDict.insert Baz "hello"

                    result =
                        BiSeqDict.getKeys "hello" dict
                in
                Expect.equal (SeqSet.size result) 2
        , test "can use custom records as values" <|
            \() ->
                let
                    dict =
                        BiSeqDict.empty
                            |> BiSeqDict.insert "alice" { name = "Alice", value = 1 }
                            |> BiSeqDict.insert "bob" { name = "Bob", value = 2 }
                            |> BiSeqDict.insert "charlie" { name = "Alice", value = 1 }

                    record =
                        { name = "Alice", value = 1 }

                    reverseKeys =
                        BiSeqDict.getKeys record dict
                in
                Expect.equal (SeqSet.size reverseKeys) 2
        ]


fuzzTests : Test
fuzzTests =
    describe "Fuzz Tests"
        [ fuzz2 fuzzPairs int "get works" <|
            \pairs num ->
                let
                    dict =
                        BiSeqDict.fromList pairs

                    result =
                        BiSeqDict.get num dict

                    expected =
                        pairs
                            |> List.filter (\( k, _ ) -> k == num)
                            |> List.head
                            |> Maybe.map Tuple.second
                in
                Expect.equal result expected
        , fuzz fuzzPairs "fromList and toList roundtrip" <|
            \pairs ->
                let
                    dict =
                        BiSeqDict.fromList pairs

                    uniquePairs =
                        pairs
                            |> List.foldl
                                (\( k, v ) acc ->
                                    if List.any (\( k2, _ ) -> k2 == k) acc then
                                        acc

                                    else
                                        acc ++ [ ( k, v ) ]
                                )
                                []
                in
                BiSeqDict.toList dict
                    |> Expect.equal uniquePairs
        , fuzz2 fuzzPairs int "insert works" <|
            \pairs num ->
                let
                    dict =
                        BiSeqDict.fromList pairs
                            |> BiSeqDict.insert num num
                in
                BiSeqDict.get num dict
                    |> Expect.equal (Just num)
        , fuzz2 fuzzPairs int "remove works" <|
            \pairs num ->
                let
                    dict =
                        BiSeqDict.fromList pairs
                            |> BiSeqDict.remove num
                in
                BiSeqDict.get num dict
                    |> Expect.equal Nothing
        , fuzz fuzzPairs "reverse index is consistent" <|
            \pairs ->
                let
                    dict =
                        BiSeqDict.fromList pairs

                    checkConsistency ( k, v ) =
                        BiSeqDict.getKeys v dict
                            |> SeqSet.member k
                in
                BiSeqDict.toList dict
                    |> List.all checkConsistency
                    |> Expect.equal True
        , fuzz2 fuzzPairs fuzzPairs "union works" <|
            \pairs1 pairs2 ->
                let
                    d1 =
                        BiSeqDict.fromList pairs1

                    d2 =
                        BiSeqDict.fromList pairs2

                    result =
                        BiSeqDict.union d1 d2

                    expectedSize =
                        (pairs1 ++ pairs2)
                            |> List.map Tuple.first
                            |> List.foldl
                                (\k acc ->
                                    if List.member k acc then
                                        acc

                                    else
                                        k :: acc
                                )
                                []
                            |> List.length
                in
                BiSeqDict.size result
                    |> Expect.equal expectedSize
        ]
