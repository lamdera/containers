module Tests exposing (tests)

import Array
import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int)
import Fuzz.Query
import List
import Random
import Random.List
import SeqDict exposing (SeqDict)
import SeqSet exposing (SeqSet)
import Set
import Test exposing (..)


animals : SeqDict String String
animals =
    SeqDict.fromList [ ( "Tom", "cat" ), ( "Jerry", "mouse" ) ]


type alias CollisionObject =
    { a : Int, b : String }


firstCollider : CollisionObject
firstCollider =
    { a = 449311, b = "449311" }


secondCollider : CollisionObject
secondCollider =
    { a = 989797, b = "989797" }


collisionObjects : List CollisionObject
collisionObjects =
    [ firstCollider
    , secondCollider
    ]


tests : Test
tests =
    let
        buildTests =
            describe "build Tests"
                [ test "empty" <|
                    \() -> Expect.equal (SeqDict.fromList []) SeqDict.empty
                , test "singleton" <|
                    \() -> Expect.equal (SeqDict.fromList [ ( "k", "v" ) ]) (SeqDict.singleton "k" "v")
                , test "insert" <|
                    \() -> Expect.equal (SeqDict.fromList [ ( "k", "v" ) ]) (SeqDict.insert "k" "v" SeqDict.empty)
                , test "insert replace" <|
                    \() -> Expect.equal (SeqDict.fromList [ ( "k", "vv" ) ]) (SeqDict.insert "k" "vv" (SeqDict.singleton "k" "v"))
                , test "update" <|
                    \() -> Expect.equal (SeqDict.fromList [ ( "k", "vv" ) ]) (SeqDict.update "k" (\v -> Just "vv") (SeqDict.singleton "k" "v"))
                , test "update Nothing" <|
                    \() ->
                        SeqDict.singleton "k" "v"
                            |> SeqDict.update "k" (\v -> Nothing)
                            |> SeqDict.toList
                            |> Expect.equal []
                , test "remove" <|
                    \() ->
                        SeqDict.singleton "k" "v"
                            |> SeqDict.remove "k"
                            |> SeqDict.toList
                            |> Expect.equal []
                , test "remove not found" <|
                    \() -> Expect.equal (SeqDict.singleton "k" "v") (SeqDict.remove "kk" (SeqDict.singleton "k" "v"))
                , test "fromList excludes duplicates" <|
                    \() -> Expect.equal (SeqDict.singleton 1 1) (SeqDict.fromList [ ( 1, 1 ), ( 1, 1 ) ])
                , test "size" <|
                    \() ->
                        SeqDict.empty
                            |> SeqDict.insert "k1" "v"
                            |> SeqDict.insert "k2" "v"
                            |> SeqDict.insert "k1" "y"
                            |> SeqDict.remove "k2"
                            |> SeqDict.size
                            |> Expect.equal 1
                ]

        queryTests =
            describe "query Tests"
                [ test "member 1" <|
                    \() -> Expect.equal True (SeqDict.member "Tom" animals)
                , test "member 2" <|
                    \() -> Expect.equal False (SeqDict.member "Spike" animals)
                , test "get 1" <|
                    \() -> Expect.equal (Just "cat") (SeqDict.get "Tom" animals)
                , test "get 2" <|
                    \() -> Expect.equal Nothing (SeqDict.get "Spike" animals)
                , test "size of empty dictionary" <|
                    \() -> Expect.equal 0 (SeqDict.size SeqDict.empty)
                , test "size of example dictionary" <|
                    \() -> Expect.equal 2 (SeqDict.size animals)
                ]

        combineTests =
            describe "combine Tests"
                [ test "union" <|
                    \() ->
                        Expect.equal
                            (SeqDict.toList animals)
                            (SeqDict.toList
                                (SeqDict.union
                                    (SeqDict.singleton "Jerry" "mouse")
                                    (SeqDict.singleton "Tom" "cat")
                                )
                            )
                , test "union collison" <|
                    \() ->
                        Expect.equal
                            (SeqDict.toList (SeqDict.singleton "Tom" "cat"))
                            (SeqDict.toList
                                (SeqDict.union
                                    (SeqDict.singleton "Tom" "cat")
                                    (SeqDict.singleton "Tom" "mouse")
                                )
                            )
                , test "intersect" <|
                    \() ->
                        Expect.equal
                            (SeqDict.toList (SeqDict.singleton "Tom" "cat"))
                            (SeqDict.toList
                                (SeqDict.intersect
                                    animals
                                    (SeqDict.singleton "Tom" "cat")
                                )
                            )
                , test "intersect collision" <|
                    \() ->
                        Expect.equal
                            (SeqDict.toList (SeqDict.singleton "Tom" "wolf"))
                            (SeqDict.toList
                                (SeqDict.intersect
                                    (SeqDict.singleton "Tom" "wolf")
                                    animals
                                )
                            )
                , test "diff" <|
                    \() ->
                        Expect.equal
                            (SeqDict.toList (SeqDict.singleton "Jerry" "mouse"))
                            (SeqDict.toList
                                (SeqDict.diff
                                    animals
                                    (SeqDict.singleton "Tom" "cat")
                                )
                            )
                ]

        transformTests =
            describe "transform Tests"
                [ test "filter" <|
                    \() -> Expect.equal (SeqDict.singleton "Tom" "cat") (SeqDict.filter (\k v -> k == "Tom") animals)
                , test "filter (numbers)" <|
                    \() ->
                        Expect.equal [ 2, 4, 6, 8, 10 ]
                            (List.range 1 10
                                |> List.indexedMap Tuple.pair
                                |> SeqDict.fromList
                                |> SeqDict.filter (\_ v -> modBy 2 v == 0)
                                |> SeqDict.toList
                                |> List.map Tuple.second
                            )
                , test "partition" <|
                    \() ->
                        Expect.equal
                            ( SeqDict.singleton "Tom" "cat", SeqDict.singleton "Jerry" "mouse" )
                            (SeqDict.partition (\k v -> k == "Tom") animals)
                , test "partition (numbers)" <|
                    \() ->
                        Expect.equal ( [ 2, 4, 6, 8, 10 ], [ 1, 3, 5, 7, 9 ] )
                            (List.range 1 10
                                |> List.indexedMap Tuple.pair
                                |> SeqDict.fromList
                                |> SeqDict.partition (\_ v -> modBy 2 v == 0)
                                |> (\( a, b ) ->
                                        ( SeqDict.toList a |> List.map Tuple.second
                                        , SeqDict.toList b |> List.map Tuple.second
                                        )
                                   )
                            )
                ]

        fuzzTests =
            describe "Fuzz tests"
                [ fuzz2 fuzzPairs int "Get works" <|
                    \pairs num ->
                        SeqDict.get num (SeqDict.fromList pairs)
                            |> Expect.equal (Dict.get num (Dict.fromList pairs))
                , fuzz fuzzPairs "Converting to/from list works" <|
                    \pairs ->
                        pairs
                            |> SeqDict.fromList
                            |> SeqDict.toList
                            |> List.sortBy Tuple.first
                            |> Expect.equal (Dict.toList (Dict.fromList pairs))
                , fuzz fuzzPairs "Insert order is maintained" <|
                    \pairs ->
                        let
                            deduped =
                                pairs
                                    |> List.map Tuple.first
                                    |> listUnique
                        in
                        pairs
                            |> SeqDict.fromList
                            |> SeqDict.keys
                            |> Expect.equal deduped
                , fuzz2 fuzzPairs int "Insert works" <|
                    \pairs num ->
                        SeqDict.insert num num (SeqDict.fromList pairs)
                            |> Expect.all
                                [ expectSynchronized
                                , expectEqualDict (Dict.insert num num (Dict.fromList pairs))
                                ]
                , fuzz2 fuzzPairs int "Removal works" <|
                    \pairs num ->
                        SeqDict.remove num (SeqDict.fromList pairs)
                            |> Expect.all
                                [ expectSynchronized
                                , expectEqualDict (Dict.remove num (Dict.fromList pairs))
                                ]
                , fuzz fuzzPairs "Map works" <|
                    \pairs ->
                        SeqDict.map (\k v -> k + v) (SeqDict.fromList pairs)
                            |> Expect.all
                                [ expectSynchronized
                                , expectEqualDict (Dict.map (\k v -> k + v) (Dict.fromList pairs))
                                ]
                , fuzz fuzzPairs "Filter works" <|
                    \pairs ->
                        SeqDict.filter (\k _ -> modBy 2 k == 0) (SeqDict.fromList pairs)
                            |> Expect.all
                                [ expectSynchronized
                                , expectEqualDict (Dict.filter (\k _ -> modBy 2 k == 0) (Dict.fromList pairs))
                                ]
                , fuzz2 fuzzPairs fuzzPairs "Union works" <|
                    \pairs pairs2 ->
                        SeqDict.union (SeqDict.fromList pairs) (SeqDict.fromList pairs2)
                            |> Expect.all
                                [ expectSynchronized
                                , expectEqualDict (Dict.union (Dict.fromList pairs) (Dict.fromList pairs2))
                                ]
                , fuzz2 fuzzPairs fuzzPairs "Intersect works" <|
                    \pairs pairs2 ->
                        SeqDict.intersect (SeqDict.fromList pairs) (SeqDict.fromList pairs2)
                            |> Expect.all
                                [ expectSynchronized
                                , expectEqualDict (Dict.intersect (Dict.fromList pairs) (Dict.fromList pairs2))
                                ]
                , fuzz2 fuzzPairs fuzzPairs "Diff works" <|
                    \pairs pairs2 ->
                        SeqDict.diff (SeqDict.fromList pairs) (SeqDict.fromList pairs2)
                            |> Expect.all
                                [ expectSynchronized
                                , expectEqualDict (Dict.diff (Dict.fromList pairs) (Dict.fromList pairs2))
                                ]
                ]

        collisionTests =
            describe "Collision tests"
                [ test "Insert" <|
                    \() ->
                        SeqSet.toList (SeqSet.fromList collisionObjects)
                            |> Expect.equal collisionObjects
                , test "Remove" <|
                    \() ->
                        SeqSet.toList (SeqSet.remove firstCollider (SeqSet.fromList collisionObjects))
                            |> Expect.equal [ secondCollider ]
                , test "Get" <|
                    \() ->
                        SeqSet.member secondCollider (SeqSet.fromList collisionObjects)
                            |> Expect.equal True
                ]

        equalityTests =
            describe "Order independent equality"
                [ test "Reverse input" <|
                    \() ->
                        let
                            input =
                                [ ( 1, "" ), ( 2, "" ), ( 3, "" ) ]
                        in
                        SeqDict.unorderedEquals
                            (SeqDict.fromList input)
                            (SeqDict.fromList (List.reverse input))
                            |> Expect.equal True
                , test "Not equal, missing item" <|
                    \() ->
                        SeqDict.unorderedEquals
                            (SeqDict.fromList [ ( 1, "" ), ( 2, "" ), ( 3, "" ) ])
                            (SeqDict.fromList [ ( 1, "" ), ( 2, "" ) ])
                            |> Expect.equal False
                , test "Not equal, value is different" <|
                    \() ->
                        SeqDict.unorderedEquals
                            (SeqDict.fromList [ ( 1, "" ), ( 2, "" ), ( 3, "" ) ])
                            (SeqDict.fromList [ ( 1, "" ), ( 2, "" ), ( 3, "a" ) ])
                            |> Expect.equal False
                , test "Handle hash collisions" <|
                    \() ->
                        let
                            key0 =
                                0

                            key1 =
                                933527461

                            key2 =
                                3399271663

                            --_ =
                            --    Debug.log "" (getCollision (key1 + 1))
                            --
                            --getCollision : Int -> Int
                            --getCollision index =
                            --    if SeqFNV.hash index == hash0 then
                            --        index
                            --
                            --    else
                            --        getCollision (index + 1)
                        in
                        SeqDict.unorderedEquals
                            (SeqDict.fromList [ ( key0, "0" ), ( key1, "1" ), ( key2, "2" ) ])
                            (SeqDict.fromList [ ( key1, "1" ), ( key0, "0" ), ( key2, "2" ) ])
                            |> Expect.equal True
                , test "Random list" <|
                    \() ->
                        let
                            key1 =
                                933527461

                            key2 =
                                3399271663

                            pairs : List ( Int, Int )
                            pairs =
                                List.range 0 100000
                                    |> List.map (\index -> ( index, index ))
                                    |> (\list -> ( key1, key1 ) :: ( key2, key2 ) :: list)

                            dict1 : SeqDict Int Int
                            dict1 =
                                Random.step (Random.List.shuffle pairs) (Random.initialSeed 321)
                                    |> Tuple.first
                                    |> SeqDict.fromList
                                    |> (\dict ->
                                            List.foldl
                                                (\index dict3 -> SeqDict.remove index dict3)
                                                dict
                                                (List.range 100 105)
                                       )

                            dict2 : SeqDict Int Int
                            dict2 =
                                Random.step (Random.List.shuffle pairs) (Random.initialSeed 123)
                                    |> Tuple.first
                                    |> List.filter (\( index, _ ) -> index < 100 || index > 105)
                                    |> SeqDict.fromList
                        in
                        SeqDict.unorderedEquals dict1 dict2 |> Expect.equal True
                , test "Not equal, random list" <|
                    \() ->
                        let
                            key1 =
                                933527461

                            key2 =
                                3399271663

                            pairs : List ( Int, Int )
                            pairs =
                                List.range 0 100000
                                    |> List.map (\index -> ( index, index ))
                                    |> (\list -> ( key1, key1 ) :: ( key2, key2 ) :: list)

                            dict1 : SeqDict Int Int
                            dict1 =
                                Random.step (Random.List.shuffle pairs) (Random.initialSeed 321)
                                    |> Tuple.first
                                    |> List.drop 1
                                    |> SeqDict.fromList

                            dict2 : SeqDict Int Int
                            dict2 =
                                Random.step (Random.List.shuffle pairs) (Random.initialSeed 123)
                                    |> Tuple.first
                                    |> SeqDict.fromList
                        in
                        SeqDict.unorderedEquals dict1 dict2 |> Expect.equal False
                , test "Insert bug" <|
                    \() ->
                        let
                            dict =
                                SeqDict.fromList [ ( 0, 0 ), ( 933527461, 933527461 ), ( 5, 5 ) ]
                        in
                        Expect.equal
                            ( Just 0, Just 933527461, Just 5 )
                            ( SeqDict.get 0 dict, SeqDict.get 933527461 dict, SeqDict.get 5 dict )
                , test "SeqDict as key" <|
                    \() ->
                        let
                            key1 =
                                SeqDict.fromList [ ( "a", 2 ) ]

                            key2 =
                                SeqDict.fromList [ ( "a", 2 ), ( "b", 3 ) ] |> SeqDict.remove "b"

                            dict : SeqDict (SeqDict String number) number
                            dict =
                                SeqDict.fromList [ ( key1, 1 ), ( key2, 2 ) ]
                        in
                        Expect.equal 1 (SeqDict.size dict)
                , test "SeqSet as key" <|
                    \() ->
                        let
                            key1 =
                                SeqSet.fromList [ "a" ]

                            key2 =
                                SeqSet.fromList [ "a", "b" ] |> SeqSet.remove "b"

                            dict : SeqDict (SeqSet String) number
                            dict =
                                SeqDict.fromList [ ( key1, 1 ), ( key2, 2 ) ]
                        in
                        Expect.equal 1 (SeqDict.size dict)
                , test "Dict as key" <|
                    \() ->
                        let
                            key1 =
                                Dict.fromList [ ( "a", 2 ) ]

                            key2 =
                                Dict.fromList [ ( "a", 2 ), ( "b", 3 ) ] |> Dict.remove "b"

                            dict : SeqDict (Dict.Dict String number) number
                            dict =
                                SeqDict.fromList [ ( key1, 1 ), ( key2, 2 ) ]
                        in
                        Expect.equal 1 (SeqDict.size dict)
                , test "Set as key" <|
                    \() ->
                        let
                            key1 =
                                Set.fromList [ "a" ]

                            key2 =
                                Set.fromList [ "a", "b" ] |> Set.remove "b"

                            dict : SeqDict (Set.Set String) number
                            dict =
                                SeqDict.fromList [ ( key1, 1 ), ( key2, 2 ) ]
                        in
                        Expect.equal 1 (SeqDict.size dict)
                , test "==" <|
                    \() ->
                        let
                            key1 =
                                933527461

                            key2 =
                                3399271663

                            pairs : List ( Int, Int )
                            pairs =
                                List.range 0 100000
                                    |> List.map (\index -> ( index, index ))
                                    |> (\list -> ( key1, key1 ) :: ( key2, key2 ) :: list)
                                    |> (\a -> Random.step (Random.List.shuffle a) (Random.initialSeed 321))
                                    |> Tuple.first

                            dict1 : SeqDict Int Int
                            dict1 =
                                List.foldl
                                    (\index dict3 -> SeqDict.remove index dict3)
                                    (SeqDict.fromList pairs)
                                    (List.range 100 500)

                            dict2 : SeqDict Int Int
                            dict2 =
                                pairs
                                    |> List.filter (\( index, _ ) -> index < 100 || index > 500)
                                    |> SeqDict.fromList
                        in
                        dict1 == dict2 |> Expect.equal True
                , test "Not ==" <|
                    \() ->
                        let
                            key1 =
                                933527461

                            key2 =
                                3399271663

                            pairs : List ( Int, Int )
                            pairs =
                                List.range 0 100000
                                    |> List.map (\index -> ( index, index ))
                                    |> (\list -> ( key1, key1 ) :: ( key2, key2 ) :: list)
                                    |> (\a -> Random.step (Random.List.shuffle a) (Random.initialSeed 321))
                                    |> Tuple.first

                            dict1 : SeqDict Int Int
                            dict1 =
                                List.foldl
                                    (\index dict3 -> SeqDict.remove index dict3)
                                    (SeqDict.fromList pairs)
                                    (List.range 100 500)

                            dict2 : SeqDict Int Int
                            dict2 =
                                pairs
                                    |> List.filter (\( index, _ ) -> index < 100 || index > 500)
                                    |> List.reverse
                                    |> SeqDict.fromList
                        in
                        dict1 == dict2 |> Expect.equal False

                --, test "Debug.toString SeqDict" <|
                --    \() ->
                --        SeqDict.fromList [ ( 1, 1 ) ] |> Debug.toString |> Expect.equal "SeqDict.fromList [(1,1)]"
                --, test "Debug.toString SeqSet" <|
                --    \() ->
                --        SeqSet.fromList [ 1 ] |> Debug.toString |> Expect.equal "SeqSet.fromList [1]"
                --, test "Debug.toString Dict" <|
                --    \() ->
                --        Dict.fromList [ ( 1, 1 ) ] |> Debug.toString |> Expect.equal "Dict.fromList [(1,1)]"
                --, test "Debug.toString Set" <|
                --    \() ->
                --        Set.fromList [ 1 ] |> Debug.toString |> Expect.equal "Set.fromList [1]"
                --, test "Debug.toString Array" <|
                --    \() ->
                --        Array.fromList [ ( 1, 1 ) ] |> Debug.toString |> Expect.equal "Array.fromList [(1,1)]"
                , test "Array ==" <|
                    \() ->
                        Array.fromList [ 1 ] == Array.fromList [ 1 ] |> Expect.equal True
                , test "Array not ==" <|
                    \() ->
                        Array.fromList [ 1 ] == Array.fromList [ 0 ] |> Expect.equal False
                , test "Set ==" <|
                    \() ->
                        Set.fromList [ 1 ] == Set.fromList [ 1 ] |> Expect.equal True
                , test "Set not ==" <|
                    \() ->
                        Set.fromList [ 1 ] == Set.fromList [ 0 ] |> Expect.equal False
                , test "Dict ==" <|
                    \() ->
                        Dict.fromList [ ( 1, 1 ) ] == Dict.fromList [ ( 1, 1 ) ] |> Expect.equal True
                , test "Dict not ==" <|
                    \() ->
                        Dict.fromList [ ( 1, 1 ) ] == Dict.fromList [ ( 1, 0 ) ] |> Expect.equal False
                , test "SeqSet ==" <|
                    \() ->
                        SeqSet.fromList [ 1 ] == SeqSet.fromList [ 1 ] |> Expect.equal True
                , test "SeqSet not ==" <|
                    \() ->
                        SeqSet.fromList [ 1 ] == SeqSet.fromList [ 0 ] |> Expect.equal False
                ]
    in
    describe "Dict Tests"
        [ buildTests
        , queryTests
        , combineTests
        , transformTests
        , fuzzTests
        , collisionTests
        , equalityTests
        ]



-- HELPERS


fuzzPairs : Fuzzer (List ( Int, Int ))
fuzzPairs =
    Fuzz.map2 Tuple.pair int int
        |> Fuzz.list


expectEqualDict : Dict.Dict comparable a -> SeqDict comparable a -> Expectation
expectEqualDict core hash =
    let
        listify key value acc =
            ( key, value ) :: acc

        coreList =
            Dict.foldr listify [] core

        hashList =
            SeqDict.toList hash
                |> List.sortBy Tuple.first
    in
    Expect.equal coreList hashList


expectSynchronized : SeqDict comparable a -> Expectation
expectSynchronized hash =
    SeqDict.foldl
        (\key value acc ->
            case SeqDict.get key hash of
                Just toCompare ->
                    if toCompare == value then
                        Dict.insert key value acc

                    else
                        acc

                Nothing ->
                    acc
        )
        Dict.empty
        hash
        |> (\toCompare -> expectEqualDict toCompare hash)


listUnique : List a -> List a
listUnique list =
    uniqueHelp identity [] list []


uniqueHelp : (a -> b) -> List b -> List a -> List a -> List a
uniqueHelp f existing remaining accumulator =
    case remaining of
        [] ->
            List.reverse accumulator

        first :: rest ->
            let
                computedFirst =
                    f first
            in
            if List.member computedFirst existing then
                uniqueHelp f existing rest accumulator

            else
                uniqueHelp f (computedFirst :: existing) rest (first :: accumulator)
