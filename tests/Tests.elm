module Tests exposing (tests)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int)
import List
import OrderedDict exposing (OrderedDict)
import OrderedSet
import Random
import Random.List
import Test exposing (..)


animals : OrderedDict String String
animals =
    OrderedDict.fromList [ ( "Tom", "cat" ), ( "Jerry", "mouse" ) ]


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
                    \() -> Expect.equal (OrderedDict.fromList []) OrderedDict.empty
                , test "singleton" <|
                    \() -> Expect.equal (OrderedDict.fromList [ ( "k", "v" ) ]) (OrderedDict.singleton "k" "v")
                , test "insert" <|
                    \() -> Expect.equal (OrderedDict.fromList [ ( "k", "v" ) ]) (OrderedDict.insert "k" "v" OrderedDict.empty)
                , test "insert replace" <|
                    \() -> Expect.equal (OrderedDict.fromList [ ( "k", "vv" ) ]) (OrderedDict.insert "k" "vv" (OrderedDict.singleton "k" "v"))
                , test "update" <|
                    \() -> Expect.equal (OrderedDict.fromList [ ( "k", "vv" ) ]) (OrderedDict.update "k" (\v -> Just "vv") (OrderedDict.singleton "k" "v"))
                , test "update Nothing" <|
                    \() ->
                        OrderedDict.singleton "k" "v"
                            |> OrderedDict.update "k" (\v -> Nothing)
                            |> OrderedDict.toList
                            |> Expect.equal []
                , test "remove" <|
                    \() ->
                        OrderedDict.singleton "k" "v"
                            |> OrderedDict.remove "k"
                            |> OrderedDict.toList
                            |> Expect.equal []
                , test "remove not found" <|
                    \() -> Expect.equal (OrderedDict.singleton "k" "v") (OrderedDict.remove "kk" (OrderedDict.singleton "k" "v"))
                , test "fromList excludes duplicates" <|
                    \() -> Expect.equal (OrderedDict.singleton 1 1) (OrderedDict.fromList [ ( 1, 1 ), ( 1, 1 ) ])
                , test "size" <|
                    \() ->
                        OrderedDict.empty
                            |> OrderedDict.insert "k1" "v"
                            |> OrderedDict.insert "k2" "v"
                            |> OrderedDict.insert "k1" "y"
                            |> OrderedDict.remove "k2"
                            |> OrderedDict.size
                            |> Expect.equal 1
                ]

        queryTests =
            describe "query Tests"
                [ test "member 1" <|
                    \() -> Expect.equal True (OrderedDict.member "Tom" animals)
                , test "member 2" <|
                    \() -> Expect.equal False (OrderedDict.member "Spike" animals)
                , test "get 1" <|
                    \() -> Expect.equal (Just "cat") (OrderedDict.get "Tom" animals)
                , test "get 2" <|
                    \() -> Expect.equal Nothing (OrderedDict.get "Spike" animals)
                , test "size of empty dictionary" <|
                    \() -> Expect.equal 0 (OrderedDict.size OrderedDict.empty)
                , test "size of example dictionary" <|
                    \() -> Expect.equal 2 (OrderedDict.size animals)
                ]

        combineTests =
            describe "combine Tests"
                [ test "union" <|
                    \() ->
                        Expect.equal
                            (OrderedDict.toList animals)
                            (OrderedDict.toList
                                (OrderedDict.union
                                    (OrderedDict.singleton "Jerry" "mouse")
                                    (OrderedDict.singleton "Tom" "cat")
                                )
                            )
                , test "union collison" <|
                    \() ->
                        Expect.equal
                            (OrderedDict.toList (OrderedDict.singleton "Tom" "cat"))
                            (OrderedDict.toList
                                (OrderedDict.union
                                    (OrderedDict.singleton "Tom" "cat")
                                    (OrderedDict.singleton "Tom" "mouse")
                                )
                            )
                , test "intersect" <|
                    \() ->
                        Expect.equal
                            (OrderedDict.toList (OrderedDict.singleton "Tom" "cat"))
                            (OrderedDict.toList
                                (OrderedDict.intersect
                                    animals
                                    (OrderedDict.singleton "Tom" "cat")
                                )
                            )
                , test "intersect collision" <|
                    \() ->
                        Expect.equal
                            (OrderedDict.toList (OrderedDict.singleton "Tom" "wolf"))
                            (OrderedDict.toList
                                (OrderedDict.intersect
                                    (OrderedDict.singleton "Tom" "wolf")
                                    animals
                                )
                            )
                , test "diff" <|
                    \() ->
                        Expect.equal
                            (OrderedDict.toList (OrderedDict.singleton "Jerry" "mouse"))
                            (OrderedDict.toList
                                (OrderedDict.diff
                                    animals
                                    (OrderedDict.singleton "Tom" "cat")
                                )
                            )
                ]

        transformTests =
            describe "transform Tests"
                [ test "filter" <|
                    \() -> Expect.equal (OrderedDict.singleton "Tom" "cat") (OrderedDict.filter (\k v -> k == "Tom") animals)
                , test "filter (numbers)" <|
                    \() ->
                        Expect.equal [ 2, 4, 6, 8, 10 ]
                            (List.range 1 10
                                |> List.indexedMap Tuple.pair
                                |> OrderedDict.fromList
                                |> OrderedDict.filter (\_ v -> modBy 2 v == 0)
                                |> OrderedDict.toList
                                |> List.map Tuple.second
                            )
                , test "partition" <|
                    \() ->
                        Expect.equal
                            ( OrderedDict.singleton "Tom" "cat", OrderedDict.singleton "Jerry" "mouse" )
                            (OrderedDict.partition (\k v -> k == "Tom") animals)
                , test "partition (numbers)" <|
                    \() ->
                        Expect.equal ( [ 2, 4, 6, 8, 10 ], [ 1, 3, 5, 7, 9 ] )
                            (List.range 1 10
                                |> List.indexedMap Tuple.pair
                                |> OrderedDict.fromList
                                |> OrderedDict.partition (\_ v -> modBy 2 v == 0)
                                |> (\( a, b ) ->
                                        ( OrderedDict.toList a |> List.map Tuple.second
                                        , OrderedDict.toList b |> List.map Tuple.second
                                        )
                                   )
                            )
                ]

        fuzzTests =
            describe "Fuzz tests"
                [ fuzz2 fuzzPairs int "Get works" <|
                    \pairs num ->
                        OrderedDict.get num (OrderedDict.fromList pairs)
                            |> Expect.equal (Dict.get num (Dict.fromList pairs))
                , fuzz fuzzPairs "Converting to/from list works" <|
                    \pairs ->
                        pairs
                            |> OrderedDict.fromList
                            |> OrderedDict.toList
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
                            |> OrderedDict.fromList
                            |> OrderedDict.keys
                            |> Expect.equal deduped
                , fuzz2 fuzzPairs int "Insert works" <|
                    \pairs num ->
                        OrderedDict.insert num num (OrderedDict.fromList pairs)
                            |> Expect.all
                                [ expectSynchronized
                                , expectEqualDict (Dict.insert num num (Dict.fromList pairs))
                                ]
                , fuzz2 fuzzPairs int "Removal works" <|
                    \pairs num ->
                        OrderedDict.remove num (OrderedDict.fromList pairs)
                            |> Expect.all
                                [ expectSynchronized
                                , expectEqualDict (Dict.remove num (Dict.fromList pairs))
                                ]
                , fuzz fuzzPairs "Map works" <|
                    \pairs ->
                        OrderedDict.map (\k v -> k + v) (OrderedDict.fromList pairs)
                            |> Expect.all
                                [ expectSynchronized
                                , expectEqualDict (Dict.map (\k v -> k + v) (Dict.fromList pairs))
                                ]
                , fuzz fuzzPairs "Filter works" <|
                    \pairs ->
                        OrderedDict.filter (\k _ -> modBy 2 k == 0) (OrderedDict.fromList pairs)
                            |> Expect.all
                                [ expectSynchronized
                                , expectEqualDict (Dict.filter (\k _ -> modBy 2 k == 0) (Dict.fromList pairs))
                                ]
                , fuzz2 fuzzPairs fuzzPairs "Union works" <|
                    \pairs pairs2 ->
                        OrderedDict.union (OrderedDict.fromList pairs) (OrderedDict.fromList pairs2)
                            |> Expect.all
                                [ expectSynchronized
                                , expectEqualDict (Dict.union (Dict.fromList pairs) (Dict.fromList pairs2))
                                ]
                , fuzz2 fuzzPairs fuzzPairs "Intersect works" <|
                    \pairs pairs2 ->
                        OrderedDict.intersect (OrderedDict.fromList pairs) (OrderedDict.fromList pairs2)
                            |> Expect.all
                                [ expectSynchronized
                                , expectEqualDict (Dict.intersect (Dict.fromList pairs) (Dict.fromList pairs2))
                                ]
                , fuzz2 fuzzPairs fuzzPairs "Diff works" <|
                    \pairs pairs2 ->
                        OrderedDict.diff (OrderedDict.fromList pairs) (OrderedDict.fromList pairs2)
                            |> Expect.all
                                [ expectSynchronized
                                , expectEqualDict (Dict.diff (Dict.fromList pairs) (Dict.fromList pairs2))
                                ]
                ]

        collisionTests =
            describe "Collision tests"
                [ test "Insert" <|
                    \() ->
                        OrderedSet.toList (OrderedSet.fromList collisionObjects)
                            |> Expect.equal collisionObjects
                , test "Remove" <|
                    \() ->
                        OrderedSet.toList (OrderedSet.remove firstCollider (OrderedSet.fromList collisionObjects))
                            |> Expect.equal [ secondCollider ]
                , test "Get" <|
                    \() ->
                        OrderedSet.member secondCollider (OrderedSet.fromList collisionObjects)
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
                        OrderedDict.unorderedEquals
                            (OrderedDict.fromList input)
                            (OrderedDict.fromList (List.reverse input))
                            |> Expect.equal True
                , test "Not equal, missing item" <|
                    \() ->
                        OrderedDict.unorderedEquals
                            (OrderedDict.fromList [ ( 1, "" ), ( 2, "" ), ( 3, "" ) ])
                            (OrderedDict.fromList [ ( 1, "" ), ( 2, "" ) ])
                            |> Expect.equal False
                , test "Not equal, value is different" <|
                    \() ->
                        OrderedDict.unorderedEquals
                            (OrderedDict.fromList [ ( 1, "" ), ( 2, "" ), ( 3, "" ) ])
                            (OrderedDict.fromList [ ( 1, "" ), ( 2, "" ), ( 3, "a" ) ])
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
                            --    if OrderedFNV.hash index == hash0 then
                            --        index
                            --
                            --    else
                            --        getCollision (index + 1)
                        in
                        OrderedDict.unorderedEquals
                            (OrderedDict.fromList [ ( key0, "0" ), ( key1, "1" ), ( key2, "2" ) ] |> Debug.log "a")
                            (OrderedDict.fromList [ ( key1, "1" ), ( key0, "0" ), ( key2, "2" ) ] |> Debug.log "b")
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

                            dict1 : OrderedDict Int Int
                            dict1 =
                                Random.step (Random.List.shuffle pairs) (Random.initialSeed 321)
                                    |> Tuple.first
                                    |> OrderedDict.fromList
                                    |> (\dict ->
                                            List.foldl
                                                (\index dict3 -> OrderedDict.remove index dict3)
                                                dict
                                                (List.range 100 105)
                                       )

                            dict2 : OrderedDict Int Int
                            dict2 =
                                Random.step (Random.List.shuffle pairs) (Random.initialSeed 123)
                                    |> Tuple.first
                                    |> List.filter (\( index, _ ) -> index < 100 || index > 105)
                                    |> OrderedDict.fromList
                        in
                        OrderedDict.unorderedEquals dict1 dict2 |> Expect.equal True
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

                            dict1 : OrderedDict Int Int
                            dict1 =
                                Random.step (Random.List.shuffle pairs) (Random.initialSeed 321)
                                    |> Tuple.first
                                    |> List.drop 1
                                    |> OrderedDict.fromList

                            dict2 : OrderedDict Int Int
                            dict2 =
                                Random.step (Random.List.shuffle pairs) (Random.initialSeed 123)
                                    |> Tuple.first
                                    |> OrderedDict.fromList
                        in
                        OrderedDict.unorderedEquals dict1 dict2 |> Expect.equal False
                ]
    in
    describe "Dict Tests"
        [ buildTests
        , queryTests
        , combineTests
        , transformTests
        , fuzzTests
        , collisionTests
        , Test.only equalityTests
        ]



-- HELPERS


fuzzPairs : Fuzzer (List ( Int, Int ))
fuzzPairs =
    Fuzz.map2 Tuple.pair int int
        |> Fuzz.list


expectEqualDict : Dict.Dict comparable a -> OrderedDict comparable a -> Expectation
expectEqualDict core hash =
    let
        listify key value acc =
            ( key, value ) :: acc

        coreList =
            Dict.foldr listify [] core

        hashList =
            OrderedDict.toList hash
                |> List.sortBy Tuple.first
    in
    Expect.equal coreList hashList


expectSynchronized : OrderedDict comparable a -> Expectation
expectSynchronized hash =
    OrderedDict.foldl
        (\key value acc ->
            case OrderedDict.get key hash of
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
