module Fuzz.OldThenNew exposing (suite)

{-| Builds a dict with SeqDictOld, which may leave it broken (keys that toList
lists but get can't find, keys listed twice, collision nodes with the wrong
hash), and then carries on with SeqDict. The new code can't repair what's
already broken, but it mustn't break anything further.
-}

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Fuzz.Collisions exposing (Key, Op(..), Value, allKeys, keep, keyFuzzer, modelFromList, opFuzzer, valueFuzzer)
import SeqDict exposing (SeqDict)
import SeqDictOld
import Test exposing (Test, describe, fuzz2)
import Test.Distribution


applyOldOp : Op -> SeqDict Key Value -> SeqDict Key Value
applyOldOp op dict =
    case op of
        Insert key value ->
            SeqDictOld.insert key value dict

        Remove key ->
            SeqDictOld.remove key dict

        UpdateToJust key value ->
            SeqDictOld.update key (\_ -> Just value) dict

        UpdateToNothing key ->
            SeqDictOld.update key (\_ -> Nothing) dict

        UpdateValue key value ->
            SeqDictOld.update key (Maybe.map ((+) value)) dict

        UpdateIfExists key value ->
            SeqDictOld.updateIfExists key ((+) value) dict

        Map n ->
            SeqDictOld.map (\_ value -> value + n) dict

        Filter n ->
            SeqDictOld.filter (keep n) dict

        FilterMap n ->
            SeqDictOld.filterMap
                (\key value ->
                    if keep n key value then
                        Just (value * 2)

                    else
                        Nothing
                )
                dict

        PartitionFirst n ->
            SeqDictOld.partition (keep n) dict |> Tuple.first

        PartitionSecond n ->
            SeqDictOld.partition (keep n) dict |> Tuple.second

        UnionWith pairs ->
            SeqDictOld.union dict (SeqDictOld.fromList pairs)

        UnionInto pairs ->
            SeqDictOld.union (SeqDictOld.fromList pairs) dict

        IntersectWith pairs ->
            SeqDictOld.intersect dict (SeqDictOld.fromList pairs)

        DiffWith pairs ->
            SeqDictOld.diff dict (SeqDictOld.fromList pairs)

        MergeWith pairs ->
            SeqDictOld.merge
                SeqDictOld.insert
                (\key left right acc -> SeqDictOld.insert key (left - right) acc)
                SeqDictOld.insert
                dict
                (SeqDictOld.fromList pairs)
                SeqDictOld.empty


{-| Mostly inserts and removes, since those are what break the dict.
-}
oldDictFuzzer : Fuzzer (SeqDict Key Value)
oldDictFuzzer =
    Fuzz.frequency
        [ ( 8, Fuzz.map2 Insert keyFuzzer valueFuzzer )
        , ( 5, Fuzz.map Remove keyFuzzer )
        , ( 2, Fuzz.map2 UpdateValue keyFuzzer valueFuzzer )
        , ( 2, opFuzzer )
        ]
        |> Fuzz.listOfLengthBetween 0 80
        |> Fuzz.map (List.foldl applyOldOp SeqDictOld.empty)


{-| Only the operations that never remove a key, so the only damage is from
SeqDictOld.insert dropping keys that share a hash.
-}
oldInsertsOnlyDictFuzzer : Fuzzer (SeqDict Key Value)
oldInsertsOnlyDictFuzzer =
    Fuzz.frequency
        [ ( 8, Fuzz.map2 Insert keyFuzzer valueFuzzer )
        , ( 1, Fuzz.map2 UpdateToJust keyFuzzer valueFuzzer )
        , ( 1, Fuzz.map2 UpdateIfExists keyFuzzer valueFuzzer )
        ]
        |> Fuzz.listOfLengthBetween 0 80
        |> Fuzz.map (List.foldl applyOldOp SeqDictOld.empty)


hasLostKey : SeqDict Key Value -> Bool
hasLostKey dict =
    List.any (\( key, _ ) -> SeqDict.get key dict == Nothing) (SeqDict.toList dict)


{-| An operation on a single key, so every other key can be checked for
collateral damage.
-}
type KeyOp
    = KeyInsert Key Value
    | KeyRemove Key
    | KeyUpdateToJust Key Value
    | KeyUpdateToNothing Key
    | KeyUpdateValue Key Value
    | KeyUpdateIfExists Key Value


keyOpFuzzer : Fuzzer KeyOp
keyOpFuzzer =
    Fuzz.frequency
        [ ( 4, Fuzz.map2 KeyInsert keyFuzzer valueFuzzer )
        , ( 3, Fuzz.map KeyRemove keyFuzzer )
        , ( 1, Fuzz.map2 KeyUpdateToJust keyFuzzer valueFuzzer )
        , ( 1, Fuzz.map KeyUpdateToNothing keyFuzzer )
        , ( 1, Fuzz.map2 KeyUpdateValue keyFuzzer valueFuzzer )
        , ( 1, Fuzz.map2 KeyUpdateIfExists keyFuzzer valueFuzzer )
        ]


applyKeyOp : KeyOp -> SeqDict Key Value -> SeqDict Key Value
applyKeyOp op dict =
    case op of
        KeyInsert key value ->
            SeqDict.insert key value dict

        KeyRemove key ->
            SeqDict.remove key dict

        KeyUpdateToJust key value ->
            SeqDict.update key (\_ -> Just value) dict

        KeyUpdateToNothing key ->
            SeqDict.update key (\_ -> Nothing) dict

        KeyUpdateValue key value ->
            SeqDict.update key (Maybe.map ((+) value)) dict

        KeyUpdateIfExists key value ->
            SeqDict.updateIfExists key ((+) value) dict


applyOldKeyOp : KeyOp -> SeqDict Key Value -> SeqDict Key Value
applyOldKeyOp op dict =
    case op of
        KeyInsert key value ->
            SeqDictOld.insert key value dict

        KeyRemove key ->
            SeqDictOld.remove key dict

        KeyUpdateToJust key value ->
            SeqDictOld.update key (\_ -> Just value) dict

        KeyUpdateToNothing key ->
            SeqDictOld.update key (\_ -> Nothing) dict

        KeyUpdateValue key value ->
            SeqDictOld.update key (Maybe.map ((+) value)) dict

        KeyUpdateIfExists key value ->
            SeqDictOld.updateIfExists key ((+) value) dict


{-| The key the operation is about, and what get should return for it afterwards.
-}
expectedGet : KeyOp -> SeqDict Key Value -> ( Key, Maybe Value )
expectedGet op dict =
    case op of
        KeyInsert key value ->
            ( key, Just value )

        KeyRemove key ->
            ( key, Nothing )

        KeyUpdateToJust key value ->
            ( key, Just value )

        KeyUpdateToNothing key ->
            ( key, Nothing )

        KeyUpdateValue key value ->
            ( key, Maybe.map ((+) value) (SeqDict.get key dict) )

        KeyUpdateIfExists key value ->
            ( key, Maybe.map ((+) value) (SeqDict.get key dict) )


{-| The value fromList would end up with for a key toList lists: the last one.
-}
lastListed : Key -> SeqDict Key Value -> Maybe Value
lastListed key dict =
    SeqDict.toList dict
        |> List.filter (\( k, _ ) -> k == key)
        |> List.reverse
        |> List.head
        |> Maybe.map Tuple.second


{-| Keys other than the one the operation is about whose get changed. A key get
couldn't find may come back (remove sometimes rebuilds the dict from toList), as
long as it comes back with the value toList has for it.
-}
collateralDamage : Key -> SeqDict Key Value -> SeqDict Key Value -> List ( Key, Maybe Value, Maybe Value )
collateralDamage touched before after =
    List.filterMap
        (\key ->
            let
                was =
                    SeqDict.get key before

                is =
                    SeqDict.get key after
            in
            if key == touched || was == is then
                Nothing

            else if was == Nothing && is == lastListed key before then
                Nothing

            else
                Just ( key, was, is )
        )
        allKeys


{-| Everything get finds is in toList, and size matches toList.
-}
expectConsistent : SeqDict Key Value -> Expectation
expectConsistent dict =
    Expect.all
        [ \_ ->
            List.filterMap
                (\key ->
                    case SeqDict.get key dict of
                        Just value ->
                            if List.member ( key, value ) (SeqDict.toList dict) then
                                Nothing

                            else
                                Just ( key, value )

                        Nothing ->
                            Nothing
                )
                allKeys
                |> Expect.equal []
        , \_ -> SeqDict.size dict |> Expect.equal (List.length (SeqDict.toList dict))
        ]
        ()


expectKeyOpIsSafe : KeyOp -> SeqDict Key Value -> Expectation
expectKeyOpIsSafe op before =
    let
        after =
            applyKeyOp op before

        ( touched, expected ) =
            expectedGet op before
    in
    Expect.all
        [ \_ -> SeqDict.get touched after |> Expect.equal expected
        , \_ -> collateralDamage touched before after |> Expect.equal []
        , \_ -> expectConsistent after
        , \_ ->
            -- Replacing the value of a key get can find doesn't add an entry
            case ( op, SeqDict.get touched before ) of
                ( KeyInsert _ _, Just _ ) ->
                    List.length (SeqDict.toList after) |> Expect.equal (List.length (SeqDict.toList before))

                _ ->
                    Expect.pass
        ]
        ()


expectKeyOpsAreSafe : SeqDict Key Value -> List KeyOp -> Expectation
expectKeyOpsAreSafe start ops =
    let
        befores : List ( KeyOp, SeqDict Key Value )
        befores =
            List.foldl
                (\op ( dict, acc ) -> ( applyKeyOp op dict, ( op, dict ) :: acc ))
                ( start, [] )
                ops
                |> Tuple.second
                |> List.reverse
    in
    Expect.all
        (List.map (\( op, dict ) () -> expectKeyOpIsSafe op dict) befores)
        ()


{-| A dict SeqDictOld.remove has damaged can have a collision node with the wrong
hash, which get doesn't notice. Inserting a key that reaches that node splits it by
the wrong hash and loses the keys in it, in SeqDictOld and SeqDict alike. So for
those dicts this only checks that SeqDict never does worse than SeqDictOld would
have done with the same dict.
-}
expectKeyOpIsNoWorseThanOld : KeyOp -> SeqDict Key Value -> Expectation
expectKeyOpIsNoWorseThanOld op before =
    let
        after =
            applyKeyOp op before

        afterOld =
            applyOldKeyOp op before

        ( touched, expected ) =
            expectedGet op before

        damagedKeys dict =
            collateralDamage touched before dict |> List.map (\( key, _, _ ) -> key)
    in
    Expect.all
        [ \_ ->
            if SeqDict.get touched after == expected || SeqDict.get touched afterOld /= expected then
                Expect.pass

            else
                SeqDict.get touched after |> Expect.equal expected
        , \_ ->
            List.filter (\key -> not (List.member key (damagedKeys afterOld))) (damagedKeys after)
                |> Expect.equal []
        ]
        ()


expectKeyOpsAreNoWorseThanOld : SeqDict Key Value -> List KeyOp -> Expectation
expectKeyOpsAreNoWorseThanOld start ops =
    let
        befores : List ( KeyOp, SeqDict Key Value )
        befores =
            List.foldl
                (\op ( dict, acc ) -> ( applyKeyOp op dict, ( op, dict ) :: acc ))
                ( start, [] )
                ops
                |> Tuple.second
                |> List.reverse
    in
    Expect.all
        (List.map (\( op, dict ) () -> expectKeyOpIsNoWorseThanOld op dict) befores)
        ()


{-| What a rebuild from toList should give: each key once, where it first
appears, with the last value listed for it.
-}
expectRepaired : SeqDict Key Value -> SeqDict Key Value -> Expectation
expectRepaired old rebuilt =
    let
        expected =
            modelFromList (SeqDict.toList old)
    in
    Expect.all
        [ \_ -> SeqDict.toList rebuilt |> Expect.equal expected
        , \_ -> SeqDict.size rebuilt |> Expect.equal (List.length expected)
        , \_ ->
            List.map (\key -> ( key, SeqDict.get key rebuilt )) allKeys
                |> Expect.equal
                    (List.map
                        (\key ->
                            ( key
                            , List.filter (\( k, _ ) -> k == key) expected |> List.head |> Maybe.map Tuple.second
                            )
                        )
                        allKeys
                    )
        ]
        ()


suite : Test
suite =
    describe "A dict built by SeqDictOld, continued with SeqDict"
        [ Test.fuzzWith
            { runs = 500
            , distribution =
                Test.expectDistribution
                    [ ( Test.Distribution.atLeast 15, "the old code lost a key", \( old, _ ) -> hasLostKey old ) ]
            }
            (Fuzz.pair oldInsertsOnlyDictFuzzer (Fuzz.listOfLengthBetween 1 30 keyOpFuzzer))
            "After SeqDictOld.insert damage, insert, remove and update don't damage other keys"
            (\( old, ops ) -> expectKeyOpsAreSafe old ops)
        , Test.fuzzWith
            { runs = 500
            , distribution =
                Test.expectDistribution
                    [ ( Test.Distribution.atLeast 25, "the old code lost a key", \( old, _ ) -> hasLostKey old ) ]
            }
            (Fuzz.pair oldDictFuzzer (Fuzz.listOfLengthBetween 1 30 keyOpFuzzer))
            "After any SeqDictOld damage, insert, remove and update never do worse than SeqDictOld"
            (\( old, ops ) -> expectKeyOpsAreNoWorseThanOld old ops)
        , Test.test "Removing one key of a collision node whose hash SeqDictOld overwrote, then setting the other, keeps one entry" <|
            \_ ->
                let
                    -- "180" isn't in the dict but reaches the node holding "698" and "237607",
                    -- so SeqDictOld.remove stores 180's hash on that node.
                    old =
                        SeqDictOld.empty
                            |> SeqDictOld.insert "698" 1
                            |> SeqDictOld.insert "237607" 2
                            |> SeqDictOld.remove "180"

                    new =
                        old
                            |> SeqDict.remove "698"
                            |> SeqDict.insert "237607" 3
                in
                Expect.all
                    [ \_ -> SeqDict.toList new |> Expect.equal [ ( "237607", 3 ) ]
                    , \_ -> SeqDict.get "237607" new |> Expect.equal (Just 3)
                    , \_ -> SeqDict.size new |> Expect.equal 1
                    ]
                    ()
        , fuzz2 oldDictFuzzer
            valueFuzzer
            "map keeps what get and toList return"
            (\old n ->
                let
                    mapped =
                        SeqDict.map (\_ value -> value + n) old
                in
                Expect.all
                    [ \_ ->
                        List.map (\key -> SeqDict.get key mapped) allKeys
                            |> Expect.equal (List.map (\key -> SeqDict.get key old |> Maybe.map ((+) n)) allKeys)
                    , \_ ->
                        SeqDict.toList mapped
                            |> Expect.equal (List.map (\( key, value ) -> ( key, value + n )) (SeqDict.toList old))
                    ]
                    ()
            )
        , describe "Rebuilding from toList repairs the dict"
            [ Test.fuzz oldDictFuzzer "fromList" <|
                \old -> expectRepaired old (SeqDict.fromList (SeqDict.toList old))
            , Test.fuzz oldDictFuzzer "filter" <|
                \old -> expectRepaired old (SeqDict.filter (\_ _ -> True) old)
            , Test.fuzz oldDictFuzzer "filterMap" <|
                \old -> expectRepaired old (SeqDict.filterMap (\_ value -> Just value) old)
            , Test.fuzz oldDictFuzzer "partition" <|
                \old -> expectRepaired old (SeqDict.partition (\_ _ -> True) old |> Tuple.first)
            , Test.fuzz oldDictFuzzer "union into empty" <|
                \old -> expectRepaired old (SeqDict.union old SeqDict.empty)
            ]
        ]
