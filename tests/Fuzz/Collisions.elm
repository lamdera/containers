module Fuzz.Collisions exposing (Key, Op(..), Value, allKeys, keep, keyFuzzer, modelFromList, opFuzzer, suite, valueFuzzer)

{-| Applies random sequences of SeqDict operations to keys that mostly share a
hash with other keys, and after every operation compares the dict to a plain
association list that is updated the same way.
-}

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import SeqDict exposing (SeqDict)
import Test exposing (Test, describe, fuzz)


type alias Key =
    String


type alias Value =
    Int


{-| The keys in each group have the same hash.
-}
collidingKeyGroups : List (List Key)
collidingKeyGroups =
    [ [ "4446438", "9083548", "15750573", "33152112", "38891445" ]
    , [ "6953243", "11566812", "14249809", "22225888" ]
    , [ "6035280", "6797635", "22704722", "24624050" ]
    , [ "698", "237607" ]
    , [ "94476", "106248" ]
    ]


{-| Keys whose hash shares its lowest 10, 20 or 25 bits with the first or the
fourth group, so collisions end up inside subtrees of different depths.
-}
nearlyCollidingKeys : List Key
nearlyCollidingKeys =
    [ "397", "1034719", "19053682", "180", "200827", "4559384" ]


otherKeys : List Key
otherKeys =
    List.range 0 9 |> List.map String.fromInt


allKeys : List Key
allKeys =
    List.concat collidingKeyGroups ++ nearlyCollidingKeys ++ otherKeys


keyFuzzer : Fuzzer Key
keyFuzzer =
    Fuzz.frequency
        [ ( 6, Fuzz.oneOfValues (List.concat collidingKeyGroups) )
        , ( 2, Fuzz.oneOfValues nearlyCollidingKeys )
        , ( 1, Fuzz.oneOfValues otherKeys )
        ]


valueFuzzer : Fuzzer Value
valueFuzzer =
    Fuzz.intRange 0 100


pairsFuzzer : Fuzzer (List ( Key, Value ))
pairsFuzzer =
    Fuzz.listOfLengthBetween 0 10 (Fuzz.pair keyFuzzer valueFuzzer)


type Op
    = Insert Key Value
    | Remove Key
    | UpdateToJust Key Value
    | UpdateToNothing Key
    | UpdateValue Key Value
    | UpdateIfExists Key Value
    | Map Value
    | Filter Value
    | FilterMap Value
    | PartitionFirst Value
    | PartitionSecond Value
    | UnionWith (List ( Key, Value ))
    | UnionInto (List ( Key, Value ))
    | IntersectWith (List ( Key, Value ))
    | DiffWith (List ( Key, Value ))
    | MergeWith (List ( Key, Value ))


opFuzzer : Fuzzer Op
opFuzzer =
    Fuzz.frequency
        [ ( 6, Fuzz.map2 Insert keyFuzzer valueFuzzer )
        , ( 3, Fuzz.map Remove keyFuzzer )
        , ( 2, Fuzz.map2 UpdateToJust keyFuzzer valueFuzzer )
        , ( 1, Fuzz.map UpdateToNothing keyFuzzer )
        , ( 2, Fuzz.map2 UpdateValue keyFuzzer valueFuzzer )
        , ( 2, Fuzz.map2 UpdateIfExists keyFuzzer valueFuzzer )
        , ( 1, Fuzz.map Map valueFuzzer )
        , ( 1, Fuzz.map Filter valueFuzzer )
        , ( 1, Fuzz.map FilterMap valueFuzzer )
        , ( 1, Fuzz.map PartitionFirst valueFuzzer )
        , ( 1, Fuzz.map PartitionSecond valueFuzzer )
        , ( 1, Fuzz.map UnionWith pairsFuzzer )
        , ( 1, Fuzz.map UnionInto pairsFuzzer )
        , ( 1, Fuzz.map IntersectWith pairsFuzzer )
        , ( 1, Fuzz.map DiffWith pairsFuzzer )
        , ( 1, Fuzz.map MergeWith pairsFuzzer )
        ]


keep : Value -> Key -> Value -> Bool
keep n _ value =
    modBy 3 (value + n) /= 0


applyOp : Op -> SeqDict Key Value -> SeqDict Key Value
applyOp op dict =
    case op of
        Insert key value ->
            SeqDict.insert key value dict

        Remove key ->
            SeqDict.remove key dict

        UpdateToJust key value ->
            SeqDict.update key (\_ -> Just value) dict

        UpdateToNothing key ->
            SeqDict.update key (\_ -> Nothing) dict

        UpdateValue key value ->
            SeqDict.update key (Maybe.map ((+) value)) dict

        UpdateIfExists key value ->
            SeqDict.updateIfExists key ((+) value) dict

        Map n ->
            SeqDict.map (\_ value -> value + n) dict

        Filter n ->
            SeqDict.filter (keep n) dict

        FilterMap n ->
            SeqDict.filterMap
                (\key value ->
                    if keep n key value then
                        Just (value * 2)

                    else
                        Nothing
                )
                dict

        PartitionFirst n ->
            SeqDict.partition (keep n) dict |> Tuple.first

        PartitionSecond n ->
            SeqDict.partition (keep n) dict |> Tuple.second

        UnionWith pairs ->
            SeqDict.union dict (SeqDict.fromList pairs)

        UnionInto pairs ->
            SeqDict.union (SeqDict.fromList pairs) dict

        IntersectWith pairs ->
            SeqDict.intersect dict (SeqDict.fromList pairs)

        DiffWith pairs ->
            SeqDict.diff dict (SeqDict.fromList pairs)

        MergeWith pairs ->
            SeqDict.merge
                SeqDict.insert
                (\key left right acc -> SeqDict.insert key (left - right) acc)
                SeqDict.insert
                dict
                (SeqDict.fromList pairs)
                SeqDict.empty


{-| The expected contents of the dict, in insertion order.
-}
type alias Model =
    List ( Key, Value )


modelGet : Key -> Model -> Maybe Value
modelGet key model =
    case List.filter (\( k, _ ) -> k == key) model of
        ( _, value ) :: _ ->
            Just value

        [] ->
            Nothing


modelInsert : Key -> Value -> Model -> Model
modelInsert key value model =
    if List.any (\( k, _ ) -> k == key) model then
        List.map
            (\( k, v ) ->
                if k == key then
                    ( k, value )

                else
                    ( k, v )
            )
            model

    else
        model ++ [ ( key, value ) ]


modelRemove : Key -> Model -> Model
modelRemove key model =
    List.filter (\( k, _ ) -> k /= key) model


modelUpdate : Key -> (Maybe Value -> Maybe Value) -> Model -> Model
modelUpdate key fn model =
    case fn (modelGet key model) of
        Just value ->
            modelInsert key value model

        Nothing ->
            modelRemove key model


modelFromList : List ( Key, Value ) -> Model
modelFromList pairs =
    List.foldl (\( key, value ) acc -> modelInsert key value acc) [] pairs


modelMember : Key -> Model -> Bool
modelMember key model =
    List.any (\( k, _ ) -> k == key) model


applyModelOp : Op -> Model -> Model
applyModelOp op model =
    case op of
        Insert key value ->
            modelInsert key value model

        Remove key ->
            modelRemove key model

        UpdateToJust key value ->
            modelUpdate key (\_ -> Just value) model

        UpdateToNothing key ->
            modelUpdate key (\_ -> Nothing) model

        UpdateValue key value ->
            modelUpdate key (Maybe.map ((+) value)) model

        UpdateIfExists key value ->
            modelUpdate key (Maybe.map ((+) value)) model

        Map n ->
            List.map (\( key, value ) -> ( key, value + n )) model

        Filter n ->
            List.filter (\( key, value ) -> keep n key value) model

        FilterMap n ->
            List.filterMap
                (\( key, value ) ->
                    if keep n key value then
                        Just ( key, value * 2 )

                    else
                        Nothing
                )
                model

        PartitionFirst n ->
            List.filter (\( key, value ) -> keep n key value) model

        PartitionSecond n ->
            List.filter (\( key, value ) -> not (keep n key value)) model

        UnionWith pairs ->
            List.foldl (\( key, value ) acc -> modelInsert key value acc) (modelFromList pairs) model

        UnionInto pairs ->
            List.foldl (\( key, value ) acc -> modelInsert key value acc) model (modelFromList pairs)

        IntersectWith pairs ->
            let
                other =
                    modelFromList pairs
            in
            List.filter (\( key, _ ) -> modelMember key other) model

        DiffWith pairs ->
            let
                other =
                    modelFromList pairs
            in
            List.filter (\( key, _ ) -> not (modelMember key other)) model

        MergeWith pairs ->
            let
                other =
                    modelFromList pairs
            in
            List.map
                (\( key, value ) ->
                    case modelGet key other of
                        Just right ->
                            ( key, value - right )

                        Nothing ->
                            ( key, value )
                )
                model
                ++ List.filter (\( key, _ ) -> not (modelMember key model)) other


expectMatchesModel : Model -> SeqDict Key Value -> Expectation
expectMatchesModel model dict =
    Expect.all
        [ \_ -> SeqDict.toList dict |> Expect.equal model
        , \_ -> SeqDict.size dict |> Expect.equal (List.length model)
        , \_ ->
            List.filterMap
                (\key ->
                    if SeqDict.get key dict == modelGet key model then
                        Nothing

                    else
                        Just ( key, SeqDict.get key dict, modelGet key model )
                )
                allKeys
                |> Expect.equal []
        , \_ ->
            List.filter (\key -> SeqDict.member key dict /= modelMember key model) allKeys
                |> Expect.equal []
        , \_ -> SeqDict.keys dict |> Expect.equal (List.map Tuple.first model)
        , \_ -> SeqDict.values dict |> Expect.equal (List.map Tuple.second model)
        , \_ -> SeqDict.unorderedEquals dict (SeqDict.fromList (List.reverse model)) |> Expect.equal True
        ]
        ()


{-| Checks the dict against the model after every operation, so a broken dict
that a later operation happens to repair still fails.
-}
expectOpsMatchModel : List Op -> Expectation
expectOpsMatchModel ops =
    let
        states : List ( Model, SeqDict Key Value )
        states =
            List.foldl
                (\op ( ( model, dict ), acc ) ->
                    let
                        next =
                            ( applyModelOp op model, applyOp op dict )
                    in
                    ( next, next :: acc )
                )
                ( ( [], SeqDict.empty ), [] )
                ops
                |> Tuple.second
                |> List.reverse
    in
    Expect.all
        (List.map (\( model, dict ) () -> expectMatchesModel model dict) states)
        ()


suite : Test
suite =
    describe "Collisions"
        [ fuzz (Fuzz.listOfLengthBetween 1 60 opFuzzer) "Random operations on colliding keys match an association list" <|
            expectOpsMatchModel
        ]
