module MultiBiSeqDict exposing
    ( MultiBiSeqDict
    , toDict, fromDict, getKeys, uniqueValues, uniqueValuesCount, toReverseList
    , empty, singleton, insert, update, remove, removeAll, removeValues
    , isEmpty, member, getAll, size
    , keys, values, toList, fromList
    , map, foldl, foldr, filter, partition
    , union, intersect, diff, merge
    , encodeMultiBiSeqDict, decodeMultiBiSeqDict
    )

{-| A dictionary mapping unique keys to **multiple** values, which
**maintains a mapping from the values back to keys,** allowing for
modelling **many-to-many relationships.**

Example usage:

    manyToMany : MultiBiSeqDict String Int
    manyToMany =
        MultiBiSeqDict.empty
            |> MultiBiSeqDict.insert "A" 1
            |> MultiBiSeqDict.insert "B" 2
            |> MultiBiSeqDict.insert "C" 3
            |> MultiBiSeqDict.insert "A" 2

    MultiBiSeqDict.getAll "A" manyToMany
    --> SeqSet.fromList [1, 2]

    MultiBiSeqDict.getKeys 2 manyToMany
    --> SeqSet.fromList ["A", "B"]


# Dictionaries

@docs MultiBiSeqDict


# Differences from Dict

@docs toDict, fromDict, getKeys, uniqueValues, uniqueValuesCount, toReverseList


# Build

@docs empty, singleton, insert, update, remove, removeAll, removeValues


# Query

@docs isEmpty, member, getAll, size


# Lists

@docs keys, values, toList, fromList


# Transform

@docs map, foldl, foldr, filter, partition


# Combine

@docs union, intersect, diff, merge


# Internal

@docs encodeMultiBiSeqDict, decodeMultiBiSeqDict

-}

import Bytes.Decode
import Lamdera.Wire3
import SeqDict exposing (SeqDict)
import SeqSet exposing (SeqSet)


{-| The underlying data structure. Think about it as

    type alias MultiBiSeqDict k v =
        { forward : SeqDict k (SeqSet v) -- just a normal Dict!
        , reverse : SeqDict v (SeqSet k) -- the reverse mappings!
        }

-}
type MultiBiSeqDict k v
    = MultiBiSeqDict
        { forward : SeqDict k (SeqSet v)
        , reverse : SeqDict v (SeqSet k)
        }


{-| Create an empty dictionary.
-}
empty : MultiBiSeqDict k v
empty =
    MultiBiSeqDict
        { forward = SeqDict.empty
        , reverse = SeqDict.empty
        }


{-| Create a dictionary with one key-value pair.
-}
singleton : k -> v -> MultiBiSeqDict k v
singleton from to =
    MultiBiSeqDict
        { forward = SeqDict.singleton from (SeqSet.singleton to)
        , reverse = SeqDict.singleton to (SeqSet.singleton from)
        }


{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision.
-}
insert : k -> v -> MultiBiSeqDict k v -> MultiBiSeqDict k v
insert from to (MultiBiSeqDict d) =
    SeqDict.update
        from
        (\maybeSet ->
            case maybeSet of
                Nothing ->
                    Just (SeqSet.singleton to)

                Just set ->
                    Just (SeqSet.insert to set)
        )
        d.forward
        |> fromDict


{-| Update the value of a dictionary for a specific key with a given function.
-}
update : k -> (SeqSet v -> SeqSet v) -> MultiBiSeqDict k v -> MultiBiSeqDict k v
update from fn (MultiBiSeqDict d) =
    SeqDict.update from (Maybe.andThen (normalizeSet << fn)) d.forward
        |> fromDict


{-| In our model, (Just SeqSet.empty) has the same meaning as Nothing.
Make it be Nothing!
-}
normalizeSet : SeqSet k -> Maybe (SeqSet k)
normalizeSet set =
    if SeqSet.isEmpty set then
        Nothing

    else
        Just set


{-| Remove all key-value pairs for the given key from a dictionary. If the key is
not found, no changes are made.
-}
removeAll : k -> MultiBiSeqDict k v -> MultiBiSeqDict k v
removeAll from (MultiBiSeqDict d) =
    MultiBiSeqDict
        { d
            | forward = SeqDict.remove from d.forward
            , reverse = SeqDict.filterMap (\_ set -> SeqSet.remove from set |> normalizeSet) d.reverse
        }


{-| Remove a single key-value pair from a dictionary. If the key is not found,
no changes are made.
-}
remove : k -> v -> MultiBiSeqDict k v -> MultiBiSeqDict k v
remove from to (MultiBiSeqDict d) =
    SeqDict.update from (Maybe.andThen (SeqSet.remove to >> normalizeSet)) d.forward
        |> fromDict


{-| Remove all occurrences of a value from all keys in the dictionary.
-}
removeValues : v -> MultiBiSeqDict k v -> MultiBiSeqDict k v
removeValues value (MultiBiSeqDict d) =
    SeqDict.filterMap (\_ set -> SeqSet.remove value set |> normalizeSet) d.forward
        |> fromDict


{-| Determine if a dictionary is empty.

    isEmpty empty == True

-}
isEmpty : MultiBiSeqDict k v -> Bool
isEmpty (MultiBiSeqDict d) =
    SeqDict.isEmpty d.forward


{-| Determine if a key is in a dictionary.
-}
member : k -> MultiBiSeqDict k v -> Bool
member from (MultiBiSeqDict d) =
    SeqDict.member from d.forward


{-| Get all values associated with a key. If the key is not found, return
an empty set.

    animals = fromList [ ("Tom", Cat), ("Jerry", Mouse) ]

    getAll "Tom"   animals == SeqSet.singleton Cat
    getAll "Jerry" animals == SeqSet.singleton Mouse
    getAll "Spike" animals == SeqSet.empty

-}
getAll : k -> MultiBiSeqDict k v -> SeqSet v
getAll from (MultiBiSeqDict d) =
    SeqDict.get from d.forward
        |> Maybe.withDefault SeqSet.empty


{-| Get all keys associated with a value. If the value is not found,
return an empty set.
-}
getKeys : v -> MultiBiSeqDict k v -> SeqSet k
getKeys to (MultiBiSeqDict d) =
    SeqDict.get to d.reverse
        |> Maybe.withDefault SeqSet.empty


{-| Determine the number of key-value pairs in the dictionary.
-}
size : MultiBiSeqDict k v -> Int
size (MultiBiSeqDict d) =
    SeqDict.foldl (\_ set acc -> SeqSet.size set + acc) 0 d.forward


{-| Get all of the keys in a dictionary, sorted from lowest to highest.

    keys (fromList [ ( 0, "Alice" ), ( 1, "Bob" ) ]) == [ 0, 1 ]

-}
keys : MultiBiSeqDict k v -> List k
keys (MultiBiSeqDict d) =
    SeqDict.keys d.forward


{-| Get all of the values in a dictionary, in the order of their keys.

    values (fromList [ ( 0, "Alice" ), ( 1, "Bob" ) ]) == [ "Alice", "Bob" ]

-}
values : MultiBiSeqDict k v -> List v
values (MultiBiSeqDict d) =
    SeqDict.values d.forward
        |> List.concatMap SeqSet.toList


{-| Get a list of unique values in the dictionary.
-}
uniqueValues : MultiBiSeqDict k v -> List v
uniqueValues (MultiBiSeqDict d) =
    SeqDict.keys d.reverse


{-| Get a count of unique values in the dictionary.
-}
uniqueValuesCount : MultiBiSeqDict k v -> Int
uniqueValuesCount (MultiBiSeqDict d) =
    SeqDict.size d.reverse


{-| Convert a dictionary into an association list of key-value pairs, sorted by keys.
-}
toList : MultiBiSeqDict k v -> List ( k, SeqSet v )
toList (MultiBiSeqDict d) =
    SeqDict.toList d.forward


{-| Convert a dictionary into a reverse association list of value-keys pairs.
-}
toReverseList : MultiBiSeqDict k v -> List ( v, SeqSet k )
toReverseList (MultiBiSeqDict d) =
    SeqDict.toList d.reverse


{-| Convert an association list into a dictionary.
-}
fromList : List ( k, SeqSet v ) -> MultiBiSeqDict k v
fromList list =
    SeqDict.fromList list
        |> fromDict


{-| Apply a function to all values in a dictionary.
-}
map : (k -> v1 -> v2) -> MultiBiSeqDict k v1 -> MultiBiSeqDict k v2
map fn (MultiBiSeqDict d) =
    -- TODO diff instead of throwing away and creating from scratch?
    SeqDict.map (\key set -> SeqSet.map (fn key) set) d.forward
        |> fromDict


{-| Convert MultiBiSeqDict into a SeqDict. (Throw away the reverse mapping.)
-}
toDict : MultiBiSeqDict k v -> SeqDict k (SeqSet v)
toDict (MultiBiSeqDict d) =
    d.forward


{-| Convert Dict into a MultiBiSeqDict. (Compute the reverse mapping.)
-}
fromDict : SeqDict k (SeqSet v) -> MultiBiSeqDict k v
fromDict forward =
    MultiBiSeqDict
        { forward = forward
        , reverse =
            SeqDict.foldl
                (\key set acc ->
                    SeqSet.foldl
                        (\value acc_ ->
                            SeqDict.update
                                value
                                (\maybeSet ->
                                    case maybeSet of
                                        Nothing ->
                                            Just (SeqSet.singleton key)

                                        Just set_ ->
                                            Just (SeqSet.insert key set_)
                                )
                                acc_
                        )
                        acc
                        set
                )
                SeqDict.empty
                forward
        }


{-| Fold over the key-value pairs in a dictionary in insertion order.
-}
foldl : (k -> SeqSet v -> acc -> acc) -> acc -> MultiBiSeqDict k v -> acc
foldl fn zero (MultiBiSeqDict d) =
    SeqDict.foldl fn zero d.forward


{-| Fold over the key-value pairs in a dictionary in reverse insertion order.
-}
foldr : (k -> SeqSet v -> acc -> acc) -> acc -> MultiBiSeqDict k v -> acc
foldr fn zero (MultiBiSeqDict d) =
    SeqDict.foldr fn zero d.forward


{-| Keep only the mappings that pass the given test.
-}
filter : (k -> v -> Bool) -> MultiBiSeqDict k v -> MultiBiSeqDict k v
filter fn (MultiBiSeqDict d) =
    SeqDict.toList d.forward
        |> List.filterMap
            (\( key, values_ ) ->
                values_
                    |> SeqSet.filter (fn key)
                    |> normalizeSet
                    |> Maybe.map (Tuple.pair key)
            )
        |> fromList


{-| Partition a dictionary according to some test. The first dictionary
contains all key-value pairs which passed the test, and the second contains
the pairs that did not.
-}
partition : (k -> SeqSet v -> Bool) -> MultiBiSeqDict k v -> ( MultiBiSeqDict k v, MultiBiSeqDict k v )
partition fn (MultiBiSeqDict d) =
    -- TODO diff instead of throwing away and creating from scratch?
    let
        ( forwardTrue, forwardFalse ) =
            SeqDict.partition fn d.forward
    in
    ( fromDict forwardTrue
    , fromDict forwardFalse
    )


{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary.
-}
union : MultiBiSeqDict k v -> MultiBiSeqDict k v -> MultiBiSeqDict k v
union (MultiBiSeqDict left) (MultiBiSeqDict right) =
    -- TODO diff instead of throwing away and creating from scratch?
    SeqDict.union left.forward right.forward
        |> fromDict


{-| Keep a key-value pair when its key appears in the second dictionary.
Preference is given to values in the first dictionary.
-}
intersect : MultiBiSeqDict k v -> MultiBiSeqDict k v -> MultiBiSeqDict k v
intersect (MultiBiSeqDict left) (MultiBiSeqDict right) =
    -- TODO diff instead of throwing away and creating from scratch?
    SeqDict.intersect left.forward right.forward
        |> fromDict


{-| Keep a key-value pair when its key does not appear in the second dictionary.
-}
diff : MultiBiSeqDict k v -> MultiBiSeqDict k v -> MultiBiSeqDict k v
diff (MultiBiSeqDict left) (MultiBiSeqDict right) =
    -- TODO diff instead of throwing away and creating from scratch?
    SeqDict.diff left.forward right.forward
        |> fromDict


{-| The most general way of combining two dictionaries. You provide three
accumulators for when a given key appears:

1.  Only in the left dictionary.
2.  In both dictionaries.
3.  Only in the right dictionary.

You then traverse all the keys from lowest to highest, building up whatever
you want.

-}
merge :
    (k -> SeqSet v1 -> acc -> acc)
    -> (k -> SeqSet v1 -> SeqSet v2 -> acc -> acc)
    -> (k -> SeqSet v2 -> acc -> acc)
    -> MultiBiSeqDict k v1
    -> MultiBiSeqDict k v2
    -> acc
    -> acc
merge fnLeft fnBoth fnRight (MultiBiSeqDict left) (MultiBiSeqDict right) zero =
    SeqDict.merge fnLeft fnBoth fnRight left.forward right.forward zero


{-| The Lamdera compiler relies on this function, it is not intended to be used directly. Vendor this function in your own codebase if you want to use it, as the encoding can change without notice.
-}
encodeMultiBiSeqDict : (key -> Lamdera.Wire3.Encoder) -> (value -> Lamdera.Wire3.Encoder) -> MultiBiSeqDict key value -> Lamdera.Wire3.Encoder
encodeMultiBiSeqDict encKey encValue d =
    Lamdera.Wire3.encodeList (Lamdera.Wire3.encodePair encKey (SeqSet.encodeSet encValue)) (toList d)


{-| The Lamdera compiler relies on this function, it is not intended to be used directly. Vendor this function in your own codebase if you want to use it, as the encoding can change without notice.
-}
decodeMultiBiSeqDict : Lamdera.Wire3.Decoder k -> Lamdera.Wire3.Decoder value -> Lamdera.Wire3.Decoder (MultiBiSeqDict k value)
decodeMultiBiSeqDict decKey decValue =
    Lamdera.Wire3.decodeList (Lamdera.Wire3.decodePair decKey (SeqSet.decodeSet decValue)) |> Bytes.Decode.map fromList
