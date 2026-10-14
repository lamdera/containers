module BiSeqDict exposing
    ( BiSeqDict
    , toDict, fromDict, getKeys, uniqueValues, uniqueValuesCount, toReverseList
    , empty, singleton, insert, update, remove
    , isEmpty, member, get, size
    , keys, values, toList, fromList
    , map, foldl, foldr, filter, partition
    , union, intersect, diff, merge
    , encodeBiSeqDict, decodeBiSeqDict
    )

{-| A dictionary that **maintains a mapping from the values back to keys,**
allowing for modelling **many-to-one relationships.**

Example usage:

    manyToOne : BiSeqDict String Int
    manyToOne =
        BiSeqDict.empty
            |> BiSeqDict.insert "A" 1
            |> BiSeqDict.insert "B" 2
            |> BiSeqDict.insert "C" 1
            |> BiSeqDict.insert "D" 4

    BiSeqDict.getKeys 1 manyToOne
    --> SeqSet.fromList ["A", "C"]


# Dictionaries

@docs BiSeqDict


# Differences from Dict

@docs toDict, fromDict, getKeys, uniqueValues, uniqueValuesCount, toReverseList


# Build

@docs empty, singleton, insert, update, remove


# Query

@docs isEmpty, member, get, size


# Lists

@docs keys, values, toList, fromList


# Transform

@docs map, foldl, foldr, filter, partition


# Combine

@docs union, intersect, diff, merge


# Internal

@docs encodeBiSeqDict, decodeBiSeqDict

-}

import Bytes.Decode
import Lamdera.Wire3
import SeqDict exposing (SeqDict)
import SeqSet exposing (SeqSet)


{-| The underlying data structure. Think about it as

    type alias BiSeqDict a b =
        { forward : SeqDict a b -- just a normal Dict!
        , reverse : SeqDict b (SeqSet a) -- the reverse mappings!
        }

-}
type BiSeqDict k v
    = BiSeqDict
        { forward : SeqDict k v
        , reverse : SeqDict v (SeqSet k)
        }


{-| Create an empty dictionary.
-}
empty : BiSeqDict k v
empty =
    BiSeqDict
        { forward = SeqDict.empty
        , reverse = SeqDict.empty
        }


{-| Create a dictionary with one key-value pair.
-}
singleton : k -> v -> BiSeqDict k v
singleton from to =
    BiSeqDict
        { forward = SeqDict.singleton from to
        , reverse = SeqDict.singleton to (SeqSet.singleton from)
        }


{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision.
-}
insert : k -> v -> BiSeqDict k v -> BiSeqDict k v
insert from to (BiSeqDict d) =
    BiSeqDict
        { d
            | forward = SeqDict.insert from to d.forward
            , reverse =
                let
                    oldTo =
                        SeqDict.get from d.forward

                    reverseWithoutOld =
                        case oldTo of
                            Nothing ->
                                d.reverse

                            Just oldTo_ ->
                                d.reverse
                                    |> SeqDict.update oldTo_
                                        (Maybe.map (SeqSet.remove from)
                                            >> Maybe.andThen normalizeSet
                                        )
                in
                reverseWithoutOld
                    |> SeqDict.update to (Maybe.withDefault SeqSet.empty >> SeqSet.insert from >> Just)
        }


{-| Update the value of a dictionary for a specific key with a given function.
-}
update : k -> (Maybe v -> Maybe v) -> BiSeqDict k v -> BiSeqDict k v
update from fn (BiSeqDict d) =
    SeqDict.update from fn d.forward
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


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made.
-}
remove : k -> BiSeqDict k v -> BiSeqDict k v
remove from (BiSeqDict d) =
    BiSeqDict
        { d
            | forward = SeqDict.remove from d.forward
            , reverse = SeqDict.filterMap (\_ set -> SeqSet.remove from set |> normalizeSet) d.reverse
        }


{-| Determine if a dictionary is empty.

    isEmpty empty == True

-}
isEmpty : BiSeqDict k v -> Bool
isEmpty (BiSeqDict d) =
    SeqDict.isEmpty d.forward


{-| Determine if a key is in a dictionary.
-}
member : k -> BiSeqDict k v -> Bool
member from (BiSeqDict d) =
    SeqDict.member from d.forward


{-| Get the value associated with a key. If the key is not found, return
`Nothing`. This is useful when you are not sure if a key will be in the
dictionary.

    animals = fromList [ ("Tom", Cat), ("Jerry", Mouse) ]

    get "Tom"   animals == Just Cat
    get "Jerry" animals == Just Mouse
    get "Spike" animals == Nothing

-}
get : k -> BiSeqDict k v -> Maybe v
get from (BiSeqDict d) =
    SeqDict.get from d.forward


{-| Get all keys associated with a value. If the value is not found,
return an empty set.
-}
getKeys : v -> BiSeqDict k v -> SeqSet k
getKeys to (BiSeqDict d) =
    SeqDict.get to d.reverse
        |> Maybe.withDefault SeqSet.empty


{-| Determine the number of key-value pairs in the dictionary.
-}
size : BiSeqDict k v -> Int
size (BiSeqDict d) =
    SeqDict.size d.forward


{-| Get all of the keys in a dictionary, sorted from lowest to highest.

    keys (fromList [ ( 0, "Alice" ), ( 1, "Bob" ) ]) == [ 0, 1 ]

-}
keys : BiSeqDict k v -> List k
keys (BiSeqDict d) =
    SeqDict.keys d.forward


{-| Get all of the values in a dictionary, in the order of their keys.

    values (fromList [ ( 0, "Alice" ), ( 1, "Bob" ) ]) == [ "Alice", "Bob" ]

-}
values : BiSeqDict k v -> List v
values (BiSeqDict d) =
    SeqDict.values d.forward


{-| Get a list of unique values in the dictionary.
-}
uniqueValues : BiSeqDict k v -> List v
uniqueValues (BiSeqDict d) =
    SeqDict.keys d.reverse


{-| Get a count of unique values in the dictionary.
-}
uniqueValuesCount : BiSeqDict k v -> Int
uniqueValuesCount (BiSeqDict d) =
    SeqDict.size d.reverse


{-| Convert a dictionary into an association list of key-value pairs, sorted by keys.
-}
toList : BiSeqDict k v -> List ( k, v )
toList (BiSeqDict d) =
    SeqDict.toList d.forward


{-| Convert a dictionary into a reverse association list of value-keys pairs.
-}
toReverseList : BiSeqDict k v -> List ( v, SeqSet k )
toReverseList (BiSeqDict d) =
    SeqDict.toList d.reverse


{-| Convert an association list into a dictionary.
-}
fromList : List ( k, v ) -> BiSeqDict k v
fromList list =
    SeqDict.fromList list
        |> fromDict


{-| Apply a function to all values in a dictionary.
-}
map : (k -> v1 -> v2) -> BiSeqDict k v1 -> BiSeqDict k v2
map fn (BiSeqDict d) =
    -- TODO diff instead of throwing away and creating from scratch?
    SeqDict.map fn d.forward
        |> fromDict


{-| Convert BiSeqDict into a SeqDict. (Throw away the reverse mapping.)
-}
toDict : BiSeqDict k v -> SeqDict k v
toDict (BiSeqDict d) =
    d.forward


{-| Convert Dict into a BiSeqDict. (Compute the reverse mapping.)
-}
fromDict : SeqDict k v -> BiSeqDict k v
fromDict forward =
    BiSeqDict
        { forward = forward
        , reverse =
            forward
                |> SeqDict.foldl
                    (\key value acc ->
                        SeqDict.update value
                            (\maybeKeys ->
                                Just <|
                                    case maybeKeys of
                                        Nothing ->
                                            SeqSet.singleton key

                                        Just keys_ ->
                                            SeqSet.insert key keys_
                            )
                            acc
                    )
                    SeqDict.empty
        }


{-| Fold over the key-value pairs in a dictionary in insertion order.
-}
foldl : (k -> v -> acc -> acc) -> acc -> BiSeqDict k v -> acc
foldl fn zero (BiSeqDict d) =
    SeqDict.foldl fn zero d.forward


{-| Fold over the key-value pairs in a dictionary in reverse insertion order.
-}
foldr : (k -> v -> acc -> acc) -> acc -> BiSeqDict k v -> acc
foldr fn zero (BiSeqDict d) =
    SeqDict.foldr fn zero d.forward


{-| Keep only the key-value pairs that pass the given test.
-}
filter : (k -> v -> Bool) -> BiSeqDict k v -> BiSeqDict k v
filter fn (BiSeqDict d) =
    -- TODO diff instead of throwing away and creating from scratch?
    SeqDict.filter fn d.forward
        |> fromDict


{-| Partition a dictionary according to some test. The first dictionary
contains all key-value pairs which passed the test, and the second contains
the pairs that did not.
-}
partition : (k -> v -> Bool) -> BiSeqDict k v -> ( BiSeqDict k v, BiSeqDict k v )
partition fn (BiSeqDict d) =
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
union : BiSeqDict k v -> BiSeqDict k v -> BiSeqDict k v
union (BiSeqDict left) (BiSeqDict right) =
    -- TODO diff instead of throwing away and creating from scratch?
    SeqDict.union left.forward right.forward
        |> fromDict


{-| Keep a key-value pair when its key appears in the second dictionary.
Preference is given to values in the first dictionary.
-}
intersect : BiSeqDict k v -> BiSeqDict k v -> BiSeqDict k v
intersect (BiSeqDict left) (BiSeqDict right) =
    -- TODO diff instead of throwing away and creating from scratch?
    SeqDict.intersect left.forward right.forward
        |> fromDict


{-| Keep a key-value pair when its key does not appear in the second dictionary.
-}
diff : BiSeqDict k v -> BiSeqDict k v -> BiSeqDict k v
diff (BiSeqDict left) (BiSeqDict right) =
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
    (k -> v1 -> acc -> acc)
    -> (k -> v1 -> v2 -> acc -> acc)
    -> (k -> v2 -> acc -> acc)
    -> BiSeqDict k v1
    -> BiSeqDict k v2
    -> acc
    -> acc
merge fnLeft fnBoth fnRight (BiSeqDict left) (BiSeqDict right) zero =
    SeqDict.merge fnLeft fnBoth fnRight left.forward right.forward zero


{-| The Lamdera compiler relies on this function, it is not intended to be used directly. Vendor this function in your own codebase if you want to use it, as the encoding can change without notice.
-}
encodeBiSeqDict : (key -> Lamdera.Wire3.Encoder) -> (value -> Lamdera.Wire3.Encoder) -> BiSeqDict key value -> Lamdera.Wire3.Encoder
encodeBiSeqDict encKey encValue d =
    Lamdera.Wire3.encodeList (Lamdera.Wire3.encodePair encKey encValue) (toList d)


{-| The Lamdera compiler relies on this function, it is not intended to be used directly. Vendor this function in your own codebase if you want to use it, as the encoding can change without notice.
-}
decodeBiSeqDict : Lamdera.Wire3.Decoder k -> Lamdera.Wire3.Decoder value -> Lamdera.Wire3.Decoder (BiSeqDict k value)
decodeBiSeqDict decKey decValue =
    Lamdera.Wire3.decodeList (Lamdera.Wire3.decodePair decKey decValue) |> Bytes.Decode.map fromList
