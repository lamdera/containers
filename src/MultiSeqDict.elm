module MultiSeqDict exposing
    ( MultiSeqDict
    , toDict, fromDict
    , empty, singleton, insert, update, remove, removeAll, removeValues
    , isEmpty, member, getAll, size
    , keys, values, toList, fromList, fromFlatList
    , map, foldl, foldr, filter, partition
    , union, intersect, diff, merge
    , encodeMultiSeqDict, decodeMultiSeqDict
    )

{-| A dictionary mapping unique keys to **multiple** values, allowing for
modelling **one-to-many relationships.**

Example usage:

    oneToMany : MultiSeqDict String Int
    oneToMany =
        MultiSeqDict.empty
            |> MultiSeqDict.insert "A" 1
            |> MultiSeqDict.insert "B" 2
            |> MultiSeqDict.insert "C" 3
            |> MultiSeqDict.insert "A" 2

    MultiSeqDict.getAll "A" oneToMany
    --> SeqSet.fromList [1, 2]


# Dictionaries

@docs MultiSeqDict


# Differences from Dict

@docs toDict, fromDict


# Build

@docs empty, singleton, insert, update, remove, removeAll, removeValues


# Query

@docs isEmpty, member, getAll, size


# Lists

@docs keys, values, toList, fromList, fromFlatList


# Transform

@docs map, foldl, foldr, filter, partition


# Combine

@docs union, intersect, diff, merge


# Internal

@docs encodeMultiSeqDict, decodeMultiSeqDict

-}

import Bytes.Decode
import Internal.ListHelpers exposing (gatherEqualsBy)
import Lamdera.Wire3
import SeqDict exposing (SeqDict)
import SeqSet exposing (SeqSet)


{-| The underlying data structure. Think about it as

     type alias MultiSeqDict k v =
         Dict k (SeqSet v) -- just a normal Dict!

-}
type MultiSeqDict k v
    = MultiSeqDict (SeqDict k (SeqSet v))


{-| Create an empty dictionary.
-}
empty : MultiSeqDict k v
empty =
    MultiSeqDict SeqDict.empty


{-| Create a dictionary with one key-value pair.
-}
singleton : k -> v -> MultiSeqDict k v
singleton from to =
    MultiSeqDict (SeqDict.singleton from (SeqSet.singleton to))


{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision.
-}
insert : k -> v -> MultiSeqDict k v -> MultiSeqDict k v
insert from to (MultiSeqDict d) =
    MultiSeqDict <|
        SeqDict.update
            from
            (\maybeSet ->
                case maybeSet of
                    Nothing ->
                        Just (SeqSet.singleton to)

                    Just set ->
                        Just (SeqSet.insert to set)
            )
            d


{-| Update the value of a dictionary for a specific key with a given function.
-}
update : k -> (SeqSet v -> SeqSet v) -> MultiSeqDict k v -> MultiSeqDict k v
update from fn (MultiSeqDict d) =
    MultiSeqDict <| SeqDict.update from (Maybe.andThen (normalizeSet << fn)) d


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
removeAll : k -> MultiSeqDict k v -> MultiSeqDict k v
removeAll from (MultiSeqDict d) =
    MultiSeqDict (SeqDict.remove from d)


{-| Remove a single key-value pair from a dictionary. If the key is not found,
no changes are made.
-}
remove : k -> v -> MultiSeqDict k v -> MultiSeqDict k v
remove from to (MultiSeqDict d) =
    MultiSeqDict <|
        SeqDict.update from (Maybe.andThen (SeqSet.remove to >> normalizeSet)) d


{-| Remove all occurrences of a value from all keys in the dictionary.
-}
removeValues : v -> MultiSeqDict k v -> MultiSeqDict k v
removeValues value (MultiSeqDict d) =
    MultiSeqDict <|
        SeqDict.filterMap (\_ set -> SeqSet.remove value set |> normalizeSet) d


{-| Determine if a dictionary is empty.

    isEmpty empty == True

-}
isEmpty : MultiSeqDict k v -> Bool
isEmpty (MultiSeqDict d) =
    SeqDict.isEmpty d


{-| Determine if a key is in a dictionary.
-}
member : k -> MultiSeqDict k v -> Bool
member from (MultiSeqDict d) =
    SeqDict.member from d


{-| Get all values associated with a key. If the key is not found, return
an empty set.

    animals = fromList [ ("Tom", "cat"), ("Jerry", "mouse") ]

    getAll "Tom"   animals == SeqSet.singleton "cat"
    getAll "Jerry" animals == SeqSet.singleton "mouse"
    getAll "Spike" animals == SeqSet.empty

-}
getAll : k -> MultiSeqDict k v -> SeqSet v
getAll from (MultiSeqDict d) =
    SeqDict.get from d
        |> Maybe.withDefault SeqSet.empty


{-| Determine the number of key-value pairs in the dictionary.
-}
size : MultiSeqDict k v -> Int
size (MultiSeqDict d) =
    SeqDict.foldl (\_ set acc -> SeqSet.size set + acc) 0 d


{-| Get all of the keys in a dictionary, sorted from lowest to highest.

    keys (fromList [ ( 0, "Alice" ), ( 1, "Bob" ) ]) == [ 0, 1 ]

-}
keys : MultiSeqDict k v -> List k
keys (MultiSeqDict d) =
    SeqDict.keys d


{-| Get all of the values in a dictionary, in the order of their keys.

    values (fromList [ ( 0, "Alice" ), ( 1, "Bob" ) ]) == [ "Alice", "Bob" ]

-}
values : MultiSeqDict k v -> List v
values (MultiSeqDict d) =
    SeqDict.values d
        |> List.concatMap SeqSet.toList


{-| Convert a dictionary into an association list of key-value pairs, sorted by keys.
-}
toList : MultiSeqDict k v -> List ( k, SeqSet v )
toList (MultiSeqDict d) =
    SeqDict.toList d


{-| Convert an association list into a dictionary.
-}
fromList : List ( k, SeqSet v ) -> MultiSeqDict k v
fromList list =
    SeqDict.fromList list
        |> fromDict


{-| Convert an association list into a dictionary.

    fromFlatList
        [ ( "foo", 1 )
        , ( "bar", 2 )
        , ( "foo", 3 )
        ]

results in the same dict as

    fromList
        [ ( "foo", SeqSet.fromList [ 1, 3 ] )
        , ( "bar", SeqSet.fromList [ 2 ] )
        ]

-}
fromFlatList : List ( k, v ) -> MultiSeqDict k v
fromFlatList list =
    list
        |> gatherEqualsBy Tuple.first
        |> List.map
            (\( ( key, _ ) as x, xs ) ->
                ( key
                , SeqSet.fromList <| List.map Tuple.second <| x :: xs
                )
            )
        |> SeqDict.fromList
        |> fromDict


{-| Apply a function to all values in a dictionary.
-}
map : (k -> v1 -> v2) -> MultiSeqDict k v1 -> MultiSeqDict k v2
map fn (MultiSeqDict d) =
    MultiSeqDict <| SeqDict.map (\key set -> SeqSet.map (fn key) set) d


{-| Convert MultiSeqDict into a SeqDict. (Throw away the reverse mapping.)
-}
toDict : MultiSeqDict k v -> SeqDict k (SeqSet v)
toDict (MultiSeqDict d) =
    d


{-| Convert Dict into a MultiSeqDict. (Compute the reverse mapping.)
-}
fromDict : SeqDict k (SeqSet v) -> MultiSeqDict k v
fromDict dict =
    MultiSeqDict dict


{-| Fold over the key-value pairs in a dictionary in insertion order.
-}
foldl : (k -> SeqSet v -> acc -> acc) -> acc -> MultiSeqDict k v -> acc
foldl fn zero (MultiSeqDict d) =
    SeqDict.foldl fn zero d


{-| Fold over the key-value pairs in a dictionary in reverse insertion order.
-}
foldr : (k -> SeqSet v -> acc -> acc) -> acc -> MultiSeqDict k v -> acc
foldr fn zero (MultiSeqDict d) =
    SeqDict.foldr fn zero d


{-| Keep only the mappings that pass the given test.
-}
filter : (k -> v -> Bool) -> MultiSeqDict k v -> MultiSeqDict k v
filter fn (MultiSeqDict d) =
    SeqDict.toList d
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
partition : (k -> SeqSet v -> Bool) -> MultiSeqDict k v -> ( MultiSeqDict k v, MultiSeqDict k v )
partition fn (MultiSeqDict d) =
    let
        ( true, false ) =
            SeqDict.partition fn d
    in
    ( MultiSeqDict true
    , MultiSeqDict false
    )


{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary.
-}
union : MultiSeqDict k v -> MultiSeqDict k v -> MultiSeqDict k v
union (MultiSeqDict left) (MultiSeqDict right) =
    MultiSeqDict <| SeqDict.union left right


{-| Keep a key-value pair when its key appears in the second dictionary.
Preference is given to values in the first dictionary.
-}
intersect : MultiSeqDict k v -> MultiSeqDict k v -> MultiSeqDict k v
intersect (MultiSeqDict left) (MultiSeqDict right) =
    MultiSeqDict <| SeqDict.intersect left right


{-| Keep a key-value pair when its key does not appear in the second dictionary.
-}
diff : MultiSeqDict k v -> MultiSeqDict k v -> MultiSeqDict k v
diff (MultiSeqDict left) (MultiSeqDict right) =
    MultiSeqDict <| SeqDict.diff left right


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
    -> MultiSeqDict k v1
    -> MultiSeqDict k v2
    -> acc
    -> acc
merge fnLeft fnBoth fnRight (MultiSeqDict left) (MultiSeqDict right) zero =
    SeqDict.merge fnLeft fnBoth fnRight left right zero


{-| The Lamdera compiler relies on this function, it is not intended to be used directly. Vendor this function in your own codebase if you want to use it, as the encoding can change without notice.
-}
encodeMultiSeqDict : (key -> Lamdera.Wire3.Encoder) -> (value -> Lamdera.Wire3.Encoder) -> MultiSeqDict key value -> Lamdera.Wire3.Encoder
encodeMultiSeqDict encKey encValue d =
    Lamdera.Wire3.encodeList (Lamdera.Wire3.encodePair encKey (SeqSet.encodeSet encValue)) (toList d)


{-| The Lamdera compiler relies on this function, it is not intended to be used directly. Vendor this function in your own codebase if you want to use it, as the encoding can change without notice.
-}
decodeMultiSeqDict : Lamdera.Wire3.Decoder k -> Lamdera.Wire3.Decoder value -> Lamdera.Wire3.Decoder (MultiSeqDict k value)
decodeMultiSeqDict decKey decValue =
    Lamdera.Wire3.decodeList (Lamdera.Wire3.decodePair decKey (SeqSet.decodeSet decValue)) |> Bytes.Decode.map fromList
