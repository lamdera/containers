module OrderedSet exposing
    ( empty, singleton, insert, remove
    , isEmpty, member, size, unorderedEquals
    , union, intersect, diff
    , toList, fromList
    , map, foldl, foldr, filter, partition
    , OrderedSet
    )

{-| A set of unique values.


# Sets

@docs Set


# Build

@docs empty, singleton, insert, remove


# Query

@docs isEmpty, member, size, unorderedEquals


# Combine

@docs union, intersect, diff


# Lists

@docs toList, fromList


# Transform

@docs map, foldl, foldr, filter, partition

-}

import Bytes.Decode exposing (Decoder)
import Bytes.Encode exposing (Encoder)
import OrderedDict as Dict exposing (OrderedDict)
import Lamdera.Wire3


{-| Represents a set of unique values. So `(Set Int)` is a set of integers and
`(Set String)` is a set of strings.
-}
type OrderedSet a
    = OrderedSet_elm_builtin (OrderedDict a Bool)


{-| Create an empty set.
-}
empty : OrderedSet a
empty =
    OrderedSet_elm_builtin Dict.empty


{-| Create a set with one value.
-}
singleton : a -> OrderedSet a
singleton k =
    OrderedSet_elm_builtin (Dict.singleton k True)


{-| Insert a value into a set.
-}
insert : a -> OrderedSet a -> OrderedSet a
insert k (OrderedSet_elm_builtin dict) =
    OrderedSet_elm_builtin (Dict.insert k True dict)


{-| Remove a value from a set. If the value is not found, no changes are made.
-}
remove : a -> OrderedSet a -> OrderedSet a
remove k (OrderedSet_elm_builtin dict) =
    OrderedSet_elm_builtin (Dict.remove k dict)


{-| Determine if a set is empty.
-}
isEmpty : OrderedSet a -> Bool
isEmpty (OrderedSet_elm_builtin dict) =
    Dict.isEmpty dict


{-| Determine if a value is in a set.
-}
member : a -> OrderedSet a -> Bool
member k (OrderedSet_elm_builtin dict) =
    Dict.member k dict


{-| Determine the number of elements in a set.
-}
size : OrderedSet a -> Int
size (OrderedSet_elm_builtin dict) =
    Dict.size dict


{-| Get the union of two sets. Keep all values.
-}
union : OrderedSet a -> OrderedSet a -> OrderedSet a
union (OrderedSet_elm_builtin d1) (OrderedSet_elm_builtin d2) =
    OrderedSet_elm_builtin (Dict.union d1 d2)


{-| Get the intersection of two sets. Keeps values that appear in both sets.
-}
intersect : OrderedSet a -> OrderedSet a -> OrderedSet a
intersect (OrderedSet_elm_builtin d1) (OrderedSet_elm_builtin d2) =
    OrderedSet_elm_builtin (Dict.intersect d1 d2)


{-| Get the difference between the first set and the second. Keeps values
that do not appear in the second set.
-}
diff : OrderedSet a -> OrderedSet a -> OrderedSet a
diff (OrderedSet_elm_builtin d1) (OrderedSet_elm_builtin d2) =
    OrderedSet_elm_builtin (Dict.diff d1 d2)


{-| Convert a set into a list.
-}
toList : OrderedSet a -> List a
toList (OrderedSet_elm_builtin dict) =
    Dict.keys dict


{-| Convert a list into a set, removing any duplicates.
-}
fromList : List a -> OrderedSet a
fromList xs =
    List.foldl insert empty xs


{-| Fold over the values in a set.
-}
foldl : (a -> b -> b) -> b -> OrderedSet a -> b
foldl f b (OrderedSet_elm_builtin dict) =
    Dict.foldl (\k _ b2 -> f k b2) b dict


{-| Fold over the values in a set.
-}
foldr : (a -> b -> b) -> b -> OrderedSet a -> b
foldr f b (OrderedSet_elm_builtin dict) =
    Dict.foldr (\k _ b2 -> f k b2) b dict


{-| Map a function onto a set, creating a new set with no duplicates.
-}
map : (a -> b) -> OrderedSet a -> OrderedSet b
map fn (OrderedSet_elm_builtin dict) =
    OrderedSet_elm_builtin (Dict.foldl (\k _ b2 -> Dict.insert (fn k) True b2) Dict.empty dict)


{-| Create a new set consisting only of elements which satisfy a predicate.
-}
filter : (a -> Bool) -> OrderedSet a -> OrderedSet a
filter p (OrderedSet_elm_builtin dict) =
    OrderedSet_elm_builtin (Dict.filter (\k _ -> p k) dict)


{-| Create two new sets; the first consisting of elements which satisfy a
predicate, the second consisting of elements which do not.
-}
partition : (a -> Bool) -> OrderedSet a -> ( OrderedSet a, OrderedSet a )
partition p (OrderedSet_elm_builtin dict) =
    let
        ( trues, falses ) =
            Dict.partition (\k _ -> p k) dict
    in
    ( OrderedSet_elm_builtin trues, OrderedSet_elm_builtin falses )


{-| The Lamdera compiler relies on this function existing even though it isn't exposed. Don't delete it!
-}
encodeSet : (value -> Encoder) -> OrderedSet value -> Encoder
encodeSet encVal s =
    Lamdera.Wire3.encodeList encVal (toList s)


{-| The Lamdera compiler relies on this function existing even though it isn't exposed. Don't delete it!
-}
decodeSet : Decoder k -> Decoder (OrderedSet k)
decodeSet decVal =
    Lamdera.Wire3.decodeList decVal |> Bytes.Decode.map fromList


orderedEquals : OrderedSet k -> OrderedSet k -> Bool
orderedEquals set1 set2 =
    if size set1 == size set2 then
        toList set1 == toList set2

    else
        False


{-| Check if two sets are equal regardless what order items were inserted.
Note that this is not the same as writing `set1 == set2`. Insertion order matters when using `==`.

    set1 = OrderedSet.fromList [ ( "A", 1 ), ( "B", 2 ) ]
    set2 = OrderedSet.fromList [ ( "B", 2 ), ( "A", 1 ) ]

    OrderedSet.unorderedEquals set1 set2 -- True
    set1 == set2 -- False

-}
unorderedEquals : OrderedSet k -> OrderedSet k -> Bool
unorderedEquals (OrderedSet_elm_builtin dict1) (OrderedSet_elm_builtin dict2) =
    Dict.unorderedEquals dict1 dict2
