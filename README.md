## Containers

This package is a collection of various container-like data structures including `SeqDict`, `SeqSet`, and bidirectional/multi-value dictionaries.

Install using `lamdera install lamdera/containers`.

## SeqDict and SeqSet (sequential)

These behave like `Dict` and `Set` with some important differences:

* They do not require `comparable` keys, any equatable* Elm value can be used as a key
* `toList` returns a list of key-value pairs in insertion order rather than being sorted by comparable keys
* `fromList [ ("A", 1), ("B", 2) ] /= fromList [ ("B", 2), ("A", 1) ]`, use `unorderedEquals` if you want to check if two SeqDicts or SeqSets are equal regardless of insertion order

This is similar to [`pzp1997/assoc-list`](https://package.elm-lang.org/packages/pzp1997/assoc-list/latest/), however unlike assoc-list,
SeqDict and SeqSet are backed by a hashmap meaning they have better asymptotic performance.
For example insertions are `O(log(n))` rather than `O(n)` and fromList is `O(n * log(n))` rather than `O(n^2)`.

<sup>*Non-equatable Elm values are currently: functions, `Bytes`, `Html`, `Json.Value`, `Task`, `Cmd`, `Sub`, `Never`, `Texture`, `Shader`, and any datastructures containing these types.</sup>


## BiSeqDict, MultiSeqDict, and MultiBiSeqDict (bidirectional and multi-value dictionaries)

These data structures extend the capabilities of `SeqDict` to handle more complex relationships:

### BiSeqDict (Many-to-One)

`BiSeqDict` is a bidirectional dictionary that maintains a reverse mapping from values back to keys. This is useful when:
- Multiple keys can map to the same value
- You need efficient lookups in both directions
- You want to find all keys associated with a particular value

**Example with opaque types:**
```elm
import BiSeqDict exposing (BiSeqDict)

-- Opaque ID types (not comparable!)
type UserId = UserId Never
type WorkspaceId = WorkspaceId Never

-- Multiple users can belong to the same workspace
userWorkspaces : BiSeqDict (Id UserId) (Id WorkspaceId)
userWorkspaces =
    BiSeqDict.empty
        |> BiSeqDict.insert aliceId workspace1
        |> BiSeqDict.insert bobId workspace1
        |> BiSeqDict.insert charlieId workspace2

-- Forward lookup: What workspace does alice belong to?
BiSeqDict.get aliceId userWorkspaces
--> Just workspace1

-- Reverse lookup: Who are all members of workspace1?
BiSeqDict.getKeys workspace1 userWorkspaces
--> SeqSet.fromList [aliceId, bobId]
```

**Note:** This works with opaque ID types that aren't `comparable` - you couldn't do this with regular `Dict`!

**Performance:** O(log n) for both forward and reverse lookups.

### MultiSeqDict (One-to-Many)

`MultiSeqDict` allows one key to map to multiple values. This is useful when:
- A single key naturally has multiple associated values
- You want to maintain a collection of values per key
- You need set semantics (no duplicate values per key)

**Example with opaque types:**
```elm
import MultiSeqDict exposing (MultiSeqDict)

type PropertyId = PropertyId Never
type UnitId = UnitId Never

-- A property can have multiple units
propertyUnits : MultiSeqDict (Id PropertyId) (Id UnitId)
propertyUnits =
    MultiSeqDict.empty
        |> MultiSeqDict.insert property1 unit101
        |> MultiSeqDict.insert property1 unit102
        |> MultiSeqDict.insert property2 unit201

-- Get all units for property1
MultiSeqDict.getAll property1 propertyUnits
--> SeqSet.fromList [unit101, unit102]

-- Remove a specific unit
MultiSeqDict.remove property1 unit102 propertyUnits
```

**Performance:** O(log n) for lookups and insertions.

### MultiBiSeqDict (Many-to-Many)

`MultiBiSeqDict` combines both features: multiple values per key AND efficient reverse lookups. This is useful when:
- You have a many-to-many relationship
- You need lookups in both directions
- Each key can have multiple values and each value can be associated with multiple keys

**Real-world example: Documents can belong to multiple chats**
```elm
import MultiBiSeqDict exposing (MultiBiSeqDict)

type ChatId = ChatId Never
type DocumentId = DocumentId Never

-- Documents can be shared across multiple chats
-- Chats can have multiple documents
-- Documents can be transferred between chats
chatDocuments : MultiBiSeqDict (Id ChatId) (Id DocumentId)
chatDocuments =
    MultiBiSeqDict.empty
        |> MultiBiSeqDict.insert chat1 doc1
        |> MultiBiSeqDict.insert chat1 doc2
        |> MultiBiSeqDict.insert chat2 doc1  -- doc1 is shared!

-- What documents are in chat1?
MultiBiSeqDict.getAll chat1 chatDocuments
--> SeqSet.fromList [doc1, doc2]

-- Which chats contain doc1?
MultiBiSeqDict.getKeys doc1 chatDocuments
--> SeqSet.fromList [chat1, chat2]

-- Transfer doc2 from chat1 to chat3
chatDocuments
    |> MultiBiSeqDict.remove chat1 doc2
    |> MultiBiSeqDict.insert chat3 doc2
```

**Why this is better than regular Dict:**
- ✅ Works with opaque ID types (not `comparable`)
- ✅ O(log n) queries in both directions
- ✅ Automatic consistency when transferring documents
- ❌ Regular `Dict` would require manual index maintenance and comparable keys

**Performance:** O(log n) for lookups in both directions.

### Key Features

All three types:
- ✅ Work with any equatable types (no `comparable` constraint)
- ✅ Preserve insertion order
- ✅ Provide O(log n) performance for core operations
- ✅ Automatically maintain consistency (removing a key updates all related mappings)

**Wire3 Support:** Full Lamdera Wire3 codec support is included for all three types (`encodeBiSeqDict`, `encodeMultiSeqDict`, `encodeMultiBiSeqDict`), allowing them to be used directly in your Lamdera `BackendModel`, `FrontendModel`, and messages.


## Comparison to other Elm packages

See miniBill's [comparison of Elm Dict implementations](https://docs.google.com/spreadsheets/d/1j2rHUx5Nf5auvg5ikzYxbW4e1M9g0-hgU8nMogLD4EY) for a meta-analysis of implementation and performance characteristics.


## Attribution

The core implementation for SeqDict and SeqSet was written by [Robin Hansen](https://github.com/robinheghan/).
Thanks to [Ambue](https://ambue.com/) for sponsoring the work needed to get this integrated into Lamdera!

## Additional reading

If you'd like to read about the trade-offs that were made when designing this package, check out this [blog post](https://martinstewart.dev/stuff/lamdera-containers/)!
