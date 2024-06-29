## Containers

This is a collection of various container-like data structures. Currently only SeqDict and SeqSet are included.

## SeqDict and SeqSet (aka sequential dictionary and sequential set)

These behave like `Dict` and `Set` with some important differences:
* They do not require `comparable` keys
* `toList` returns a list of key-value pairs in insertion order rather than being sorted by comparable keys
* `fromList [ ("A", 1), ("B", 2) ] /= fromList [ ("B", 2), ("A", 1) ]`, use `unorderedEquals` if you want to check if two SeqDicts or SeqSets are equal regardless of insertion order

This is similar to [`pzp1997/assoc-list`](https://package.elm-lang.org/packages/pzp1997/assoc-list/latest/), however unlike assoc-list,
SeqDict and SeqSet are backed by a hashmap meaning they have better asymptotic performance.
For example insertions are `O(log(n))` rather than `O(n)` and fromList is `O(n * log(n))` rather than `O(n^2)`.

## Attribution

The core implementation for SeqDict and SeqSet was written by [Robin Hansen](https://github.com/robinheghan/).
Thanks to [Ambue](https://ambue.com/) for sponsoring the work needed to get this integrated into Lamdera!
