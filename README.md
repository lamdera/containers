## Containers

This package is a collection of various container-like data structures. Currently only `SeqDict` and `SeqSet` are included.

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


## Comparison to other Elm packages

See miniBill's [comparison of Elm Dict implementations](https://docs.google.com/spreadsheets/d/1j2rHUx5Nf5auvg5ikzYxbW4e1M9g0-hgU8nMogLD4EY) for a meta-analysis of implementation and performance characteristics.


## Attribution

The core implementation for SeqDict and SeqSet was written by [Robin Hansen](https://github.com/robinheghan/).
Thanks to [Ambue](https://ambue.com/) for sponsoring the work needed to get this integrated into Lamdera!

## Additional reading

If you'd like to read about the trade-offs that were made when designing this package, check out this [blog post](https://martinstewart.dev/stuff/lamdera-containers/)!
