This directory contains a benchmark package for the `dlist` package. It is a
separate package instead of a benchmark suite in the [`dlist.cabal`][] to avoid
a cyclical dependency by `aeson` on `dlist`.

To run the benchmarks:

```sh
cabal run
```

[`dlist.cabal`]: ../dlist.cabal
