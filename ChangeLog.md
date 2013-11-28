
Change Log
==========

Version 0.6 (*expected*)
-------------------------

* Maintenance and development taken over by Sean Leather
* Migrate repository from http://code.haskell.org/~dons/code/dlist/ to
  https://github.com/spl/dlist
* Add instances: `Eq`, `Ord`, `Read`, `Show`, `Alternative`, and `Foldable`
* Make DList abstract (see [#4](https://github.com/spl/dlist/issues/4))
* Add `apply` to use instead of `unDL`
* Remove `maybeReturn` which is not directly relevant to dlists
* Stop supporting `base < 2`
* Update tests to run `cabal test` using parallel QuickCheck (`pqc`)
* Add scripts for running `hpc`
* Update documentation

