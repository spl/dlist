
Change Log
==========

Version NEXT (????-??-??)
-------------------------

* Maintenance and development taken over by Sean Leather
* Migrate repository from http://code.haskell.org/~dons/code/dlist/ to
  https://github.com/spl/dlist
* Add `Eq`, `Ord`, `Read`, `Show`, `Alternative`, `Foldable`, `Traversable`
  instances
* Deprecate functions in favor of their type class equivalents: `concat`, `map`,
  `foldr`
* Deprecate `DL`, `unDL` and add `apply` ([#4](https://github.com/spl/dlist/issues/4))
* Update tests to run `cabal test` using parallel QuickCheck (`pqc`)
* Add scripts for running `hpc`
* Update documentation

