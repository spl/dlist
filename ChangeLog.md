
Change Log
==========

Version 0.6 (*expected*)
-------------------------

#### Development changes

* Maintenance and development taken over by Sean Leather
* Migrate repository from http://code.haskell.org/~dons/code/dlist/ to
  https://github.com/spl/dlist

#### Package changes

* Stop supporting `base < 2`
* Fix tests and use `cabal test`
* Add scripts for running `hpc`
* Update documentation

#### New features

* New type class instances: `Eq`, `Ord`, `Read`, `Show`, `Alternative`,
  and `Foldable`
* New function `apply` to use instead of `unDL`

#### Deprecations

* Deprecate DList constructor and record selector to make it abstract
  (see [#4](https://github.com/spl/dlist/issues/4))
* Deprecate `maybeReturn` which is not directly relevant to dlists

