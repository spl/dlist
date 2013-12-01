
Change Log
==========

Version 0.6.0.1 (2013-12-01) *World AIDS Day*
---------------------------------------------

#### Package changes

* Change QuickCheck lower bound from 2.6 to 2.5

#### Contributors

* (Michael Snoyman)[https://github.com/snoyberg]

Version 0.6 (2013-11-29) *Black Friday*
---------------------------------------

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

#### Contributors

* (Bas van Dijk)[https://github.com/basvandijk]
* (Herbert Valerio Riedel)[https://github.com/hvr]

