
Change Log
==========

Version 0.8.0.2 (2016-09-04) *World Sexual Health Day*
------------------------------------------------------

#### Package changes

* Fix test suite: add missing module `OverloadedStrings`
  ([Sergei Trofimovich](https://github.com/trofi))

Version 0.8.0.1 (2016-07-29) *58th Anniversary of the Creation of NASA*
-----------------------------------------------------------------------

#### Package changes

* Change QuickCheck lower bound to 2.9 for GHC >= 8 (base >= 4.9)
  ([Adam Bergmark](https://github.com/bergmark))

Version 0.8 (2016-07-17) *Constitution Day in South Korea*
----------------------------------------------------------

#### New features

* Add pattern synonyms `Nil` and `Cons` for GHC >= 7.8
* Add `Semigroup` instance for GHC >= 8 (base >= 4.9)
* Use inflexible instance for `IsString` to improve support for overloaded
  strings ([Baldur Blöndal](https://github.com/Icelandjack))

#### Package changes

* Change QuickCheck upper bound from 2.9 to 2.10

#### Development changes

* Add `-Wall -Werror` testing
* Add testing for GHC 8.0.1 to Travis-CI

Version 0.7.1.2 (2015-08-23) *International Day for the Remembrance of the Slave Trade and its Abolition*
---------------------------------------------------------------------------------------------------------

#### Package changes

* Fix GHC 7.10 warnings due to imports
  ([Mikhail Glushenkov](https://github.com/23Skidoo))

Version 0.7.1.1 (2015-03-19) *St Joseph's Day*
----------------------------------------------

#### Package changes

* Change QuickCheck upper bound from 2.8 to 2.9

Version 0.7.1 (2014-06-28) *100th Anniversary of the Assassination of Franz Ferdinand*
--------------------------------------------------------------------------------------

#### New features

* Add `IsList` instance for GHC >= 7.8
  ([Baldur Blöndal](https://github.com/Icelandjack))

Version 0.7.0.1 (2014-03-24) *World Tuberculosis Day*
-----------------------------------------------------

#### Package changes

* Change QuickCheck upper bound from 2.7 to 2.8

Version 0.7 (2014-03-17) *St. Patrick's Day*
--------------------------------------------

#### New features

* Add `NFData` instance (and `deepseq` dependency)
* Add `IsString` instance
* Remove deprecated entities

Version 0.6.0.1 (2013-12-01) *World AIDS Day*
---------------------------------------------

#### Package changes

* Change QuickCheck lower bound from 2.6 to 2.5
  ([Michael Snoyman](https://github.com/snoyberg))

Version 0.6 (2013-11-29) *Black Friday*
---------------------------------------

#### Development changes

* Maintenance and development taken over by Sean Leather
  ([Bas van Dijk](https://github.com/basvandijk))
* Migrate repository from http://code.haskell.org/~dons/code/dlist/ to
  https://github.com/spl/dlist
* Add Travis-CI ([Herbert Valerio Riedel](https://github.com/hvr))

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
