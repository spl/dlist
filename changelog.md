# Change Log

## v0.8.0.8

_Released on 2020-04-02 (World Autism Awareness Day)._

#### Package changes

* Change QuickCheck upper bound from 2.14 to 2.15

## v0.8.0.7

_Released on 2019-08-05 (Independence Day in Burkina Faso)._

#### Package changes

* Add `MonadFail` instance for GHC >= 8.8
  ([Vanessa McHale](https://github.com/vmchale))
* Change `deepseq` upper bound to 1.5
  ([Herbert Valerio Riedel](https://github.com/hvr))

## v0.8.0.6

_Released on 2019-03-29 (Martyrs' Day in Madagascar)._

#### Package changes

* Change QuickCheck upper bound from 2.13 to 2.14

## v0.8.0.5

_Released on 2018-09-13 (Day of the Programmer)._

#### Package changes

* Change QuickCheck upper bound from 2.12 to 2.13

## v0.8.0.4

_Released on 2018-01-19 (Kokborok Day)._

#### Package changes

* Change QuickCheck upper bound from 2.11 to 2.12
* Make `Data.DList` trustworthy
  ([Bertram Felgenhauer](https://github.com/int-e))
* Remove quickcheck-instances dependency for tests

## v0.8.0.3

_Released on 2017-07-04 (Independence Day in the United States)._

#### Package changes

* Change QuickCheck upper bound from 2.10 to 2.11 and import the `Arbitrary`
  `NonEmpty` instance from quickcheck-instances for 2.10
* Fix `stimes` property in test suite
  ([Oleg Grenrus](https://github.com/phadej))

## v0.8.0.2

_Released on 2016-09-04 (World Sexual Health Day)._

#### Package changes

* Fix test suite: add missing module `OverloadedStrings`
  ([Sergei Trofimovich](https://github.com/trofi))

## v0.8.0.1

_Released on 2016-07-29 (58th Anniversary of the Creation of NASA)._

#### Package changes

* Change QuickCheck lower bound to 2.9 for GHC >= 8 (base >= 4.9)
  ([Adam Bergmark](https://github.com/bergmark))

## v0.8

_Released on 2016-07-17 (Constitution Day in South Korea)._

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

## v0.7.1.2

_Released on 2015-08-23 (International Day for the Remembrance of the Slave Trade and its Abolition)._

#### Package changes

* Fix GHC 7.10 warnings due to imports
  ([Mikhail Glushenkov](https://github.com/23Skidoo))

## v0.7.1.1

_Released on 2015-03-19 (St. Joseph's Day)._

#### Package changes

* Change QuickCheck upper bound from 2.8 to 2.9

## v0.7.1

_Released on 2014-06-28 (100th Anniversary of the Assassination of Franz Ferdinand)._

#### New features

* Add `IsList` instance for GHC >= 7.8
  ([Baldur Blöndal](https://github.com/Icelandjack))

## v0.7.0.1

_Released on 2014-03-24 (World Tuberculosis Day)._

#### Package changes

* Change QuickCheck upper bound from 2.7 to 2.8

## v0.7

_Released on 2014-03-17 (St. Patrick's Day)._

#### New features

* Add `NFData` instance (and `deepseq` dependency)
* Add `IsString` instance
* Remove deprecated entities

## v0.6.0.1

_Released on 2013-12-01 (World AIDS Day)._

#### Package changes

* Change QuickCheck lower bound from 2.6 to 2.5
  ([Michael Snoyman](https://github.com/snoyberg))

## v0.6

_Released on 2013-11-29 (Black Friday)._

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

* Deprecate `DList` constructor and record selector to make it abstract
  (see [#4](https://github.com/spl/dlist/issues/4))
* Deprecate `maybeReturn` which is not directly relevant to dlists
