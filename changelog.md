# Change Log

## [v0.8.0.8][] - 2020-04-02

Released on **World Autism Awareness Day**.

### Added

* `toList` in the `Foldable` instance ([#36][], [Ryan Scott][])

### Changed

* `QuickCheck` upper bound from 2.14 to 2.15 ([a7ea60d][])

## [v0.8.0.7][] - 2019-08-05

Released on **Independence Day in Burkina Faso**.

### Package changes

* Add `MonadFail` instance for GHC >= 8.8
  ([Vanessa McHale](https://github.com/vmchale))
* Change `deepseq` upper bound to 1.5
  ([Herbert Valerio Riedel](https://github.com/hvr))

## [v0.8.0.6][] - 2019-03-29

Released on **Martyrs' Day in Madagascar**.

### Package changes

* Change QuickCheck upper bound from 2.13 to 2.14

## [v0.8.0.5][] - 2018-09-13

Released on **Day of the Programmer**.

### Package changes

* Change QuickCheck upper bound from 2.12 to 2.13

## [v0.8.0.4][] - 2018-01-19

Released on **Kokborok Day**.

### Package changes

* Change QuickCheck upper bound from 2.11 to 2.12
* Make `Data.DList` trustworthy
  ([Bertram Felgenhauer](https://github.com/int-e))
* Remove quickcheck-instances dependency for tests

## [v0.8.0.3][] - 2017-07-04

Released on **Independence Day in the United States**.

### Package changes

* Change QuickCheck upper bound from 2.10 to 2.11 and import the `Arbitrary`
  `NonEmpty` instance from quickcheck-instances for 2.10
* Fix `stimes` property in test suite
  ([Oleg Grenrus](https://github.com/phadej))

## [v0.8.0.2][] - 2016-09-04

Released on **World Sexual Health Day**.

### Package changes

* Fix test suite: add missing module `OverloadedStrings`
  ([Sergei Trofimovich](https://github.com/trofi))

## [v0.8.0.1][] - 2016-07-29

Released on the **58th Anniversary of the Creation of NASA**.

### Package changes

* Change QuickCheck lower bound to 2.9 for GHC >= 8 (base >= 4.9)
  ([Adam Bergmark](https://github.com/bergmark))

## [v0.8][] - 2016-07-17

Released on **Constitution Day in South Korea**.

### New features

* Add pattern synonyms `Nil` and `Cons` for GHC >= 7.8
* Add `Semigroup` instance for GHC >= 8 (base >= 4.9)
* Use inflexible instance for `IsString` to improve support for overloaded
  strings ([Baldur Blöndal](https://github.com/Icelandjack))

### Package changes

* Change QuickCheck upper bound from 2.9 to 2.10

### Development changes

* Add `-Wall -Werror` testing
* Add testing for GHC 8.0.1 to Travis-CI

## [v0.7.1.2][] - 2015-08-23

Released on **International Day for the Remembrance of the Slave Trade and its Abolition**.

### Package changes

* Fix GHC 7.10 warnings due to imports
  ([Mikhail Glushenkov](https://github.com/23Skidoo))

## [v0.7.1.1][] - 2015-03-19

Released on **St. Joseph's Day**.

### Package changes

* Change QuickCheck upper bound from 2.8 to 2.9

## [v0.7.1][] - 2014-06-28

Released on the **100th Anniversary of the Assassination of Franz Ferdinand**.

### New features

* Add `IsList` instance for GHC >= 7.8
  ([Baldur Blöndal](https://github.com/Icelandjack))

## [v0.7.0.1][] - 2014-03-24

Released on **World Tuberculosis Day**.

### Package changes

* Change QuickCheck upper bound from 2.7 to 2.8

## [v0.7][] - 2014-03-17

Released on **St. Patrick's Day**.

### New features

* Add `NFData` instance (and `deepseq` dependency)
* Add `IsString` instance
* Remove deprecated entities

## [v0.6.0.1][] - 2013-12-01

Released on **World AIDS Day**.

### Package changes

* Change QuickCheck lower bound from 2.6 to 2.5
  ([Michael Snoyman](https://github.com/snoyberg))

## [v0.6][] - 2013-11-29

Released on **Black Friday**.

### Development changes

* Maintenance and development taken over by Sean Leather
  ([Bas van Dijk](https://github.com/basvandijk))
* Migrate repository from http://code.haskell.org/~dons/code/dlist/ to
  https://github.com/spl/dlist
* Add Travis-CI ([Herbert Valerio Riedel](https://github.com/hvr))

### Package changes

* Stop supporting `base < 2`
* Fix tests and use `cabal test`
* Add scripts for running `hpc`
* Update documentation

### New features

* New type class instances: `Eq`, `Ord`, `Read`, `Show`, `Alternative`,
  and `Foldable`
* New function `apply` to use instead of `unDL`

### Deprecations

* Deprecate `DList` constructor and record selector to make it abstract
  (see [#4](https://github.com/spl/dlist/issues/4))
* Deprecate `maybeReturn` which is not directly relevant to dlists

[v0.6]: https://github.com/spl/dlist/compare/v0.5...v0.6
[v0.6.0.1]: https://github.com/spl/dlist/compare/v0.6...v0.6.0.1
[v0.7]: https://github.com/spl/dlist/compare/v0.6.0.1...v0.7
[v0.7.0.1]: https://github.com/spl/dlist/compare/v0.7...v0.7.0.1
[v0.7.1.1]: https://github.com/spl/dlist/compare/v0.7.1...v0.7.1.1
[v0.7.1.2]: https://github.com/spl/dlist/compare/v0.7.1.1...v0.7.1.2
[v0.7.1]: https://github.com/spl/dlist/compare/v0.7.0.1...v0.7.1
[v0.8]: https://github.com/spl/dlist/compare/v0.7.1.2...v0.8
[v0.8.0.1]: https://github.com/spl/dlist/compare/v0.8...v0.8.0.1
[v0.8.0.2]: https://github.com/spl/dlist/compare/v0.8.0.1...v0.8.0.2
[v0.8.0.3]: https://github.com/spl/dlist/compare/v0.8.0.2...v0.8.0.3
[v0.8.0.4]: https://github.com/spl/dlist/compare/v0.8.0.3...v0.8.0.4
[v0.8.0.5]: https://github.com/spl/dlist/compare/v0.8.0.4...v0.8.0.5
[v0.8.0.6]: https://github.com/spl/dlist/compare/v0.8.0.5...v0.8.0.6
[v0.8.0.7]: https://github.com/spl/dlist/compare/v0.8.0.6...v0.8.0.7
[v0.8.0.8]: https://github.com/spl/dlist/compare/v0.8.0.7...v0.8.0.8

[#36]: https://github.com/spl/dlist/pull/36

[a7ea60d]: https://github.com/spl/dlist/commit/a7ea60d3d02775216a15d6f688db230d7735c9d1

[Ryan Scott]: https://github.com/RyanGlScott
