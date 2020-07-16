# Change Log

## [v0.8.0.8][] - 2020-04-02

Released on **World Autism Awareness Day**.

### Added

* `toList` in the `Foldable` instance for `DList` ([#36][], [Ryan Scott][])

### Changed

* `QuickCheck` upper bound: 2.14 to 2.15 ([a7ea60d][])

### Fixed

* Documented time complexity of `head` for `DList` ([#35][], [Simon Jakobi][])

## [v0.8.0.7][] - 2019-08-05

Released on **Independence Day in Burkina Faso**.

### Added

* `MonadFail` instance for `DList` ([#32][], [Vanessa
  McHale](https://github.com/vmchale))

### Changed

* `deepseq` upper bound: 2 to 1.5 ([#33][], [Herbert Valerio Riedel][])

## [v0.8.0.6][] - 2019-03-29

Released on **Martyrs' Day in Madagascar**.

### Changed

* `QuickCheck` upper bound: 2.13 to 2.14 ([242511c][])

## [v0.8.0.5][] - 2018-09-13

Released on **Day of the Programmer**.

### Changed

* `QuickCheck` upper bound: 2.12 to 2.13 ([0e2b3a5][])

## [v0.8.0.4][] - 2018-01-19

Released on **Kokborok Day**.

### Added

* `{-# LANGUAGE Trustworthy #-}` in `Data.DList` ([#31][], [Bertram
  Felgenhauer](https://github.com/int-e))

### Changed

* `QuickCheck` upper bound: 2.11 to 2.12 ([3d9c8ad][])
* `QuickCheck` lower bound: 2.7/2.9 to 2.10 ([4f92012][])
* `Arbitrary`, `Arbitrary1` instances for `NonEmpty` in the test suite copied
  from `quickcheck-instances` ([4f92012][])

## [v0.8.0.3][] - 2017-07-04

Released on **Independence Day in the United States**.

### Added

* `quickcheck-instances` dependency in the test suite for the `Arbitrary`,
  `Arbitrary1` instances for `NonEmpty` ([5b41d0d][])

### Changed

* `QuickCheck` upper bound: 2.10 to 2.11 ([b2f791a][])

### Fixed

* `stimes` property in the test suite ([#30][], [Oleg
  Grenrus](https://github.com/phadej))

## [v0.8.0.2][] - 2016-09-04

Released on **World Sexual Health Day**.

### Fixed

* Missing module `OverloadedStrings` in the test suite ([#29][], [Sergei
  Trofimovich](https://github.com/trofi))

## [v0.8.0.1][] - 2016-07-29

Released on the **58th Anniversary of the Creation of NASA**.

### Changed

* `QuickCheck` lower bound: 2.7 to 2.9 for GHC >= 8 ([#28][], [Adam
  Bergmark](https://github.com/bergmark))

## [v0.8][] - 2016-07-17

Released on **Constitution Day in South Korea**.

### Added

* Pattern synonyms `Nil` and `Cons` for GHC >= 7.8
* `Semigroup` instance for GHC >= 8
* _In_flexible `IsString` instance for `DList` to improve overloaded string
  support ([#26][], [Baldur Blöndal](https://github.com/Icelandjack))
* Canonical `Applicative` and `Monad` instances ([#23][], [Herbert Valerio
  Riedel][])

### Changed

* `QuickCheck` upper bound: 2.9 to 2.10 ([ef7eac5][])

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
* Add Travis-CI ([Herbert Valerio Riedel][])

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

[#23]: https://github.com/spl/dlist/pull/23
[#26]: https://github.com/spl/dlist/pull/26
[#28]: https://github.com/spl/dlist/issues/28
[#29]: https://github.com/spl/dlist/pull/29
[#30]: https://github.com/spl/dlist/pull/30
[#31]: https://github.com/spl/dlist/pull/31
[#32]: https://github.com/spl/dlist/pull/32
[#33]: https://github.com/spl/dlist/pull/33
[#35]: https://github.com/spl/dlist/pull/35
[#36]: https://github.com/spl/dlist/pull/36

[0e2b3a5]: https://github.com/spl/dlist/commit/0e2b3a542796b50796f2aa6dde4665911b9d15a1
[242511c]: https://github.com/spl/dlist/commit/242511c501299b38c57efeafb9e604f29cb8bb7a
[3d9c8ad]: https://github.com/spl/dlist/commit/3d9c8ad348b419590a121b8a1604e8ebd01bffbe
[4f92012]: https://github.com/spl/dlist/commit/4f920128592f6f99b8c57a1adf50cdb16d26c13b
[5b41d0d]: https://github.com/spl/dlist/commit/5b41d0d84a0a14c75798ca30883b613b37ad464a
[a7ea60d]: https://github.com/spl/dlist/commit/a7ea60d3d02775216a15d6f688db230d7735c9d1
[b2f791a]: https://github.com/spl/dlist/commit/b2f791ab98e2091303fff4567727716b6021b63e
[ef7eac5]: https://github.com/spl/dlist/commit/ef7eac55fc7e180ac3441657f4971ed171b0669c

[Herbert Valerio Riedel]: https://github.com/hvr
[Ryan Scott]: https://github.com/RyanGlScott
[Simon Jakobi]: https://github.com/sjakobi
