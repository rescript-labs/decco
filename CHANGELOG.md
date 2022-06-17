## Unreleased
### Added
* optionFromJson now supports undefined values (([#80](https://github.com/reasonml-labs/decco/pull/80)))

## v1.5.0
### Added
* windows support ([#20](https://github.com/reasonml-labs/decco/pull/20))

## v1.4.0
### Added
* Support for Polyvariants ([#64](https://github.com/reasonml-labs/decco/pull/64))
* Add support for bs-platform 9 ([#67](https://github.com/reasonml-labs/decco/pull/67))
* Integrate ppxlib, allowing decco to be used with other versions of OCaml ([#68](https://github.com/reasonml-labs/decco/pull/68))
* Run Ppx_decco as a standalone ppx ([#70](https://github.com/reasonml-labs/decco/pull/70))

### Fixed
* Build error on some platforms ([#66](https://github.com/reasonml-labs/decco/pull/66))

## v1.3.0
### Added
* bs-platform@8 to peer dependency ([#58](https://github.com/reasonml-labs/decco/pull/58))
* Support for \[@unboxed\]([#60](https://github.com/reasonml-labs/decco/pull/60))

### Fixed
* Remove unnecessary postinstall hook ([#59](https://github.com/reasonml-labs/decco/pull/59))

## v1.2.1
### Fixed
* Mutual recursion ([#46](https://github.com/reasonml-labs/decco/pull/46))

## v1.2.0
### Added
* Support for Js.Dict.t ([#48](https://github.com/reasonml-labs/decco/pull/48))

## v1.1.1
### Fixed
* Generated codecs no longer emit warning 4 ([#41](https://github.com/reasonml-labs/decco/issues/41)/[#42](https://github.com/reasonml-labs/decco/pull/42))

## v1.1.0
### Added
* Added support for BuckleScript v7

### Fixed
* Namespaced `Codecs.re` so as not to conflict with users' `Codecs.re` ([#25](https://github.com/reasonml-labs/decco/issues/25)/[#35](https://github.com/reasonml-labs/decco/pull/35))

## v1.0.0
### Changed
* Change package name from `@ryb73/decco` to `decco`
* **BREAKING CHANGE:** Move ppx executable from `ppx/ppx_decco.sh` to `ppx`
    * Migration: in `bsconfig.json`, change `"ppx-flags": [ "@ryb73/decco/ppx/ppx_decco.sh" ]"` to `"ppx-flags": [ "decco/ppx" ]`

## v0.2.2 / v0.1.1
### Fixed
* Fix error that occurs when Belt is open
* Properly handle variant decoding case where JSON input is an empty array (previously threw an exception; now properly returns a Decco error)

## v0.2.1
### Added
* Support for recursive types

## v0.2.0
### Added
* **BREAKING CHANGE:** Support for `bs-platform` ^6.0.0, dropped support for ^5.0.0

## v0.1.0
### Added
* `Belt.Result.t` support
