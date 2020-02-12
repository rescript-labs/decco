## Unreleased version
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
