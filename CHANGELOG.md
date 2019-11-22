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
