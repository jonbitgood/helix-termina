# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

* Implement legacy console API for Windows ([#16](https://github.com/helix-editor/termina/pull/16))

### Fixed

* Fix reading VT data which includes non-ASCII characters from Windows ([#15](https://github.com/helix-editor/termina/pull/15))

## [v0.1.1] - 2025-09-12

### Added

* Expose the `Parser` type ([#12](https://github.com/helix-editor/termina/pull/12))

### Fixed

* Fix overflowing subtraction on large mouse positions without SGRMouse enabled ([#11](https://github.com/helix-editor/termina/pull/11))
* Fix Illumos build by avoiding compiling macOS-specific polling functions ([#13](https://github.com/helix-editor/termina/pull/13))

## [v0.1.0] - 2025-08-31

### Added

* Initial publish
