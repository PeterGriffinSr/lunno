# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Notes
- Project is under active early development.
- APIs and language features may change without notice.

## [0.2.2] - 2026-02-07

### Added
- Top-level `let` declaration parsing (named let bindings).

### Fixed
- Reduced parsing ambiguities by refining type/parameter productions; resolved several shift/reduce and reduce/reduce conflicts.

### Removed
- `in` keyword.

## [0.2.1] - 2026-02-05

### Changed
-  Removed unused library from dune

## [0.2.0] - 2026-02-05

### Added
- Initial parser infrastructure.
- Core AST definitions and data structures.

### Changed
- Reorganized project structure to improve dependency management and modularity.
- Simplified public API to expose only necessary libraries and interfaces.

## [0.1.0] - 2026-02-02

### Added
- Initial project changelog.
- Lexer with full token coverage.
- Span-based error reporting.

### Changed
- Stabilized token and error interfaces.
- Project rewritten from an early Go prototype to an OCaml implementation.

### Notes
- The Go version was an experimental proof-of-concept and was never released.