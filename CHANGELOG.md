# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Notes
- Project is under active early development.
- APIs and language features may change without notice.

### [0.8.0] - 2026-02-25

### Added
- `Typed_ast` module in `lunno.common`- a fully type-annotated AST where every
  node carries its resolved type, eliminating `option` and unresolved meta
  variables after typechecking.
- `Ty_utils` module in `lunno.common` exposing `string_of_ty`, `string_of_binop`,
  `string_of_unop`, and `string_of_span`, decoupling type pretty-printing from
  the debug printer.
- Range expression syntax `[a..b]` with `DotDot` lexer token, parsed as
  `Range of expr * expr * Span.t` and typechecked to `List[int]`.

### Changed
- Typechecker (`lunno.lower`) now produces `Typed_ast.program` instead of `unit`,
  making inferred types available to all downstream passes.
- Debug printer (`lunno.debug`) updated to consume `Typed_ast` - all nodes now
  display their resolved types with no `?` placeholders.
- `string_of_ty` for zero-argument functions now prints `Fun(unit)` instead of
  `Fun( -> unit)`.
- Renamed `lunno.analysis` to `lunno.debug` to better reflect its purpose.
- Renamed `lunno.middle` to `lunno.lower` to signal its role as the lowering
  pipeline (typechecking and future IR passes).
- `lunno.debug` and `lunno.lower` now depend on `Ty_utils` from `lunno.common`
  rather than reaching across library boundaries for type formatting.
- Updated `cli.mli` to reflect `typecheck` returning `Typed_ast.program`.

## [0.7.1] - 2026-02-23

### Removed
- Unused dune library.

## [0.7.0] - 2026-02-23

### Added
- HM-style type inference with `generalize` and `instantiate`, enabling let-polymorphism throughout the language.
- List literal syntax `[e1, e2, ...]`, desugaring to right-associative `::` chains terminated by `Nil`.
- `LNil` literal variant in the AST, inferred as `[?a]` (a fresh list of an unconstrained element type).
- Data-driven module system via `Registry`, with `import "namespace:item"` syntax and TyModule type.
- `MemberAccess` expression form (`module.member`) for accessing exported module members.
- Polymorphism test suite with 50 tests covering identity, K combinator, higher-order functions, let-generalization in blocks, unannotated recursive functions, list literals, and error cases.

### Changed
- Updated file/directory structure to reflect the module system and frontend/common/modules split.
- Recursive `let` bindings no longer require full type annotations — unannotated params and missing return types are now inferred via `fresh_meta ()` rather than raising MissingAnnotation.
- `infer_literal` extended to handle `LNil`.

### Fixed
- `unify` spuriously raised a type mismatch when unifying a metavariable with itself (`?n` vs `?n`), caused by the occurs check returning true on equal IDs. Fixed by adding a `TyMeta m, TyMeta m2 when m.id = m2.id -> ()` guard before the occurs check.

## [0.6.3] - 2026-02-20

### Changed
- Updated README.

### Removed
- Removed gh-pages docs generation from CI.

### Fixed
- Fixed numerous compiler warnings.

## [0.6.2] - 2026-02-20

### Fixed
- Dependency issue with ounit2.

## [0.6.1] - 2026-02-20

### Added
- Lunno opam versioning file.

## [0.6.0] - 2026-02-20

### Added
- Import expressions for module/file imports.
- Type checker with comprehensive validation.
- `error.ml` support for type errors and parser errors.
- Parser error diagnostics for better error reporting.
- Tests for type checker functionality.
- Updated public API with new features.

### Changed
- Updated Dune profiles for development and release compilation.
- Improved error handling and reporting system.

### Fixed
- Fixed numerous compiler warnings.

## [0.5.5] - 2026-02-18

### Added
- Added `--dump-program` flag to print the AST and exit.
- Added `--version` flag to print the current version and exit.
- Added `--help` flag with formatted usage output.
- CLI now supports multiple input files.

## [0.5.4] - 2026-02-18

### Fixed
- Fixed Windows build failures.

## [0.5.3] - 2026-02-18

### Fixed
- Fixed Windows build failures.

## [0.5.2] - 2026-02-17

### Fixed
- Corrected dune-workspace configuration for cross-platform CI builds.
- Fixed Windows build failures by switching to Dune package management.

### Changed
- Migrated CI workflow from opam-based dependency installation to Dune package management.
- Simplified dune-workspace by removing explicit context definitions.

## [0.5.1] - 2026-02-17

### Added
- Recursive flag support for lambda expressions.
- `if` and `match` expression syntax.
- Unit tests for `if` and `match` expression validation.
- Dune package lock file for reproducible builds.

### Changed
- AST literal node references.
- Unit tests for lexer and parser validation.
- Debug output format for `let` binding expressions.
- README

## [0.5.0] - 2026-02-13

### Added
- Unit tests for parser validation.
- Function declaration syntax support.
- Function call expression parsing.

### Changed
- Dune version from 3.20 to 3.21.
- Dune progject tags and configuration.
- Debug output format for `let` binding expression.

### Removed
- `function` keyword.

## [0.4.8] - 2026-02-10

### Added
- Unit test for span tracking validation.

### Changed
- Simplified lexer public API to expose only token entry point.

## [0.4.7] - 2026-02-07

### Fixed
- Corrected project versioning inconsistencies across the repository and release metadata.

### Notes
- Commits prior to this release are considered legacy and should be ignored for version history and support expectations.

## [0.4.6] - 2026-02-07

### Added
- Top-level `let` declaration parsing (named let bindings).

### Fixed
- Reduced parsing ambiguities by refining type/parameter productions; resolved several shift/reduce and reduce/reduce conflicts.

### Removed
- `in` keyword.

## [0.4.4] - 2026-02-05

### Changed
- Removed unused library from dune

## [0.4.3] - 2026-02-05

### Added
- Initial parser infrastructure.
- Core AST definitions and data structures.

### Changed
- Reorganized project structure to improve dependency management and modularity.
- Simplified public API to expose only necessary libraries and interfaces.

## [0.4.2] - 2026-02-02

### Added
- Project changelog.

### Changed
- Migrated error handling to use Lunno span tracking for diagnostic accuracy.

## [0.4.1] - 2026-02-01

### Added
- Span-based error handling infrastructure.

### Changed
- Improved lexer implementation and reliability.
- Expanded and refined documentation comments for interface files.

## [0.4.0] - 2026-01-30

### Added
- Initial automated test suite.

## [0.3.0] - 2026-01-22

### Added
- Structured test organization.
- README visual assets.
- Public interface file for the lexer.

### Changed
- Simplified driver implementation.
- Renamed project from `vsharp` to `lunno`.

## [0.2.1] - 2026-01-21

### Fixed
- Resolved CI workflow dependency installation failures.

## [0.2.0] - 2026-01-21

### Added
- Initial lexer implementation with early pseudo-span-based error handling.

## [0.1.0] - 2026-01-20

### Added
- Initial project scaffolding and repository structure.

### Notes
- The Go version was an experimental proof-of-concept and was never released.