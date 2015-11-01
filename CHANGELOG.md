## [Unreleased]
### Added
- `IsString` instances for `Id` and `StringLit` data types.
- Conditionally use the lightweight haskell-exp-parser instead of haskell-src-meta.

### Fixed
- #55 Comments at the top of a block before a declaration.

## [0.11.2] - 2015-09-29
### Added
- `qqPat` and `qqExp` are now exposed.

### Changed
- Bump upper bound on `syb`.
- Providing a starting position is now optional when parsing.

## [0.11.1] - 2015-09-29
### Added
- Automatically-generated `Relocatable` instances added for C abstract syntax types.

### Changed
- `Located` instances are also now automatically generated.

[0.11.2]: https://github.com/mainland/language-c-quote/compare/language-c-quote-0.11.1...language-c-quote-0.11.2
[0.11.1]: https://github.com/mainland/language-c-quote/compare/language-c-quote-0.11.0.1...language-c-quote-0.11.1
