## [0.11.7] - 2016-10-03
### Fixed
- Fix compatibility with `haskell-src-meta` 0.7.0.

## [0.11.6.2] - 2016-08-30
### Fixed
- Fix alignment of pretty-printed comments.

## [0.11.6.2] - 2016-05-18
### Fixed
- #68 Cannot create block consisting of a single statements antiquote

## [0.11.6.1] - 2016-05-10
### Added
- Bump `syb` upper bound for GHC 8.0.1 compatibility.

## [0.11.6] - 2016-04-12
### Added
- Add support for type qualifier escapes.

## [0.11.5.1] - 2016-04-07
### Fixed
- Try to be -Wparentheses clean.
- Fix pretty-pretty of dangling else.
- Add missing test modules to tarball.

## [0.11.5] - 2016-03-30
### Added
- Added ToConst and ToExp instances for Int/Word types (Emil Axelsson)
- Expression and statement raw string escapes (Kosyrev Serge)
- Partial support for C++11 lambda expressions in CUDA code (Michał Wawrzyniec Urbańczyk)

### Fixed
- #64 Negated negative constants do not correctly pretty-print.
- #59 No obvious way to generate macro code -- not even through $esc:(... :: String)
- #51 Objective-C anti-quotations for interface decls broken

## [0.11.4] - 2015-12-22
### Added
- Conditionally use the lightweight haskell-exp-parser instead of haskell-src-meta.

### Fixed
- #57 language-c-quote-0.11.3 does not compile with alex 3.1.5

## [0.11.3] - 2015-10-14
### Added
- `IsString` instances for `Id` and `StringLit` data types.

### Fixed
- #55 Comments at the top of a block before a declaration.

## [0.11.2.1] - 2015-10-06
### Added
- Type qualifiers are now allowed before an antiquoted type.

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

[0.11.6.3]: https://github.com/mainland/language-c-quote/compare/language-c-quote-0.11.6.2...language-c-quote-0.11.6.3
[0.11.6.2]: https://github.com/mainland/language-c-quote/compare/language-c-quote-0.11.6.1...language-c-quote-0.11.6.2
[0.11.6.1]: https://github.com/mainland/language-c-quote/compare/language-c-quote-0.11.6...language-c-quote-0.11.6.1
[0.11.6]: https://github.com/mainland/language-c-quote/compare/language-c-quote-0.11.5.1...language-c-quote-0.11.6
[0.11.5.1]: https://github.com/mainland/language-c-quote/compare/language-c-quote-0.11.5...language-c-quote-0.11.5.1
[0.11.5]: https://github.com/mainland/language-c-quote/compare/language-c-quote-0.11.4...language-c-quote-0.11.5
[0.11.4]: https://github.com/mainland/language-c-quote/compare/language-c-quote-0.11.3...language-c-quote-0.11.4
[0.11.3]: https://github.com/mainland/language-c-quote/compare/language-c-quote-0.11.2...language-c-quote-0.11.3
[0.11.2.1]: https://github.com/mainland/language-c-quote/compare/language-c-quote-0.11.2...language-c-quote-0.11.2.1
[0.11.2]: https://github.com/mainland/language-c-quote/compare/language-c-quote-0.11.1...language-c-quote-0.11.2
[0.11.1]: https://github.com/mainland/language-c-quote/compare/language-c-quote-0.11.0.1...language-c-quote-0.11.1
