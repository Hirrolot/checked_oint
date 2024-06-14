# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## unreleased

### Added

 - The `is_zero`, `is_one`, `is_all_ones` predicates on `generic` integers.

### Changed

 - Rename `S.ty` to `S.int_ty` (in accordance with `generic_int_ty`).

## 0.1.2 - 2024-05-15

### Added

 - `S.of_int` and `S.of_string` which return `option` instead of raising an exception.

### Fixed

 - Specify a required version of `alcotest`.

## 0.1.1 - 2024-05-05

### Added

 - `S.ty` for getting a type representation (`int_ty`).

### Fixed

 - Specify required versions of `ocaml` and `ctypes-foreign` in `dune-project`.

## 0.1.0 - 2024-05-01

### Added

 - This library.
