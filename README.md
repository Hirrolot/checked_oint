# `checked_oint`

`checked_oint` is an OCaml library for checked integer arithmetic. We support the full set of signed and unsigned integers of bitnesses 8, 16, 32, 64, and 128.

## Installation

```
$ opam install checked_oint
```

## Usage

```ocaml
open Checked_oint

let () =
  let x = U8.of_int_exn 50 in
  let y = U8.of_int_exn 70 in
  assert (U8.equal (U8.add_exn x y) (U8.of_int_exn 120));
  assert (Option.is_none (U8.mul x y))
```

You can find the API documentation [here](https://hirrolot.github.io/checked_oint/checked_oint/Checked_oint/index.html).

## Release procedure

 1. Update the `version` field in `dune-project`.
 1. Type `dune build` to generate `checked_oint.opam`.
 1. Update `CHANGELOG.md`.
 1. Release the project in [GitHub Releases].
 1. Type `git pull && opam publish`.

[GitHub Releases]: https://github.com/Hirrolot/checked_oint/releases
