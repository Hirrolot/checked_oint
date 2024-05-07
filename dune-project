(lang dune 3.14)

(name checked_oint)

(version 0.1.1)

(generate_opam_files true)

(source
 (github Hirrolot/checked_oint))

(authors "Tima Kinsart <hirrolot@gmail.com>")

(maintainers "Tima Kinsart <hirrolot@gmail.com>")

(license MIT)

(package
 (name checked_oint)
 (synopsis "An OCaml library for checked integer arithmetic")
 (depends
  (ocaml
   (>= 4.13))
  dune
  ctypes
  (ctypes-foreign
   (>= 0.21.1))
  ppx_deriving
  ppx_enumerate
  bisect_ppx
  (alcotest
   (>= 1.7.0))))
