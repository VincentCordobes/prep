(lang dune 1.11)
(name prep)
(license MIT)
(authors "Vincent Cordobes <vincent.cordobes@gmail.com>")
(maintainers "Vincent Cordobes <vincent.cordobes@gmail.com>")
(source (github VincentCordobes/prep))
(homepage "https://github.com/VincentCordobes/prep")

(generate_opam_files true)

(package
 (name prep)
 (synopsis "Use spaced-repetition to learn new skills")
 (depends
  (ocaml (>= 4.08))
  (dune (>= 2.0))
  (ppx_expect :with-test)
  base
  stdio
  ppx_deriving
  ppx_deriving_yojson
  fmt
  cmdliner
  yojson
  ISO8601
  uuidm
 ))
