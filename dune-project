(lang dune 1.11)
(name spaced-repetition)
(license MIT)
(authors "Vincent Cordobes <vincent.cordobes@gmail.com>")
(source (github VincentCordobes/spaced-repetition))
(homepage "https://github.com/VincentCordobes/spaced-repetition")

(generate_opam_files true)

(package
 (name spaced-repetition)
 (synopsis "Learn new skills based on the spaced repetition idea")
 (depends
  (ocaml (>= 4.08))
  (dune (>= 1.11))))
