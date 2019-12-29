(lang dune 1.11)
(name rehearsal)
(license MIT)
(authors "Vincent Cordobes <vincent.cordobes@gmail.com>")
(source (github VincentCordobes/rehearsal))
(homepage "https://github.com/VincentCordobes/rehearsal")

(generate_opam_files true)

(package
 (name rehearsal)
 (synopsis "Learn new skills based on the spaced repetition idea")
 (depends
  (ocaml (>= 4.08))
  (dune (>= 1.11))))
