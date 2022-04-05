(lang dune 3.0)

(name swhid_compute)

(license ISC)

(authors "Léo Andrès <contact@ndrs.fr>")

(maintainers "Léo Andrès <contact@ndrs.fr>")

(source
 (github ocamlpro/swhid_compute))

(generate_opam_files true)

(package
 (name swhid_compute)
 (synopsis "OCaml library to work with Software Heritage identifiers")
 (description
  "swhid is an OCaml library to work with persistent identifiers found in Software Heritage")
 (tags
  (swh software heritage archive shwid persistent identifier))
 (depends
  (ocaml
   (>= 4.08))
  swhid_types
  (digestif
    :with-test)
  (bos
    :with-test)
  (bisect_ppx
   (and
    :with-test
    (>= 2.6)
    :dev))))
