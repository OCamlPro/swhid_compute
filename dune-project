(lang dune 2.7)

(name swhid_compute)

(license ISC)

(authors "Léo Andrès <contact@ndrs.fr>")

(maintainers "Léo Andrès <contact@ndrs.fr>")

(source
 (github ocamlpro/swhid_compute))

(generate_opam_files true)

(package
 (name swhid_compute)
 (synopsis
  "OCaml library to work with Software Heritage identifiers, compute library used in swhid")
 (description
  "swhid is an OCaml library to work with persistent identifiers found in Software Heritage,  it contains the types module used by others libraries.")
 (tags
  (swh software heritage archive shwid persistent identifier compute))
 (depends
  (ocaml
   (>= 4.08))
  swhid_types
  (digestif :with-test)
  (bos :with-test)
  (bisect_ppx
   (and
    :with-test
    (>= 2.6)
    :dev))))
