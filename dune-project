(lang dune 3.20)

(name taf)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name taf)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
   cmdliner
   notty
   ocaml
   otoml
   uuseg
   xdg)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
