(lang dune 3.16)

(name ofuck)

(generate_opam_files true)

(source
 (github Will-Banksy/ofuck))

(authors "WillBanksy")

(maintainers "WillBanksy")

(license MIT)

(package
 (name ofuck)
 (synopsis "Simple brainfuck interpreter")
 (description "Simple, low performance/inefficient implementation of brainfuck")
 (depends ocaml dune)
 (tags
  ("brainfuck" "interpreter")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
