(lang dune 3.5)
(wrapped_executables false)
(using experimental_building_ocaml_compiler_with_dune 0.1)
(using menhir 2.1)

(use_standard_c_and_cxx_flags true)
(cram enable)

(package
  (name oxcaml)
)

(package
  (name ocaml)
)

(package
  (name ocaml_runtime_stdlib)
)

