(* TEST
 readonly_files = "a.ml b.ml c.ml";
 setup-ocamlc.byte-build-env;
 module = "a.ml";
 ocamlc.byte;
 module = "b.ml";
 ocamlc.byte;
 module = "c.ml";
 ocamlc.byte;
 script = "rm -f a.cmi a.cmo c.cmi c.cmo";
 script;
 module = "c.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/test.missing.reference";
 check-ocamlc.byte-output;
*)

(* Note this tests also checks that serialization of kinds works as expected,
   as we check that things work before deleting a. *)
