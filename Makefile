taf.js: taf.ml
	ocamlfind ocamlc -package ocaml-vdom -package base -package ppx_sexp_conv -no-check-prims -linkpkg -o taf.byte taf.ml
	js_of_ocaml +gen_js_api/ojs_runtime.js +base/runtime.js +weak.js -o $@ taf.byte
