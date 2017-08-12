taf.js: taf.ml
	ocamlfind ocamlc -package ocaml-vdom -package base -package parsexp -package ppx_sexp_conv -no-check-prims -linkpkg -o taf.byte taf.mli taf.ml
	js_of_ocaml +gen_js_api/ojs_runtime.js +base/runtime.js +weak.js -o $@ taf.byte

clean:
	rm -f taf.byte taf.cmi taf.cmo taf.js
