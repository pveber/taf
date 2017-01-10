taf.js: taf.ml
	ocamlfind ocamlc -package ocaml-vdom -no-check-prims -linkpkg -o taf.byte taf.ml
	js_of_ocaml +gen_js_api/ojs_runtime.js -o $@ taf.byte
