#
build_ocaml:
	dune build

gen_data:
	./gen_data.sh && ./download_asts.sh

clean:
	rm -rf data/plummer_* && \
	rm -rf data/uniform_* && \
	rm -rf data/nested_asts && \
	rm -rf _build

.PHONY: clean gen_data build_ocaml
