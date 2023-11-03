# This Makefile is provided for convenience.  This project is compiled using
# Dune, but course at Swarthmore typically provide projects which support
# "make".

.PHONY: build
build: make_output_dirs
	dune build
	@[ -f _build/default/src/main/hatch.bc ] && ( rm -f ./hatch; ln -s _build/default/src/main/hatch.bc ./hatch )
	@[ -f _build/default/src/tests/tests.bc ] && ( rm -f ./tests; ln -s _build/default/src/tests/tests.bc ./tests )

.PHONY: test
test: build
	./tests

.PHONY: clean
clean:
	dune clean
	rm -rf _build
	@rm -f ./hatch
	@rm -f ./tests
	rm -rf logs
	rm -rf output

.PHONY: make_output_dirs
make_output_dirs:
	@bash ./make_output_dirs.sh
