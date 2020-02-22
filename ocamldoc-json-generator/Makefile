.PHONY: build doc build-test clean doc-test

build:
	cp ../native/src/Standard.ml ./_build/Standard.ml;
	cp ../native/src/Core.ml ./_build/Core.ml;
	cp ../native/src/Core.mli ./_build/Core.mli;
	ocamlfind ocamlopt \
		-package base,str,zarith \
		-linkpkg \
		-I ./_build \
		-o ./_build/example.exe \
		./_build/Core.mli \
		./_build/Core.ml \
		./_build/Standard.ml

doc:
	rm -f ../website/model.json && \
	ocamlfind ocamldoc \
		-package base,str,zarith \
		-I ./_build \
		-i ../_build/default/ocamldoc-json-generator/src/ \
		-g JsonGenerator.cma \
		-d ../website \
		./_build/Core.mli \
		./_build/Core.ml \
		./_build/Standard.ml

build-test:
	ocamlfind ocamlopt \
		-I ./test/src \
		-o ./test/src/example.exe \
		./test/src/Core.mli \
		./test/src/Core.ml \
		./test/src/Fourier.ml \
		./test/src/Included.ml \
		./test/src/ExampleEntry.ml 

clean: 
	rm -rf \
		../website/model.json \
		./_build/* \
		./test/model.json \
		./test/src/*.cmi \
		./test/src/*.cmx \
		./test/src/*.exe \
		./test/src/*.o
		
doc-test:
	rm -rf ./test-doc/* && \
	ocamlfind ocamldoc \
		-I ./test/src \
		-i ../_build/default/ocamldoc-json-generator/src/ \
		-g JsonGenerator.cma \
		-d ./test \
		./test/src/Core.mli \
		./test/src/Core.ml \
		./test/src/Fourier.ml \
		./test/src/Included.ml \
		./test/src/ExampleEntry.ml 

