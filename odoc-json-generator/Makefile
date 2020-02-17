.PHONY: gen build doc build-test clean-test doc-test

gen:
	rm -rf ./gen/*;
	cp ../native/src/Standard.ml ./gen/Standard.ml;
	refmt --print ml ../native/src/Core.re > ./gen/Core.ml;
	refmt --print ml ../native/src/Core.rei > ./gen/Core.mli

build:
	ocamlfind ocamlopt \
		-package base,str,zarith \
		-linkpkg \
		-I ./gen \
		-o ./gen/example.exe \
		./gen/Core.mli \
		./gen/Core.ml \
		./gen/Standard.ml

doc:
	rm -rf ./doc/* && \
	ocamlfind ocamldoc \
		-package base,str,zarith \
		-I ./gen \
		-i ../_build/default/odoc-json-generator/src/ \
		-g JsonGenerator.cma \
		-d ./doc \
		./gen/Core.mli \
		./gen/Core.ml \
		./gen/Standard.ml

build-test:
	ocamlfind ocamlopt \
		-I ./example \
		-o ./example/example.exe \
		./example/Core.mli \
		./example/Core.ml \
		./example/Fourier.ml \
		./example/Included.ml \
		./example/ExampleEntry.ml 

clean-test: 
	rm -f ./doc/* ./example/*.cmi ./example/*.cmx ./example/*.o
		
doc-test:
	rm -rf ./test-doc/* && \
	ocamlfind ocamldoc \
		-I ./example \
		-i ../_build/default/odoc-json-generator/src/ \
		-g JsonGenerator.cma \
		-d ./test-doc \
		./example/Core.mli \
		./example/Core.ml \
		./example/Fourier.ml \
		./example/Included.ml \
		./example/ExampleEntry.ml 

