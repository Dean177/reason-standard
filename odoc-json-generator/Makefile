.PHONY: all clean

build:
	ocamlfind ocamlopt \
		-I ./example \
		-o ./example/example.exe \
		./example/Core.mli \
		./example/Core.ml \
		./example/Fourier.ml \
		./example/Included.ml \
		./example/ExampleEntry.ml 

clean: 
	rm -f ./doc/* ./example/*.cmi ./example/*.cmx ./example/*.o
		
test:
	rm -rf ../doc/* && \
	ocamlfind ocamldoc \
		-I ./example \
	  -i ../_build/default/odoc-json-generator/src/ \
		-g JsonGenerator.cma \
		-d ./doc \
		./example/Core.mli \
		./example/Core.ml \
		./example/Fourier.ml \
		./example/Included.ml \
		./example/ExampleEntry.ml 
all:
	ocamlfind ocamldoc \
		-linkpkg \
		-package base,str,zarith \
		-I ../_build/install/default/reason-standard \
	  -i ../_build/default/odoc-json-generator/src/ \
		-g JsonGenerator.cma \
		-d ../native-doc \
		../native/src/Standard.ml 

	ocamldoc -i ../_build/default/odoc-json-generator/src/ -g JsonGenerator.cma -d ../gatsby-source-odoc/doc ../native/src/Standard.ml