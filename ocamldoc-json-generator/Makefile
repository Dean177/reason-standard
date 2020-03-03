dummy:

CMO=JsonGenerator.cmo
CMA=JsonGenerator.cma
CMX=JsonGenerator.cmx
CMXS=$(CMA:.cma=.cmxs)

COMPFLAGS=-package menhirLib,ocamldoc,reason
CMA_DEPS=menhirLib.cmo migrate_parsetree.cma ReasonEasyFormat.cma reason.cma
CMXA_DEPS=menhirLib.cmx migrate_parsetree.cmxa ReasonEasyFormat.cmxa reason.cmxa

all: byte opt

byte: $(CMA)

opt: $(CMXS)

$(CMXS): $(CMO:.cmo=.cmx)
	ocamlfind ocamlopt $(COMPFLAGS) -shared -o $@ $(CMXA_DEPS) $^

$(CMA): $(CMO)
	ocamlfind ocamlc $(COMPFLAGS) -a -o $@ $(CMA_DEPS) $^

$(CMO): JsonGenerator.ml
	ocamlfind ocamlc $(COMPFLAGS) -c $<

$(CMX): JsonGenerator.ml
	ocamlfind ocamlopt $(COMPFLAGS) -c $<

install:byte opt
	mkdir -p `ocamlfind ocamldoc -customdir`
	cp -f $(CMA) $(CMXS) `ocamlfind ocamldoc -customdir`/

build-source: dummy
	cp ../native/src/Standard.ml ./_build/Standard.ml
	cp ../native/src/Core.ml ./_build/Core.ml
	cp ../native/src/Core.mli ./_build/Core.mli	
	ocamlfind ocamlopt \
		-package base,str,zarith \
		-I ./_build \
		-o ./_build/built.exe \
		./_build/Core.mli \
		./_build/Core.ml \
		./_build/Standard.ml

doc: dummy
	ocamldoc.opt \
		-g ./$(CMXS) \
		`ocamlfind query -i-format base` \
		`ocamlfind query -i-format str` \
		`ocamlfind query -i-format zarith` \
		-I ./_build \
		-d ../website \
		./_build/Core.mli \
		./_build/Core.ml \
		./_build/Standard.ml
		
test: dummy
	ocamlc -c ./test/Example.ml
	ocamldoc.opt \
		-g ./$(CMXS) \
		-d ./test/doc \
		./test/Example.ml 
	echo "Documented"
		
# doc-test:
# 	rm -rf ./test-doc/* && \
# 	ocamlfind ocamldoc \
# 		-I ./test/src \
# 		-i ../_build/default/ocamldoc-json-generator/src/ \
# 		-g JsonGenerator.cma \
# 		-d ./test \
# 		./test/src/Core.mli \
# 		./test/src/Core.ml \
# 		./test/src/Fourier.ml \
# 		./test/src/Included.ml \
# 		./test/src/ExampleEntry.ml 

# findlib-install: byte opt
# 	ocamlfind install JsonGenerator META LICENSE \
# 		$(CMA) $(CMXS) $(CMXS:.cmxs=.o)

# findlib-uninstall: byte opt
# 	ocamlfind remove JsonGenerator

clean:
	rm -f *.cm* *.o *.a ./test/*.cm* ./test/*.o ./test/*.a
