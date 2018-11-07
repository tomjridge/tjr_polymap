DUNE:=opam exec dune

build:
	$(DUNE) build @install

install:
	$(DUNE) install

clean:
	$(DUNE) clean


doc: FORCE
	$(DUNE) build @doc

view_doc:
	google-chrome  _build/default/_doc/_html/index.html



FORCE:
