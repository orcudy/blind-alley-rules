CC=ocamlopt
BUILDDIR=build
EXE=main
all: utils.ml points.ml rules.ml main.ml
	@echo "Building project"
	$(CC) $^ -o $(EXE)
	@mkdir $(BUILDDIR)
	@mv *.cm* $(BUILDDIR)
	@mv *.o $(BUILDDIR)

clean:
	@echo "Cleaning up"
	@rm *.o *.cm* *~ 2>/dev/null || /bin/true
	@rm -rf $(BUILDDIR) || /bin/true
	@rm $(EXE) 2>/dev/null || /bin/true
