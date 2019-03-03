FLAG = -Wall --omit-frame-pointer -fno-asynchronous-unwind-tables

compiler:
	gcc runtime.c build/compiler.s $(FLAG) -o build/compiler

test:
	scheme --script test_compiler.scm

.PHONY: clean
.SILENT: clean
clean:
	rm -f build/*
	rm -f *.out
