all: run

common = corolts-syntax.k
.PHONY: clean run

kompile/.exec: corolts-execute.k $(common)
	kompile corolts-execute.k --syntax-module COROLTS-SYNTAX -d kompile/exec
	echo "kompiled" > kompile/.exec

kompile/.type: corolts-typing.k $(common)
	kompile corolts-typing.k --syntax-module COROLTS-SYNTAX -d kompile/type
	echo "kompiled" > kompile/.type

run: kompile/.exec kompile/.type
	krun "tests/$(file)" -d kompile/type
	krun "tests/$(file)" -d kompile/exec

clean: 
	rm -rf kompile/*
