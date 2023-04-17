all: kompile/.exec kompile/.type

common = corolts-syntax.k
.PHONY: clean run exec type

kompile/.exec: corolts-execute.k $(common)
	kompile corolts-execute.k --syntax-module COROLTS-SYNTAX -d kompile/exec
	echo "kompiled" > kompile/.exec

kompile/.type: corolts-typing.k $(common)
	kompile corolts-typing.k --syntax-module COROLTS-SYNTAX -d kompile/type
	echo "kompiled" > kompile/.type

%.ct: tests/%.ct kompile/.type
	krun $< -d kompile/type

clean: 
	rm -rf kompile/*
