all: kompile/.exec kompile/.type

common = corolts-syntax.k
.PHONY: clean run exec type

kompile/.exec: corolts-execute.k $(common)
	kompile corolts-execute.k --syntax-module COROLTS-SYNTAX -d kompile/exec
	echo "kompiled" > kompile/.exec

%.ct: tests/%.ct kompile/.exec
	krun $< -d kompile/exec

clean: 
	rm -rf kompile/*
