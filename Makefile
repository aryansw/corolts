all: kompile/.exec kompile/.type

common = corolts-syntax.k corolts-arith.k corolts-execute.k  
.PHONY: clean run exec type

kompile/.exec: corolts.k $(common)
	kompile corolts.k -d kompile/exec
	echo "kompiled" > kompile/.exec

%.ct: tests/%.ct kompile/.exec
	krun $< -d kompile/exec

clean: 
	rm -rf kompile/*
