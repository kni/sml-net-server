all:
	@echo "make targets: poly, mlton, clean."

poly: os-constants.sml 
	polyc -o t-poly t.mlp

mlton: os-constants.sml
	mlton -default-ann 'allowFFI true' -output t-mlton t.mlb

os-constants.sml: os-constants.c
	cc -o os-constants os-constants.c && ./os-constants > os-constants.sml && rm os-constants

clean:
	rm -rf t-poly t-mlton os-constants.sml
