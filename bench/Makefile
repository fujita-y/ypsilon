all:
	chezscheme run-chez.scm
	gosh run-gosh.scm
	GUILE_JIT_THRESHOLD=0 guile --no-debug run-guile.scm
	petite run-petite.scm
	sash run-sash.scm
	ypsilon run-ypsilon.scm

clean:
	rm -f *.out
	rm -f spheres.pgm
