LISP = clisp

all : tests compare

tests : force
	$(LISP) -i tests.lsp -x '(run-all-tests)'

compare : force
	(echo *.erg | grep '\*') >/dev/null || (echo "Test failed." ; exit 1)
	echo "Test passed."

force :

