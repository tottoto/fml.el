.PHONY: test
test:
	emacs --batch -L . -l fml-test.el --eval '(ert-run-tests-batch-and-exit (quote t))'
