.PHONY: run dev test demo demo-realistic

run:
	@sbcl --script scripts/run.lisp

dev:
	@sbcl --script scripts/dev.lisp

test:
	@sbcl --script tests/test.lisp

demo:
	@sbcl --script src/demo.lisp

demo-realistic:
	@sbcl --script src/demo-realistic.lisp
