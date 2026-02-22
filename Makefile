.PHONY: run dev test demo demo-realistic demo-tailwind

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

demo-tailwind:
	@sbcl --script src/demo-tailwind.lisp
