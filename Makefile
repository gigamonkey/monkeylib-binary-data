test:
	@sbcl --noinform --eval '(ql:quickload :binary-io :silent t)' \
			 --eval '(asdf:test-system :binary-io)' \
			 --eval '(sb-ext:quit)'
