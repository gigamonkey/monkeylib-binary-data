test:
	@sbcl --noinform --eval '(asdf:test-system :binary-io)' \
			 --eval '(sb-ext:quit)'
