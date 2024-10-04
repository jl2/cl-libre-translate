# Build cl-libre-translate demo application, to-spanish

to-spanish: manifest.txt *.lisp *.asd
	buildapp --output to-spanish \
             --manifest-file manifest.txt \
             --load-system asdf \
             --load-system sb-posix \
             --load-system alexandria \
             --load-system cl-libre-translate\
             --entry 'cl-libre-translate:main'

test: t/*.lisp *.lisp *.asd
	sbcl --eval "(ql:quickload :cl-libre-translate.test)" \
		 --eval "(setf 5am::*on-error* :debug)" \
		 --eval "(5am:run-all-tests :summary :suite)" \
		 --eval "(quit)"

manifest.txt: *.asd
	sbcl --no-userinit \
         --no-sysinit \
         --non-interactive \
         --load ~/quicklisp/setup.lisp \
         --eval '(ql:quickload :alexandria)' \
		 --eval '(ql:write-asdf-manifest-file "manifest.txt")'

clean:
	rm -Rf manifest.txt  *.fasl

.PHONY: clean test
