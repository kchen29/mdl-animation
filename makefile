sbcl := sbcl --noinform --non-interactive --load "load.lisp" --eval
script := robot.mdl

all:
	$(sbcl) '(compile-mdl "$(script)")'

clean:
	rm -f *~ *.fasl *.ppm *.png
