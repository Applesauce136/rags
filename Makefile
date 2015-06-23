all: clean build run

clean:
	rm -f *~ *.png *.gif main
	rm -rf spinning_torus

build:
	raco exe main.rkt

run:
	./main
