all: clean build run

clean:
	rm -f *~ *.png *.gif main

build:
	raco exe main.rkt

run:
	./main
