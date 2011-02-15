
all: 
	mkdir -p ebin
	erl -make

test: all
	erl -pz ebin -noshell -s account test -s init stop

clean:
	rm ebin/*
	rm erl_crash.dump

start: all
	erl -pz ebin -noshell -s main start

