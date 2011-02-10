
all: mk-ebin gen-src
	erl -make

test: all
	erl -pz target/ebin -noshell -s account test -s init stop

clean:
	rm -r target
	rm erl_crash.dump

client:
	PYTHONPATH=$(PYTHONPATH):target/generated-sources/gen-py python -i src/main/python/client.py

start: all
	erl -pz target/ebin -noshell -s main start

mk-ebin:
	mkdir -p target/ebin

clean-gen-src:
	mkdir -p target/generated-sources
	rm -r target/generated-sources

create-gen-src:
	mkdir -p target/generated-sources

gen-src: clean-gen-src create-gen-src src/main/thrift/tarabish.thrift
	thrift --gen erl -gen py:new_style -o target/generated-sources src/main/thrift/tarabish.thrift

