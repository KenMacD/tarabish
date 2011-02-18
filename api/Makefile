all: clean
	mkdir -p target
	thrift --gen erl -gen py:new_style -o target tarabish.thrift

clean:
	rm -r target
