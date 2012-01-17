all: clean
	mkdir -p target
	thrift -gen js --gen erl -gen py:new_style -o target tarabish.thrift

clean:
	mkdir -p target
	find ./target/ -type f -delete
