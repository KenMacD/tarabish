
all: 
	mkdir -p ebin
	erl -make

test: all
	erl -pz ebin -noshell -s account test \
	                      -s deck test \
			      -s rules test \
	                      -s game test -s init stop

test_ct:
	mkdir -p test_log
	ct_run -pa ebin/ -logdir test_log -suite test/system_SUITE

clean:
	rm -f ebin/*
	rm -f erl_crash.dump

start: all
	erl -pz ebin -noshell -s main start

