
all: 
	rebar3 compile

unit: all
	rebar3 eunit

system:
	rebar3 ct --suite test/system_SUITE

tar:
	rebar3 as prod tar

release:
	rebar3 as prod release

clean:
	rm -rf _build
	rm -f erl_crash.dump

start: all
	erl -env ERL_LIBS _build/default/lib -eval 'application:ensure_all_started(tarabish).' -noshell
