REBAR = ./rebar

clean:
	${REBAR} clean

get-deps:
	${REBAR} get-deps

compile:
	${REBAR} compile

run:
	@erl -pa deps/*/ebin -config app -pa ebin -s redis_proxy

compile-run: compile run
