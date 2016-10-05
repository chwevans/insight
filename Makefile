all:
	rebar3 compile

run:
	erl -pa _build/default/lib/*/ebin/ -s insight
