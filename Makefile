all:
	rebar3 compile

.PHONY: test

test:
	rebar3 eunit --verbose

escript:
	rebar3 escriptize

check:
	rebar3 dialyzer

generate:
	./code-gen.es -f spec/FIX44.xml

nif: priv
	$(if $(DEBUG)$(NIF_DEBUG),REBAR_ENV=test )make -C c_src

clean:
	rebar3 clean

priv:
	mkdir -p $@
