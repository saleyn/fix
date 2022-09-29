all: compile #escript

compile:
	mix compile

.PHONY: test

test:
	rebar3 eunit --verbose

escript:
	mix escript.build

check:
	rebar3 dialyzer

generate:
	./code-gen.es -f spec/FIX44.xml

nif: priv
	$(if $(DEBUG)$(NIF_DEBUG),REBAR_ENV=test )make -C c_src

clean:
	mix clean

priv:
	mkdir -p $@
