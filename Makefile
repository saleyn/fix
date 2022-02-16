all:
	./rebar3 compile

.PHONY: test

test:
	./rebar3 eunit --verbose

generate:
	./code-gen.es -f spec/FIX44.xml

nif:
	make -C c_src

clean:
	./rebar3 clean
