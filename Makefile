PROJ=scribester
REBAR=`which rebar || echo ./rebar`
.PHONY: all compile compile_deps clean eunit test eqc doc check dialyzer deps cleandeps tags
DIRS=src 

#all: compile eunit doc
all: compile

deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean skip_deps=true
	-rm log/*

rel: compile
	-rm -r rel/$(PROJ)
	$(REBAR) -C rebar.rel.config generate

cleandeps:
	$(REBAR) delete-deps

eunit:
	$(REBAR) skip_deps=true eunit

test: eunit

doc:
	$(REBAR) doc

tags:
	erl -s tags subdir "./" -s init stop -noshell

run:
	./start.sh
