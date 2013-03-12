###----------------------------------------------------------------------
### 
### Makefile
###
### Created by : Jorge Garrido <jorge.garrido@morelosoft.com> [zgb]
###----------------------------------------------------------------------

.PHONY: porter test

all: build

build:
	erl -make

clean:
	@rm -rf ebin/*.beam

demo:
	@erl -pa ebin/ -eval 'ok = application:start(porter).'

test:
	@erlc -o test/ test/*.erl
	@erl -pa ebin/ test/ -eval 'eunit:test('porter_sup_test', [verbose])' -noshell -s init stop
	@rm -rf test/*.beam
