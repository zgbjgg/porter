###----------------------------------------------------------------------
### 
### Makefile
###
### Created by : Jorge Garrido <jorge.garrido@morelosoft.com> [zgb]
###----------------------------------------------------------------------

.PHONY: porter

all: build

build:
	erl -make

clean:
	@rm -rf ebin/*.beam

start:
	@erl -pa ebin/ -eval 'ok = application:start(porter).'
