REBAR := rebar3

all: start
build:
	$(REBAR) compile

start: build
	$(REBAR) shell

clean:
	$(REBAR) clean
	-rm -rf _build rebar.lock rebar3.crashdump
	-rm -rf *~
	-rm -rf examples/MultipleServerModules/*~
	-rm -rf examples/DiningPhilosophers/*~
