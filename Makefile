LIBS := lib/misultin/ebin
compile:
	./rebar compile
	make -C lib/misultin all
clean:
	./rebar clean
	make -C lib/misultin clean

run: compile
	erl -pa ebin -pa $(LIBS)
