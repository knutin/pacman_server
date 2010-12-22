all: compile

compile:
	cd src; make 

run:
	erl -pa ../gen_nb_server/ebin/ -pa ebin -s game_server start
