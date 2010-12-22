#!/bin/bash
erl -pa ebin deps/*/ebin ../gen_nb_server/ebin/ \
    -boot start_sasl \
    -s pacman
