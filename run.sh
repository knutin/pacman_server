#!/bin/bash
erl -pa ebin deps/*/ebin ../gen_nb_server/ebin/ \
    -s game_server start