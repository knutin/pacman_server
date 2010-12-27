#!/bin/bash
erl -pa ebin deps/*/ebin \
    -boot start_sasl \
    -s pacman
