%%% @author Knut Ivar Nesheim <knutin@gmail.com>
%%% @copyright (C) 2010, Knut Ivar Nesheim
%%% @doc
%%%
%%% @end

-module(pacman).
-author('knutin@gmail.com').

-export([start/0, stop/0]).

start() ->
    application:start(pacman_server).

stop() ->
    application:stop(pacman_server).

