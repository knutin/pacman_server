%%% @author Knut Ivar Nesheim <knutin@gmail.com>
%%% @copyright (C) 2010, Knut Ivar Nesheim
%%% @doc
%%%
%%% @end

-module(pacman_app).
-author('knutin@gmail.com').

-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    pacman_sup:start_link().

stop(_State) ->
    ok.

