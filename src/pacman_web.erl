%%% @author Knut Ivar Nesheim <knutin@gmail.com>
%%% @copyright (C) 2010, Knut Ivar Nesheim
%%% @doc
%%%
%%% @end

-module(pacman_web).
-author('knutin@gmail.com').

-export([start/1, stop/0, loop/1]).

start(Options) ->
    Loop = fun(Req) ->
                   ?MODULE:loop(Req)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req) ->
    error_logger:info_msg("Web: Path: ~p~n", [Req:get(path)]),
    Req:not_found().


