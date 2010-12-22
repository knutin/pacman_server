%%% @author Knut Ivar Nesheim <knutin@gmail.com>
%%% @copyright (C) 2010, Knut Ivar Nesheim
%%% @doc
%%% Router for connecting a running game with one or more Comet web requests.
%%% A game worker will call router:state_update(Token, State) after Pacman and
%%% the ghost has moved. The router will then send a message to all processes
%%% registered for that token. The process may then decide to emit the new
%%% board state to it's client.
%%% @end

-module(router).
-author('knutin@gmail.com').

-behaivour(gen_server).

%% API
-export([state_update/2, register/2, unregister/2]).

state_update(Token, State) ->
    ok.

register(Pid, Token) ->
    ok.

unregister(Pid, Token) ->
    ok.

