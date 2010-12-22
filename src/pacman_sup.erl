%%% @author Knut Ivar Nesheim <knutin@gmail.com>
%%% @copyright (C) 2010, Knut Ivar Nesheim
%%% @doc
%%%
%%% @end

-module(pacman_sup).
-author('knutin@gmail.com').

-behaivour(supervisor).

-export([start_link/0]).

-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Server = {game_server, {game_server, start_link, []},
              permanent, 2000, worker, [game_server]},
    Children = [Server],
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.

