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
    Web = web_specs(pacman_web, 8080),
    Router = router_specs(),
    Children = [Server, Web, Router],
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.

router_specs() ->
    {router,
     {router, start_link, []},
     permanent, 5000, worker, [router]}.

web_specs(Mod, Port) ->
    WebConfig = [{ip, {0,0,0,0}},
                 {port, Port}],

    {Mod,
     {Mod, start, [WebConfig]},
     permanent, 5000, worker, dynamic}.
