-module(game_server).
-author('knutin@gmail.com').

-include_lib("../include/game.hrl").

-behaviour(gen_nb_server).

-export([start/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         sock_opts/0,
         new_connection/2]).

start() -> 
    start_link("0.0.0.0", 2222).

start_link(IpAddr, Port) ->
    gen_nb_server:start_link(?MODULE, IpAddr, Port, []).

init([]) ->
    ets:new(?MAP_TABLE, [set, named_table, public]),
    game_engine:load_maps(),
    error_logger:info_msg("Server started OK~n"),
    {ok, []}.

handle_call(_Request, _From, State) -> {noreply, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
    
    
sock_opts() ->    
    [binary, {active, once}, {packet, raw}, {reuseaddr, true}].

new_connection(Sock, State) ->
    {ok, _Pid} = game_worker:start(Sock),
    {ok, State}.
    


