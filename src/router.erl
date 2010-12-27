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

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


%% API
-export([state_update/2, register/2, unregister/2, clear/1, start_link/0]).

-record(router, {
          token2pid, % ets table reference
          pid2token  % ets table reference
         }).

-define(SERVER, {global, ?MODULE}).

start_link() ->
    gen_server:start_link(?SERVER, ?MODULE, [], []).

state_update(Token, State) ->
    gen_server:call(?SERVER, {state_update, Token, State}).

register(Pid, Token) ->
    gen_server:call(?SERVER, {register, Pid, Token}).

unregister(Pid, Token) ->
    gen_server:call(?SERVER, {unregister, Pid, Token}).

%% @doc: Removes all pids connected to Token
clear(Token) ->
    gen_server:call(?SERVER, {clear, Token}).


init([]) ->
    {ok, #router{token2pid = ets:new(?MODULE, [bag])}}.

handle_call({state_update, Token, GameState}, _From, State) ->
    Pids = lists:map(fun({_Token, P}) -> P end,
                     ets:lookup(State#router.token2pid, Token)),
    error_logger:info_msg("Router: Messaging ~p~n", [Pids]),

    Msg = {state_update, GameState},
    [Pid ! Msg || Pid <- Pids],
    {reply, ok, State};

handle_call({register, Pid, Token}, _From, State) when is_pid(Pid) ->
    ets:insert(State#router.token2pid, {Token, Pid}),
    error_logger:info_msg("Router: Registered ~p for ~p~n", [Pid, Token]),
    {reply, ok, State};

handle_call({clear, Token}, _From, State) ->
    ets:delete(State#router.token2pid, Token),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
    
