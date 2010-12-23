-module(game_worker).
-author('knutin@gmail.com').
-include_lib("../include/game.hrl").

-behaviour(gen_server).

-export([start/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


%% @doc: Starts a new game, returns a unique token for identifying this
%%       game and the map.
%% @end
start(Sock) ->
    {ok, Pid} = gen_server:start(?MODULE, [Sock], []),
    register(gw, Pid),

    gen_tcp:controlling_process(Sock, Pid),
    inet:setopts(Sock, [{active, once}]),

    error_logger:info_msg("~p Started game worker~n", [Pid]),
    {ok, Pid}.

%% @doc: Initialize new game state
init([Sock]) ->
    State = #state{sock = Sock, pid = self()},
    {ok, State}.


handle_call(state, _From, State) ->
    {reply, State, State};

handle_call(_Request, _From, State) ->
    {noreply, State}.
handle_cast(_Msg, State) -> {noreply, State}.


%% @doc: Receive data from client, call the corresponding game_engine commands
%%       The method in game_engine must return {ok, NewState, ResultToUser}
handle_info({tcp, _Port, Bs}, State) ->
    {Command, Args} = parse_data(binary_to_list(Bs)),

    %% Validate the command
    case lists:member(Command, ?ALLOWED_COMMANDS) of
        true -> true;
        false -> throw(illegal_command)
    end,

    {ok, NewState, Result} = game_engine:handle(Command, Args, State),

    %% Notify any connected listeners
    %% TODO: Validate token
    Token = proplists:get_value("token", Args),
    router:state_update(Token, NewState),

    error_logger:info_msg("Result: ~p~n", [Result]),

    ResultData = mochijson:encode({struct, Result}),
    gen_tcp:send(State#state.sock, list_to_binary(ResultData)),
    inet:setopts(State#state.sock, [{active, once}]),

    {noreply, NewState};

handle_info(Info, State) ->
    error_logger:info_msg("~p Received info: ~p~n", [self(), Info]),
    {noreply, State}.
    

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
    
%% @doc: Extracts the first word and JSON data from Data
parse_data(Data) ->
    [Command|ArgsString] = string:tokens(Data, " "),
    {struct, Args} = mochijson:decode(ArgsString),
    {list_to_atom(Command), Args}.


