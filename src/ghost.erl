%%%
%%% @doc Ghost server
%%%


-module(ghost).
-author('knutin@gmail.com').

-include_lib("../include/game.hrl").

-behaviour(gen_server).

-export([move/2,
         change_mode/1,
         scatter/1,
         chase/1]).

-export([start_link/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% API

%% @doc: Returns the next move for given ghost process.
move(Pid, State) ->
    gen_server:call(Pid, {move, State}).

change_mode(Pid) ->
    gen_server:cast(Pid, change_mode).

scatter(Pid) ->
    gen_server:call(Pid, scatter).

chase(Pid) ->
    gen_server:call(Pid, chase).

start_link(Map, Mod) ->
    gen_server:start_link(?MODULE, [Map, Mod], []).

init([Map, Mod]) ->
    Type = Mod:type(),
    [Pos] = game_util:find_objects(Map, Type),
    {ok, #ghost{type = Type,
                module = Mod,
                state = scatter,
                pos = Pos,
                graph = map2graph(Map),
                direction = undefined}}.

handle_call({move, #state{map = Map} = GameState}, _From,
            #ghost{module = Mod, type = Type} = State) ->
    error_logger:info_msg("~p: Moving! ~n", [self()]),

    OldPos = State#ghost.pos,

    [NewPos|Queue] = get_queue(Mod, GameState, State),
    error_logger:info_msg("~p: Moved to ~p~n", [self(), NewPos]),

    Map1 = game_util:remove_object(Map, OldPos, Type),
    Map2 = game_util:append_object(Map1, NewPos, Type),

    {reply,
     GameState#state{map = Map2}, 
     State#ghost{pos = NewPos, queue = Queue}}.

handle_cast(change_mode, State) ->
    NewMode = case State#ghost.state of
                  scatter -> chase;
                  chase -> scatter
              end,
    {noreply, State#ghost{state = NewMode}};    

handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%% @doc: Returns the queue of moves. If the queue is empty, the ghost process
%%       is asked to come up with a list of moves.
get_queue(Mod, GameState, #ghost{queue = []} = State) ->
    Mod:move(GameState, State);
get_queue(_Mod, _GameState, #ghost{queue = Queue} = _State) ->
    Queue.

%%
%% INTERNAL HELPER FUNCTIONS
%%

%% @doc: Takes the map, creates a digraph and returns its reference.
map2graph(Map) ->
    G = digraph:new(),

    %% Add all vertices to the graph
    Vertices = game_util:map2digraph(Map),
    [digraph:add_vertex(G, V, game_util:get_objects(Map, V)) || V <- Vertices],

    %% For every vertice, add edges to surrounding vertices that isn't a wall
    F = fun(V) ->
                Neighbours = game_util:neighbours(Map, V),
                [digraph:add_edge(G, V, N) || N <- Neighbours]
        end,
    lists:map(F, Vertices),
    G.


