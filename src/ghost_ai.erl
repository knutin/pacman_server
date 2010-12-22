-module(ghost_ai).
-author('knutin@gmail.com').

-include_lib("../include/game.hrl").

-export([blinky_move/1]).

%% BLINKY
%% http://home.comcast.net/~jpittman2/pacman/pacmandossier.html#Blinky
%% Blinky will pursue Pacman at all times by targeting his tile.
%% This means that for every move Pacman makes, Blinky knows what move to make.
%% There is no need for trying to peek into the future or calculate a strategy.


%% @doc: Takes the map, creates a digraph and returns its reference.
map2graph(Map) ->
    G = digraph:new(),

    %% Add all vertices to the graph
    Vertices = game_util:map2digraph(Map),
    [digraph:add_vertex(G, V, game_util:get_objects(Map, V)) || V <- Vertices],

    %% For every vertice, add edges to surrounding vertices where Pacman
    %% is allowed to move.
    F = fun(V) ->
                Neighbours = game_util:neighbours(Map, V),
                %%error_logger:info_msg("Neighbours: ~p~n", [Neighbours]),
                [digraph:add_edge(G, V, N) || N <- Neighbours]
        end,
    lists:map(F, Vertices),
    G.

%% @doc: Returns the neighbours of Pos as digraph edges
neighbours(G, Pos) ->
    Edges = digraph:out_edges(G, Pos),
    [digraph:edge(G, E) || E <- Edges].

%% @spec blinky_move(State) -> term()
%% @doc Returns Blinkys next move
blinky_move(#state{map = Map, pacman = Pacman} = State) ->
    %% General idea: Which of the neighbouring cells will take Blinky
    %% closer to Pacman?
    Blinky = hd(State#state.ghosts),
    Pos = Blinky#player.pos,
    Target = Pacman#player.pos,
    error_logger:info_msg("Blinky is at ~p, target: ~p~n", 
                          [Pos, Target]),

    G = map2graph(Map),

    %% What is the shortest path to target?
    [_ | ShortestPath] = digraph:get_short_path(G, Pos, Target),
    %% Remove the first step as it always is the current position

    error_logger:info_msg("Path to target: ~p~n", [ShortestPath]),

    %% Take the first step towards the target
    BestBet = hd(ShortestPath),

    digraph:delete(G),
    %%error_logger:info_msg("G: ~p~n", [G]),

    BestBet.
