%%%
%%% @doc: Callback module for Ghost AI
%%% PINKY
%%% http://home.comcast.net/~jpittman2/pacman/pacmandossier.html#Pinky
%%% Pinky will target a tile four tiles in front of Pacman, in the direction
%%% Pacman is currently moving.

-module(ghost_inky).
-author('knutin@gmail.com').

-include_lib("../include/game.hrl").

%% API
-export([move/2, type/0]).

%% Debugging
-export([target/3]).

-define(SCATTER_TARGET, {25, 25}).

type() ->
    ?INKY.

%% @spec move(State) -> term()
%% @doc Returns the {X, Y} coordinates of the next move
move(_GameState,
     #ghost{pos = Pos, graph = G, state = scatter} = _GhostState) ->

    Target = case Pos of
                 ?SCATTER_TARGET -> {20, 19};
                 _ -> ?SCATTER_TARGET
             end,
    game_util:get_path(G, Pos, Target);

move(#state{pacman = Pacman, map = Map} = _GameState,
     #ghost{pos = Pos, graph = G} = _GhostState) ->

    Target = target(Map, Pacman#player.direction, Pacman#player.pos),
                 
    error_logger:info_msg("Inky is at ~p, target: ~p~n", 
                          [Pos, Target]),

    ShortestPath =
        case digraph:get_short_path(G, Pos, Target) of
            [_ | P] -> P; % Remove the first step as it always is the
                          % current position
            false -> throw({no_path, Pos, Target, G})
        end,

    error_logger:info_msg("Path to target: ~p~n", [ShortestPath]),
    %% TODO: If there are two or more equally good paths, pick one randomly
    game_util:until_first_corner(ShortestPath).


%% @doc: Returns the target tile.
target(Map, Direction, Pos) ->
    target(Map, Direction, Pos, 2).


target(_Map, _Direction, Target, 0) ->
    Target;
target(Map, Direction, Target, N) ->
    NewTarget = direction2tile(Direction, Target),
    
    case game_util:allowed_position(Map, NewTarget) of
        true ->
            target(Map, Direction, NewTarget, N-1);
        false ->
            Target
    end.

direction2tile(Direction, {X, Y}) ->
    case Direction of
        left  -> {X, Y-1};
        up    -> {X-1, Y};
        right -> {X, Y+1};
        down  -> {X+1, Y}
    end.
