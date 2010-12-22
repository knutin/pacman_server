%%%
%%% @doc: Callback module for Ghost AI
%%% BLINKY
%%% http://home.comcast.net/~jpittman2/pacman/pacmandossier.html#Blinky
%%% Blinky will pursue Pacman at all times by targeting his tile.
%%% This means that for every move Pacman makes, Blinky knows what move to make.
%%% There is no need for trying to peek into the future or calculate a strategy.
%%%

-module(ghost_blinky).
-author('knutin@gmail.com').

-include_lib("../include/game.hrl").

-export([move/2, type/0]).

-define(SCATTER_TARGET, {0, 25}).

type() ->
    ?BLINKY.

%% @spec move(State) -> {int, int}
%% @doc Returns Blinkys next move
move(#state{pacman = Pacman} = _GameState,
     #ghost{pos = Pos, graph = G, state = scatter} = _GhostState) ->

    error_logger:info_msg("~p Blinky is scattering..~n", [self()]),

    %% If we have reached the scatter target, set another nearby target
    %% so it appears we are actually scattering, not just jumping between
    %% positions.
    Target = case Pos of
                 ?SCATTER_TARGET -> {3, 20};
                 _ -> ?SCATTER_TARGET
             end,

    game_util:get_path(G, Pos, Target);

move(#state{pacman = Pacman} = _GameState,
     #ghost{pos = Pos, graph = G, state = chase} = _GhostState) ->

    Target = Pacman#player.pos,
    error_logger:info_msg("Blinky is at ~p, target: ~p~n", 
                          [Pos, Target]),

    %% What is the shortest path to target?
    ShortestPath = game_util:get_path(G, Pos, Target),

    error_logger:info_msg("Path to target: ~p~n", [ShortestPath]),

    %% Return the steps until the first corner, where we will make a new
    %% decision once we get there.
    game_util:until_first_corner(ShortestPath).




