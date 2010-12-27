-module(game_engine).
-author('knutin@gmail.com').

-include_lib("../include/game.hrl").

%% Main entry point from game_worker
-export([handle/3]).

-export([load_maps/0]).

%% For debugging
-export([start/2, state/2, move/2, move_ghosts/1, collision/2]).
-export([eat/2]).


%% @doc: handle(Action, Args, State) is called when a client sends a message.
%%       This is the main entry point for game_worker.
%%       Return value is {ok, NewState, ResponseToClient}

handle(start, Args, State) ->
    start(State, Args);

handle(move, Args, State) ->
    try move(State, Args) of
        {error, NewState, Error} -> {ok, NewState, Error};
        {ok, NewState, Result} -> {ok, NewState, Result}
    catch
        throw:Why ->
            error_logger:error_msg("~p crashed: ~p~n", [self(), Why]),
            {ok, State, [{error, crash}]}
    end;

handle(state, Args, State) ->
    state(State, Args);

handle(_Action, _Args, State) ->
    {ok, State, [{error, no_matching_handler}]}.

%% @doc: Starts a new game, returns a unique token for identifying this
%%       game and the map. State is already initialized with the client socket.
%% @end
start(State, Args) ->
    Email = proplists:get_value("email", Args),
    _NumGhosts = proplists:get_value("ghosts", Args),
    Map = get_map(proplists:get_value("map", Args)),
    {_Array, Size} = Map,

    {ok, Blinky} = ghost:start_link(Map, ghost_blinky),
    {ok, Pinky}  = ghost:start_link(Map, ghost_pinky),
    {ok, Inky}   = ghost:start_link(Map, ghost_inky),
    Ghosts = [Blinky, Pinky, Inky],

    NewState = State#state{email = Email,
                           map = Map,
                           ghosts = Ghosts},

    Result = [{"token", NewState#state.token},
              {"map", game_util:map2txt(NewState#state.map)},
              {"mapwidth", Size}
             ],

    {ok, NewState, Result}.

%% @doc: Returns the game state, such as current score, the map, etc.
state(State, _Args) ->
    {ok, State, [{mapstr, game_util:map2display(State#state.map)},
                 {map, game_util:map2txt(State#state.map)}]}.


%% @doc: Move, called when the client wishes to move Pacman
%%  Overview:
%%   1. Move Pacman in the direction specified by the client
%%   2. Move ghosts according to this new position
%%   3. Detect collisions
move(S, Args) ->
    Direction = get_direction(Args),
    
    maybe_change_ghost_mode(S),

    {ok, S1} = move_pacman(S, Direction),
    {ok, S2} = move_ghosts(S1),

    %% Is there a collision?
    case collision(S2#state.map, (S2#state.pacman)#player.pos) of
        false ->
            {ok, S2, [{state, ok}, {map, game_util:map2txt(S2#state.map)}]};
        true ->
            {error, S2, [{state, game_over}]}
    end.

maybe_change_ghost_mode(#state{ghosts = Ghosts, num_moves = M} = State) ->
    case M rem 30 of
        0 ->
            lists:map(fun ghost:change_mode/1, Ghosts);
        _ -> ok
    end.

new_pos({X, Y}, Direction) ->
    case Direction of
        left  -> {X, Y-1};
        up    -> {X-1, Y};
        right -> {X, Y+1};
        down  -> {X+1, Y}
    end.

%% @doc: Moves pacman in given direction
move_pacman(#state{pacman = Pacman, map = Map} = State, Direction) ->
    %% Calculate new position. This allows any move, even into walls.
    #player{pos = OldPos} = Pacman,
    NewPos = new_pos(OldPos, Direction),
    error_logger:info_msg("~p Pacmans new position ~p~n", [self(), NewPos]),

    %% Remove Pacman from old position
    Map1 = game_util:remove_object(Map, OldPos, ?PACMAN),

    %% Move Pacman into the new position
    Map2 = game_util:append_object(Map1, NewPos, ?PACMAN),

    %% Eat pacdots and fruits in the new position
    Map3 = eat(Map2, NewPos),

    %% Update Pacman player
    NewPacman = Pacman#player{pos = NewPos, direction = Direction},

    %% Update number of moves
    Moves = State#state.num_moves + 1,

    {ok, State#state{map = Map3, pacman = NewPacman, num_moves = Moves}}.

%% @doc: Moves ghosts according to their AI logic
move_ghosts(#state{ghosts = Ghosts} = State) ->
    error_logger:info_msg("~p Moving ghosts..~n", [self()]),

    MoveF = fun(Ghost, S) -> ghost:move(Ghost, S) end,
    NewState = lists:foldl(MoveF, State, Ghosts),

    {ok, NewState}.


%% @doc: Detects collisions between Pacman and object that he
%%       gets killed if he collides with.
collision(Map, Pos) ->
    %% Find the objects in Pacmans position
    Objects = game_util:get_objects(Map, Pos),
    
    %% If all objects are allowed, there is no collision
    F = fun (O) -> lists:member(O, ?GHOSTS) end,

    lists:any(F, Objects).

%% @doc: Eats pacdots or fruit in given position,
%%       returns the new Map.
eat(Map, Pos) ->
    Objects = game_util:get_objects(Map, Pos),
    %% Remove any eatable objects
    NewObjects =
        lists:filter(fun(E) -> not lists:member(E, ?EATABLE_OBJECTS) end,
                     Objects),

    game_util:set_objects(Map, Pos, NewObjects).

load_maps() ->
    load_maps("priv/maps.txt").

load_maps(Filepath) ->
    {ok, MapData} = file:consult(Filepath),
    Maps = lists:map(fun game_util:make_map/1, MapData),
    error_logger:info_msg("Maps: ~p~n", [Maps]),
    lists:map(fun (M) -> ets:insert(?MAP_TABLE, M) end, Maps),
    error_logger:info_msg("Loaded ~p maps.~n", [length(Maps)]),
    ok.


%%
%% INTERNAL HELPER FUNCTIONS
%%
get_direction(Args) ->
    Direction = proplists:get_value("direction", Args),
    
    case lists:member(Direction, ["up", "down", "right", "left"]) of
        true -> list_to_atom(Direction);
        false -> throw(invalid_direction)
    end.

%% @doc: Returns the Map::{Array, Size} for given level
get_map(Level) ->
    case ets:lookup(maps, Level) of
        [{Level, Map}] -> Map;
        [] -> throw(invalid_map_name)
    end.



