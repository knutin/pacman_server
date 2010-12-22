-module(game_util).
-author('knutin@gmail.com').

-include_lib("../include/game.hrl").

-export([get_objects/2,
         set_objects/3,
         append_object/3,
         remove_object/3,
         has_object/3,
         set_pacman/2,
         pos2index/2,
         map2txt/1,
         map2display/1,
         find_objects/2,
         map2digraph/1,
         neighbours/2,
         allowed_position/2,
         make_map/1,
         until_first_corner/1,
         get_path/3
        ]).

%% @doc: Returns the object at given position in the map
get_objects({Array, Size}, Pos) ->
    array:get(pos2index(Pos, Size), Array).

%% @doc: Sets the given position in the map to Objects,
%%       returns the new Map struct.
set_objects({Array, Size}, Pos, Objects) ->
    {array:set(pos2index(Pos, Size), Objects, Array), Size}.

remove_object(Map, Pos, Type) ->
    F = fun(E) -> not (E =:= Type) end,
    NewObjects = lists:filter(F, get_objects(Map, Pos)),
    set_objects(Map, Pos, NewObjects).

append_object(Map, Pos, Type) ->
    OldObjects = get_objects(Map, Pos),
    set_objects(Map, Pos, [Type | OldObjects]).

%% @doc: Returns true if Type can be found in the given position
has_object(Map, Pos, Type) ->
    Objects = get_objects(Map, Pos),
    lists:member(Type, Objects).

%% @doc: Returns a list of all positions where Type can be found
%% TODO: Improve this....
find_objects(Map, Type) ->
    find_objects(Map, Type, map2digraph(Map)).

find_objects(Map, Type, Cells) ->
    F = fun(Pos) ->
                Objects = get_objects(Map, Pos),
                lists:member(Type, Objects)
        end,
    lists:filter(F, Cells).
    
%% @doc: Inserts PacMan at the given position in the map
set_pacman(Map, Pos) ->
    set_objects(Map, Pos, ?PACMAN).

pos2index({X, Y}, Size) ->
    X * Size + Y.

map2txt({Array, _Size}) ->
    F = fun (_, [], Acc) ->
                [integer_to_list(0) | Acc];
            (_I, List, Acc) ->
                [integer_to_list(hd(List)) | Acc]
        end,
    Digits = lists:reverse(array:foldl(F, [], Array)),
    lists:flatten(Digits).

%% @doc: Returns the map formatted for display to the user
map2display({Array, Size}) ->
    Chars = lists:map(fun display_object/1, array:to_list(Array)),
    Display = insert_newline(Chars, Size),
    lists:flatten(io_lib:format("~s", [Display])).

display_object([]) ->
    " ";
display_object([H|_T]) ->
    %% Pick the first item in this slot to display.
    {H, {_Name, Char}} = lists:keyfind(H, 1, ?OBJECT_TYPES),
    Char.

insert_newline(S, Length) ->
    insert_newline(S, Length, []).

insert_newline([], _, Acc) ->
    Acc;
insert_newline(S, Length, Acc) ->
    {H, T} = lists:split(Length, S),
    insert_newline(T, Length, Acc ++ H ++ "\n").

%% @doc: Returns {Level, {Array, Size}}, based on {Level, Size, MapString} for
%%       storage in ets.
make_map({Level, Str, Size}) ->
    L = lists:map(fun get_object_type/1, Str),

    %% Map size be square of size
    case length(L) =:= Size*Size of
        true ->
            {Level, {array:from_list(L), Size}};
        false ->
            throw({map_not_squared, {Level, Str, Size}})
    end.


%% @doc: Returns the object type of Char as a one-element list useful
%%       when creating maps.
get_object_type(Char) ->
    case lists:keyfind(Char, 1, ?SYMBOLS) of
        {Char, []} -> []; % [] represents empty cell in the map
        {Char, Type} -> [Type]
    end.
            

%% UGLY HACK TO GET CELLS WITH COORDINATES
map2digraph({Array, Width}) ->
    map2digraph(Array, Width, 0, 0, []).

map2digraph(_Map, Width, X, _, Acc) when X >= Width ->
    lists:reverse(Acc);

map2digraph(Map, Width, X, Y, Acc) when Y >= Width ->
    map2digraph(Map, Width, X+1, 0, Acc);

map2digraph(Map, Width, X, Y, Acc) ->
    map2digraph(Map, Width, X, Y+1, [{X, Y} | Acc]).


%% @doc: Returns a list of coordinates to the neighbours of given coordinates
neighbours(Map, {X, Y}) ->
    %% Up, right, down, left
    N = [{X-1, Y},
         {X, Y+1},
         {X+1, Y},
         {X, Y-1}],
    
    F = fun(P) -> allowed_position(Map, P) end,
    lists:filter(F, N).



%% @doc: Returns true if pacman is allowed to occupy the given position
allowed_position({Array, Size}, {X, Y})
  when X < Size andalso Y < Size andalso X >= 0 andalso Y >= 0 ->
    %% Check that all objects at the new location are allowed
    Objects = game_util:get_objects({Array, Size}, {X, Y}),

    F = fun(O) -> lists:member(O, ?ALLOWED_OBJECTS) end,
    lists:all(F, Objects);
allowed_position(_, _) ->
    false.


%% @doc: Returns the steps required to get to the first corner where a turn
%%       must be made.
until_first_corner([H|T]) ->
    lists:reverse(until_first_corner(T, [H])).

until_first_corner([], Acc) ->
    Acc;
until_first_corner([{X, Y} | T], Acc) ->
    %% Very inefficient, but it works correctly.
    Xs = lists:map(fun({A, _}) -> A end, Acc),
    Ys = lists:map(fun({_, B}) -> B end, Acc),
    SumX = lists:sum(Xs),
    SumY = lists:sum(Ys),
    
    case {length(Xs) * X, length(Ys) * Y} of
        {SumX, _} ->
            until_first_corner(T, [{X, Y} | Acc]);
        {_, SumY} ->
            until_first_corner(T, [{X, Y} | Acc]);
        {_, _} ->
            Acc
    end.


get_path(G, Start, End) ->
    case digraph:get_short_path(G, Start, End) of
        %% Remove the first step as it always is the starting position.
        [_ | P] -> P;
        false -> throw({no_path, Start, End, G})
    end.
