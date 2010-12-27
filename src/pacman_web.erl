%%% @author Knut Ivar Nesheim <knutin@gmail.com>
%%% @copyright (C) 2010, Knut Ivar Nesheim
%%% @doc
%%%
%%% @end

-module(pacman_web).
-author('knutin@gmail.com').

-include_lib("../include/game.hrl").

-export([start/1, stop/0, loop/1]).

start(Options) ->
    Loop = fun(Req) ->
                   ?MODULE:loop(Req)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req) ->
    Path = Req:get(path),
    error_logger:info_msg("Web: Path: ~p~n", [Path]),
    case Path of
        %% Returns the game viewer
        "/game/" ->
            Req:serve_file("index.html", "priv/docroot/");
        %% Returns a list of active game tokens as a JSON array
        "/games/" ->
            GameTokens = [T || {T, _Pid} <- ets:tab2list(?GAMES_TABLE)],
            Req:ok({"text/javascript", mochijson:encode({array, GameTokens})});
        %% Event-stream of moves in a game
        "/game/" ++ Token ->
            %% Register this client with the game token
            router:register(self(), Token),

            %% Setup chunked transfer
            Response = Req:ok({"text/event-stream",
                               [{"Server", "Mochiweb-pacman"}],
                               chunked}),

            feed(Response, Token);
        _ ->
            Req:not_found()
    end.


feed(Response, Token) ->
    receive
        {state_update, State} ->
            error_logger:info_msg("~s: Sending new state~n", [Token]),
            Msg = io_lib:format("data: ~p ~s~n~n", [element(2, State#state.map),
                                                    game_util:map2txt(State#state.map)]),
            Response:write_chunk(Msg)
    %%after 10000 ->
    %%        Response:write_chunk(io_lib:format("data: ~p~n~n", [Token]))
    %%        %%error_logger:info_msg("~p: Wrote chunk~n", [Token])
    end,

    feed(Response, Token).

