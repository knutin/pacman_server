%%% @author Knut Ivar Nesheim <knutin@gmail.com>
%%% @copyright (C) 2010, Knut Ivar Nesheim
%%% @doc
%%%
%%% @end

-module(pacman_web).
-author('knutin@gmail.com').

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
        "/game/" ->
            Req:serve_file("index.html", "priv/docroot/");
        "/game/" ++ Token ->
            %% Setup chunked transfer
            Response = Req:ok({"text/event-stream",
                               [{"Server", "Mochiweb-pacman"}],
                               chunked}),

            feed(Response, Token);
        _ ->
            Req:not_found()
    end.


feed(Response, Token) ->
    Response:write_chunk(lists:flatten(io_lib:format("data: ~p~n~n", [Token]))),
    error_logger:info_msg("~p: Wrote chunk~n", [Token]),
    timer:sleep(3000),
    feed(Response, Token).

