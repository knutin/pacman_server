%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-
{application, pacman_server,
 [{description, "pacman_server"},
  {vsn, "0.1"},
  {modules, [game_server,
             game_engine,
             game_worker,
             ghost,
             ghost_blinky,
             ghost_pinky,
             ghost_pinky,
             game_util,
             pacman_sup,
             pacman_app]},
  {registered, [pacman_sup]},
  {applications, [kernel, stdlib]},
  {mod, {pacman_app, []}}
 ]}.
