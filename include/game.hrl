%% Object types

-define(MAP_TABLE, maps).

-define(PACMAN , 1).
-define(PACDOT , 2).
-define(FRUIT  , 3).
-define(WALL   , 4).
-define(BLINKY , 5).
-define(PINKY  , 6).
-define(INKY   , 7).
-define(CLYDE  , 8).

-define(OBJECT_TYPES, [{?PACMAN, {"Pacman", "<"}},
                       {?PACDOT, {"Pacdot", "*"}},
                       {?FRUIT , {"Fruit" , "O"}},
                       {?WALL  , {"Wall", "|"}},
                       {?BLINKY, {"Blinky", "B"}},
                       {?PINKY , {"Pinky", "P"}},
                       {?INKY  , {"Inky", "I"}},
                       {?CLYDE , {"Clyde", "C"}}]).

-define(SYMBOLS, [{$<, ?PACMAN},
                  {$*, ?PACDOT},
                  {$B, ?BLINKY},
                  {$P, ?PINKY},
                  {$I, ?INKY},
                  {$C, ?CLYDE},
                  {$., []}, % Empty stored as "." instead of " "
                  {$|, ?WALL}
                  ]).


%% A #player record represents a player in the game,
%% either pacman or any of the ghosts.
-record(player, {
          type = ?PACMAN,
          pos = {0, 0}, % Location on map
          direction % Direction player is currently moving. Used for AI
         }).


%% Game state
-record(state, {
          email,
          sock, %% Client socket
          pid, %% Pid of the server process where the game is running
          score = 0,
          map, %% {Size, Map::array()}
          pacman = #player{},
          ghosts = [],
          num_moves = 0
         }).


-record(ghost, {
          type,      % ?BLINKY, ?PINKY, etc
          module,    % the callback module containing the AI logic
          pos,       % the current position
          state,     % scatter, chase
          graph,     % a cached digraph reference
          direction, % the direction the ghost is currently moving in(not used)
          queue = [] % a queue of moves to perform
          }).


%% PacMan is allowed to walk into positions that contain the following objects
-define(ALLOWED_OBJECTS, [?PACMAN,
                          ?PACDOT,
                          ?FRUIT,
                          ?BLINKY,
                          ?PINKY,
                          ?INKY,
                          ?CLYDE]).

%% Objects Pacman is allowed to eat
-define(EATABLE_OBJECTS, [?PACDOT, ?FRUIT]).

-define(GHOSTS, [?BLINKY, ?PINKY, ?INKY, ?CLYDE]).

-define(POINTS, [{?PACDOT, 1},
                 {?FRUIT, 10}]).

%% For every allowed command, the game_worker may call
%% game_engine:handle(Command, ...)
-define(ALLOWED_COMMANDS, [start, state, move]).
