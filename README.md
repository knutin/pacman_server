Pacman game server
==================

This is a game server for running multiple Pacman games. The server provides
the ghosts, the client must provide Pacman by implementing the protocol
described below.

Once a game is running you may visit http://servername:8080/game/ to view the
game.

Please see ["The Pac-Man dossier"](http://home.comcast.net/~jpittman2/pacman/pacmandossier.html)
by Jamey Pittman for a detailed description of the game. 


Protocol
--------

The server is listening on port 2222. You must connect using a TCP socket.
The protocol is simple text communication over this socket.

Every message must adhere to the following format:

    action JSON-ENCODED-DATA

That is, a keyword with the name of the action you wish to perform, a space
and then the arguments encoded as JSON. You should only send one command and
then wait for a reply. If your program is not well behaved, the server will
crash and your game state will be lost.

API
---

The API has the following actions:

 * 'start'
 * 'move'
 * 'wingame' - not implemented yet


### 'start' ###

'start' initializes a new game. The returned token must be included in all
subsequent API requests for this game.

Arguments:

 * 'email' - your full email
 * 'map' - name of the map you wish to play, ["small", "small_empty", "medium", "real_1", "classic", "world"]

Returns:

 * 'token' - unique identifier for this game. May be used when watching the
   game.
 * 'map' - map as string, see MAP below
 * 'mapwidth' - the map is a square and this tells you how many objects
   there are in each row

### 'move' ###

Calling 'move' will move Pacman in the specified direction. After Pacman has
moved, the ghosts are allowed to make their move.

Arguments:

 * 'token'
 * 'direction' - ["up", "right", "down", "left"]

Returns:

 * 'map' - the new map after Pacman and the ghosts has moved
 * 'state' - "ok" if you are still alive, "game_over" if you are dead,
  "game_won" (not implemented yet) if you have won the game


MAP AND OBJECT TYPES
--------------------

The map is serialized to a string made up of digits. Your client must
implement it's own map data structure. The map is always a square and you
will receive width of the map.

Note: Two ghosts are allowed to occupy the same space, but in the serialized
map only one ghost will be included if two or more overlap.

The following objects may be found on the map:
 * 0, the tile is empty
 * 1, Pacman
 * 2, pacdot
 * 3, fruit
 * 4, wall
 * 5, Blinky
 * 6, Pinky
 * 7, Inky
 * 8, Clyde

ABOUT THE GHOSTS AI
-------------------

The AI of the ghosts tries to follow the original AI as far as it is
convenient to implement. The only thing you will know for sure is that
the behaviour is completely deterministic.


