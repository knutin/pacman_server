Pacman game server
==================

This is a game server for running multiple Pacman games. The server provides
the ghosts, the client must provide Pacman by implementing the protocol
described below.

Once a game is running you may visit http://servername:8080/game/ to view the
game.


Protocol
--------

The server is listening on port 2222. You must connect using a TCP socket.
The protocol is simple text communication over this socket.

Every message must adhere to the following format:

    action JSON-ENCODED-DATA

That is, a keyword with the name of the action you wish to perform, a space
and then the arguments encoded as JSON.

API
---

<table>
        <tr>
        <td>'start'</td>
        <td>Starts a new game. The returned token must be included in all subsequent API requests for this game.</td>
        </tr>

        <tr>
        <td>Arguments</td>
        </tr>

        <tr>
        <td>'email'</td>
        <td>youremail@fqdn.tld</td>
        </tr>
        <tr>
        <td>'map'</td>
        <td>the name of the map ["small", "small_empty", "medium", "real_1", "classic", "world"]</td>
        </tr>
</table>

