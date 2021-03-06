# Cursed Threads
a game by Eric Stansifer

Hosted at https://github.com/estansifer/a-roguelike-demo/

## About Cursed Threads

This is a roguelike game, one of a family of games based on the 1980 game Rogue.
In this game, you descend through a series of randomly generated dungeon levels
filled with successively more dangerous monsters.

This game was designed to demonstrate a particular idea I had, and was aimed at
players already familiar with the roguelike genre. There is no way to win, and
there are no new monsters after level 10 or so.

The size of the dungeon is based on the size of the terminal window it is
played in, so use a smaller window if you want a quicker, harder game.

The slash `/`, by the way, represents the strength of your weapon and so how
much damage you do.

## How to play the game

To install this game you require GHC (Glasgow Haskell Compiler) and the
hscurses library. The code was tested on GHC 7.6.3. If you do not have
hscurses, you can use the command `cabal install hscurses` to install it.

To install the game, clone this git repository with

    git clone git@github.com:estansifer/a-roguelike-demo.git

and compile with

    ./make.sh

Run the game with

    ./CursedThreads

To move your character, use roguelike controls (`hjklyubn`) or
numpad (`12346789`).

Additional controls are
 * `5` or `.` to wait in place
 * `d` to drink a potion
 * `r` to read a scroll
 * `>` to go down a level
 * `R` to refresh the screen
 * `Q` to quit
