# Cursed Threads
a game by Eric Stansifer

Hosted at `https://github.com/estansifer/a-roguelike-demo/`



Tested on GHC 7.6.3, and requires hscurses. 
To install, clone this git repository with
    git clone git@github.com:estansifer/a-roguelike-demo.git
install hscurses with
    cabal install hscurses
and compile with
    ./make.sh Roguelike

Run with
    ./run.sh
or
    ./bin/Roguelike

Don't read any of the source code or anything in the doc directory
before playing the first time, or it'll give away important details.

The game is designed to be played by someone who has played
a roguelike before.

The game doesn't have any particular end condition, but there is
no new material after level 10 or so.

Here is the source code for processing input, it should give
enough information to get started:

```haskell
char_to_command c = case c of
  'h' -> Just $ Move (-1,  0)
  'j' -> Just $ Move ( 0,  1)
  'k' -> Just $ Move ( 0, -1)
  'l' -> Just $ Move ( 1,  0)
  'y' -> Just $ Move (-1, -1)
  'u' -> Just $ Move ( 1, -1)
  'b' -> Just $ Move (-1,  1)
  'n' -> Just $ Move ( 1,  1)
  '.' -> Just $ Move ( 0,  0)

  '4' -> Just $ Move (-1,  0)
  '2' -> Just $ Move ( 0,  1)
  '6' -> Just $ Move ( 0, -1)
  '8' -> Just $ Move ( 1,  0)
  '7' -> Just $ Move (-1, -1)
  '9' -> Just $ Move ( 1, -1)
  '1' -> Just $ Move (-1,  1)
  '3' -> Just $ Move ( 1,  1)
  '5' -> Just $ Move ( 0,  0)

  'd' -> Just Drink
  'r' -> Just Read
  '>' -> Just Down

  'R' -> Just RefreshScreen
  '/' -> Just TypeMessage

  'Q' -> Just Quit

  _ -> Nothing
```

If you press / to type a message, press / again to resume.
