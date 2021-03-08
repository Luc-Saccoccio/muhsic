# Haskell-music

Using the base from [tsoding's program](https://github.com/tsoding/haskell-music), I used this very interesting base to exercise myself on Haskell.

You need to provide a file containing the notes and durations (see `examples/`) as an argument.

Requires `ffplay`

## Examples

Either compile the program and run it on examples:
```shell
ghc -dynamic Main.hs -o haskell-music
haskell-music examples/1.txt
```

Or run it with `runhaskell`:
```shell
runhaskell Main.hs examples/1.txt
```

# What's next ?
- Implementing multiple notes I guess.
- Think of a new way of representing partitions.
  * Idea 1 : `duration | note duration | note duration`
  	Where first `duration` is the time before adding the waves of the next line.
  * Idea 1 : Each line represent an absolute time code (0.125 by 0.125 ?). Problem of the continuity of a note. New symbol ?
