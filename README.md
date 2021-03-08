# Haskell-music

Using the base from [tsoding's program](https://github.com/tsoding/haskell-music), I used this very interesting base to exercise myself on Haskell.

You need to provide a file containing the notes and durations (see `examples/`) as an argument.

Requires `ffplay`

## Installation

Clone the repository then install the package with either stack or cabal (as you want)
```
git clone https://github.com/Luc-Saccoccio/muhsic.git
cd muhsic
stack install
```

## Examples

There's some examples in `examples/`, try it ;)

# What's next ?
- Implementing multiple notes I guess.
- Think of a new way of representing partitions.
  * Idea 1 : `duration | note duration | note duration`
  	Where first `duration` is the time before adding the waves of the next line.
  * Idea 1 : Each line represent an absolute time code (0.125 by 0.125 ?). Problem of the continuity of a note. New symbol ?
