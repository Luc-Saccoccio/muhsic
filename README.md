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

You can use haddock to generate the module documentation
```
stack haddock
```

## Examples

There's some examples in `examples/`, try it ;)

## File Format (To rewrite)

It's not intuitive nor pratical ! *Please end my suffering*

It's not even good for notes played at the same times but with different durations ! *Please I beg you*

For you my friend, who still wants to try to write music with it *It doesn't even sound good*, here's the format :
```
# This is a comment
note | another_note duration
```
Note are coded with integer, and the silence is coded with 0.5. Refer to [this paper](https://pages.mtu.edu/~suits/NoteFreqCalcs.html) to understand the integer you have to use. In short, it's the following : the pitch standard (La 440Hz for a frenchie like me, A440 else) is reprensented by a 0. Then, the notation is pretty simple to understand (but hard to use !) : the following notes are given the integer that follows (be aware that it works with half steps), same for previous notes with negative integers.

## Output format

The output is named `output.bin`, you can read it with `ffplay -f f32le -ar 48000 output.bin`. If you want to export it to another format, you can use `ffmpeg` :
```
ffmpeg -ar 48000 -f f32le -i output.bin -ar 48000 -codec copy -f wav out.wav
```

# What's next ?
- Rework the option parsing, it's minimalistic, but hard to expand. Using [optparse-applicative](https://github.com/pcapriotti/optparse-applicative) ?
- Rework entirely the ADSR part
- Think of optimization, and make the code more workable
  * Separate the source into multiple files
  * Easy to maintain should be the goal

# Credits

Thanks a lot to [tsoding](https://github.com/tsoding/) for the original [haskell-music](https://github.com/tsoding/haskell-music)
Thanks to [Lyric Wulf](https://www.youtube.com/channel/UCOvUbpPE2ev8pXcmadB6ryw) for some of the partitions I used as examples (and also for transcribing those pieces)
