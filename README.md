# Muhsic

Using the base from [tsoding's program](https://github.com/tsoding/haskell-music), I used this very interesting base to exercise myself on Haskell.

You need to provide a file containing the notes and durations (see `examples/`) as an argument.

Requires `ffplay`. All Haskell dependencies are managed by Stack.

## Table of contents

* [Installation](#Installation)
* [Examples](#examples)
* [File Format](#file-format)
* [Output Format](#output-format)
* [Arguments](#arguments)
* [What's next ?](#next)
* [Credits](#credits)

<a name="installation"></a>

## Installation

Clone the repository then install the package with either stack or cabal (as you want)
```
git clone https://github.com/Luc-Saccoccio/muhsic.git
cd muhsic
stack install
```

<a name="examples"></a>

## Examples

There's some examples in `examples/`, with the partition so you can compare.

<a name="file-format"></a>

## File Format (To rewrite)

The file is structured by mesure and then by hand. You can add as many hand as you want, and have different durations. In fact, it's better to avoid adding an empty hand or not complete one by silence. However, all the hand start at the same time. If it starts by silence and then have notes, play the silence. A "hand" is located in a "mesure".

- `===` is the separation between two mesures. You don't need to end or start a file with this.
- `---` is the separation between two hands. You don't need to end or start a hand with this.
- `|` is the separation between two notes.
- After the list of notes (must be on the same line), the duration follows.
- A new line means that the line will be played **after** the previous line.

The notes are represented by integer, counting from La3/A4. Refer to `doc/piano_range.pdf` for a diagram. A silence is represented by `0.5`.

Here is the result, where `a`, `b`, `c` and `d` represent notes. Let's say we want to play `a+c` for 0.5, then `b+c` for 1, and, at the same time, play `a+d+c` for 0.25, a silence for 0.5 and finish by `c+b` for 0.5 (0.25 is missing, this is not a problem).

```
a | c 0.5
b | c 1
---
a | d | c 0.25
0.5 0.5
c | b 0.5
===
```

We can then write another mesure after, because we separated both blocks by `===`.

<a name="output-format"></a>

## Output format

The output is by default named `output.bin`, you can read it with `ffplay -f f32le -ar 48000 output.bin`. If you want to export it to another format, you can use `ffmpeg` :
```
ffmpeg -ar 48000 -f f32le -i output.bin -ar 48000 -codec copy -f wav out.wav
```

<a name="arguments"></a>

## Arguments

On argument is necessary : `-i`/`--input` to give the input file.

Here are the optional arguments:
- `-b`/`--bpm`: Specify beats per minute i.e. the duration of a black note. Default 120.
- `-v`/`--volume`: Specify output volume. Default 0.2.
- `-o/--output`: Specify output name. I recommend using `.bin` extension. Not conversion will be done inside the program, use `ffmpeg` for that. Default `output.bin`

<a name="next"></a>

# What's next ?
- Rework entirely the ADSR part.
- Implement new types of synthetization.
- Finish the writing of the man page.
- Adding more "examples", finish Megalovania.

<a name="credits"></a>

# Credits

- Thanks a lot to [tsoding](https://github.com/tsoding/) for the original [haskell-music](https://github.com/tsoding/haskell-music).
- Thanks to [this thread](https://tex.stackexchange.com/a/448153) on StackOverflow for providing the code used to produce the partition diagram.
