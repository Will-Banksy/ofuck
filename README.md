# ofuck

Brainfuck interpreter written in OCaml, as a first delve into actual functional programming. It is not a very efficient or high performance implementation but that wasn't an aim of this project, it works* and that is what matters.

Most of the interesting code is in [lib/brainfuck.ml](lib/brainfuck.ml), with two useful list methods in [lib/listExt.ml](lib/listExt.ml) and the entry point, driving & I/O code in [bin/main.ml](bin/main.ml).

A "hello world" program in brainfuck is provided in [hello_world.bf](hello_world.bf).

*for valid brainfuck programs, as far as manually tested

## Details

Brainfuck programs are constructed from the following one-character commands: `><+-[].,`. This section gives a description of each along with any implementation details.

`>` - Forward

Moves the data pointer forwards. Extends the working tape infinitely (until, presumably, an error occurs, but I haven't tested that).

`<` - Backwards

Moves the data pointer backwards. When the data pointer is 0, '<' will not change it.

`+` - Increment

Increments the value at the data pointer, wrapping around at 256 to 0.

`-` - Decrement

Decrements the value at the data pointer, wrapping around at -1 to 255.

`[` - Jump forwards if zero

Jumps the instruction pointer forwards to 1 after the matching `]` if the value at the data pointer is 0

`]` - Jump backwards if not zero

Jumps the instruction pointer backwards to 1 after the matching `[` if the value at the data pointer is not 0

`.` - Output

Writes the value at the data pointer to stdout as a character

`,` - Input

Reads a character value from stdin. A bit unfortunately, characters are read a line at a time

## Running

I'm just using the dune build system, so it's as simple as:

```bash
dune exec ofuck [filename]
```

This will run the program, interpreting the brainfuck code read from `[filename]`, if it exists. Omitting the filename will take input from stdin.