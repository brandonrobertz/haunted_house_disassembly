# Haunted House Commented Disassembly

Reverse-engineering and disassembly of the Atari 2600 game Haunted House
designed by James Andreasen, the _original_ survival horror game. This is a
work in progress. The disassembled and commented code is in `haunted.asm`. I
would appreciate any tips or advice anyone may have as a PR or Issue.

![Haunted House](https://github.com/brandonrobertz/haunted_house_disassembly/blob/master/atari_catalog_haunted_house.jpg)

## Building/Playing the ROM

I'm using [DASM](https://github.com/dasm-assembler/dasm). You can build the ROM
by running `make` and you'll get a ROM, `haunted.bin`. This can be played using
the [Stella](https://stella-emu.github.io/) 2600 emulator.

If you don't have make, the correct DASM command to build the ROM is:

```
dasm haunted.asm -ohaunted.bin -f3
```



## Sprites

The ghost of old man Zachary Graves:


    █ ███
    ██ █ █
     ██████
     █████ █
     █████
      ████
        ██


The urn you need to win the game and the pieces which make it:

      ████        ████
       ██          ██
      ████      █  ██   █
    ████████  ████  █   ███
    █ ████ █  █ █  ██   ██
    ████████  ███  █   ████
      ████        ████
       ██          ██


The bats flying around:


    █      █   ████
    █      █  ██████
    ██    ██  ██  ██
     ██  ██  ██    ██
     ██████  █      █
      ████   █      █

Some spiders (swaps between these two when moving):


      █  █
      █  █    ███  ███
      █  █       ██
    █  ██  █   ██████
     ██████   █  ██  █
       ██       █  █
     ██  ██    █    █
    █      █   █    █


Key to unlock doors:


         ███
    ██████ █
    █ █  ███



The bitmap font numbers (they're actually inverted in the code):


     ████
    ██  ██
    ██   ██
    ███  ██
     ██████
         ██
    ██  ██
     ████


      ████
     ██  ██
     ███  ██
     ██████
    ██  ████
    ██    ██
     ██  ███
      █████


     ███████
    ██    ██
         ██
         ██
        ██
        ██
       ██
      ████


      ████
     ██  ██
    ██  ███
    ██
    ██████
    ██  ███
    ███  ██
     █████


      █████
     ███
     ██
     █████
         ██
     ██  ██
    ██  ███
     █████


         ███
    ███  ██
     ██  ██
     ██  ██
    ██   ██
    ████████
         ██
        ████


      ████
     ██  ██
        ███
       ██
        ██
     ██  ██
    ██  ███
     █████


     █████
    ██  ███
     ██  ██
         ██
     █████
    ███
    ██
    ███████


       ██
      ███
     ████
       ██
       ██
       ██
      ███
     █████


      ███
     █████
    ██  ███
    ██   ██
    ██   ██
    ███  ██
     █████
      ███

