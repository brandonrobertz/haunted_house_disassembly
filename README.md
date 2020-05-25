# Haunted House Commented Disassembly

Reverse-engineering and disassembly of the Atari 2600 game Haunted House designed by James Andreasen, the _original_ survival horror game. This is a work in progress. The disassembled and commented code is in `haunted.asm`. I would appreciate any tips or advice anyone may have as a PR or Issue.

![Haunted House](https://github.com/brandonrobertz/haunted_house_disassembly/blob/master/atari_catalog_haunted_house.jpg)


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

