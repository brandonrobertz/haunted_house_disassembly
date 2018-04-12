# Haunted House Commented Disassembly

Reverse-engineering and disassembly of the Atari 2600 game Haunted House designed by James Andreasen. This is a work in progress. The disassembled and commented code is in `haunted.asm`.

![Haunted House](https://github.com/brandonrobertz/haunted_house_disassembly/blob/master/atari_catalog_haunted_house.jpg)


## Sprites

The ghost of old man Zachary Graves:


    ███
     ████
      ████
      █████
      █████
    █ █████
     ██ █ ██
       ███ █
    █      █
    █      █
    ██    ██
     ██  ██
     ██████
      ████


Some spiders (upward and downward facing):


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



The bitmap font numbers (they're inverted):


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

