# Haunted House Commented Disassembly

Reverse-engineering and disassembly of the Atari 2600 game Haunted House designed by James Andreasen. This is a work in progress. The disassembled and commented code is in `haunted.asm`.

## Sprites

The ghost of the old man in the mansion:


    XXX
     XXXX
      XXXX
      XXXXX
      XXXXX
    X XXXXX
     XX X XX
       XXX X
    X      X
    X      X
    XX    XX
     XX  XX
     XXXXXX
      XXXX


Some spiders (upward and downward facing):


      X  X
      X  X    XXX  XXX
      X  X       XX
    X  XX  X   XXXXXX
     XXXXXX   X  XX  X
       XX       X  X
     XX  XX    X    X
    X      X   X    X


Key to unlock doors:


         XXX
    XXXXXX X
    X X  XXX



The bitmap font numbers (they're inverted):


      XXX
     XXXXX
    XXX  XX
    XX   XX
    XX   XX
    XX  XXX
     XXXXX
      XXX
    
     XXXXX
      XXX
       XX
       XX
       XX
     XXXX
      XXX
       XX
    
    XXXXXXX
    XX
    XXX
     XXXXX
         XX
     XX  XX
    XX  XXX
     XXXXX
    
     XXXXX
    XX  XXX
     XX  XX
        XX
       XX
        XXX
     XX  XX
      XXXX
    
       XXXX
        XX
   XXXXXXXX
   XX   XX
    XX  XX
    XX  XX
   XXX  XX
        XXX
    
     XXXXX
    XX  XXX
     XX  XX
         XX
     XXXXX
     XX
     XXX
      XXXXX
    
     XXXXX
    XXX  XX
    XX  XXX
    XXXXXX
    XX
    XX  XXX
     XX  XX
      XXXX
    
     XXXX
      XX
       XX
       XX
        XX
        XX
   XX    XX
    XXXXXXX
    
     XXXXX
    XX  XXX
   XX    XX
   XX  XXXX
    XXXXXX
    XXX  XX
    XX  XX
     XXXX
    
     XXXX
    XX  XX
         XX
     XXXXXX
    XXX  XX
    XX   XX
    XX  XX
     XXXX


