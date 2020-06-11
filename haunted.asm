        PROCESSOR 6502
        ORG $F000

;; Disassembly of Haunted House
; Game developed by James Andreasen at Atari
; Disassembly & documentation by Brandon Roberts

; The player, represented by a pair of eyes, must navigate the haunted
; mansion of the late Zachary Graves to recover the three pieces of an
; urn. The game has been identified as one of the earliest examples of the
; survival horror genre.
;     -- Description of the game from Wikipedia

;  The [Atari] 2600 was a real mess.  It's a good thing that we don't try to
;  program machines like that anymore because it was just absurd. But it was
;  kind of fun in a sick way if you like that kind of challenge.
;     -- Howard Scott Warshaw, Atari 2600 game developer
;        Creator of Yar's Revenge and E.T. the Extra-Terrestrial

;; Notes about the 6502 CPU
;; Registers:
;   A: accumulator (8-bit)
;   X, Y: index registers
;   P: processor status flags (8-bits NV-BDIZC)
;   S: stack pointer
;   PC: program counter (16-bit)

;; Addressing modes:
;    Immediate          LDA #$EA        A <- $EA
;    Absolute,X         LDA $0314,X     A <- mem($0314+X)
;    Absolute,Y         LDA $0314,X     A <- mem($0314+Y)
;    Zero page          LDA $02         A <- mem($02)
;    Zero page,X        LDA $02,X       A <- mem($02+X)
;    Zero page,Y        LDA $02,Y       A <- mem($02+Y)
;    (Zero page,X)      LDA ($02,X)     A <- mem(PTR($02+X))
;    (Zero page),Y      LDA ($02),Y     A <- mem(PTR($02)+Y)

;; 6502 ASM Syntax Notes
; $ means the following is hex formatted
; # means the following is a literal number value
; #$FF means hex value FF, typically used like:
;    LDA #$FF ;  store hex FF to accumulator

;; Routines Known to Exist (but not yet found)
; - Polynomial Counter for scattering urn objects
; - Sound subroutines for walking and thunder
; - Difficulty modes: locked doors + keys, ghost
;   restrictions, hiding playfield, etc

VSYNC   =  $00 ; Relevant bits:  ......1. vertical sync set-clear
VBLANK  =  $01 ; Vertical Blank -- between screen draws
               ; Relevant bits: 11....1. vertical blank set-clear
WSYNC   =  $02 ; When this is written to, the processor will wait for
               ; horizontal sync (beam to hit left edge of screen)
               ; You typically see this at STA WSYNC

NUSIZ0  =  $04 ; number & size of player 0/missile 0 sprites
NUSIZ1  =  $05 ; .. player 1/missile 1

;; Color control registers
; 06-09, color/luminosity registers
COLUP0  =  $06 ; color
COLUP1  =  $07 ; color
COLUPF  =  $08 ; color-luminosity playfield
COLUBK  =  $09 ; color-luminosity background

CTRLPF  =  $0A ; playfield controls
PF0     =  $0D ; playfield graphics
PF1     =  $0E
PF2     =  $0F

RESP0   =  $10 ; horiz reset (all objects)
RESP1   =  $11

AUDC0   =  $15 ; audio control
AUDC1   =  $16
AUDF0   =  $17 ; audio frequency
AUDF1   =  $18
AUDV0   =  $19 ; audio volume
AUDV1   =  $1A

GRP0    =  $1B ; player graphic 0 (8-bits each)
GRP1    =  $1C ; player graphic 1
ENAM0   =  $1D ; missile one sprite (1-bit)
ENABL   =  $1F ; ball sprite (1-bit)

HMP0    =  $20 ; horizontal motion register (player 0)

HMOVE   =  $2A ; horizonal sprite positioning (strobe)
               ; writing to here, relatively, moves a sprite
               ; to a new location based on its last (+8, -7)

CXCLR   =  $2C ; clear collision latches
CXP0FB  =  $32 ; collision detect latch (player 0)
CXPPMM  =  $37 ; collision detect (missile)

;; INPT4: input register for the joystick button (1-bit)
; Pin  PlayerP0  PlayerP1  Expl.
; 6    INPT4.7   INPT5.7   Button (0=Pressed, 1=Not pressed)
INPT4   =  $3C

;; SWCHA: joystick motion register (8-bit)
; Pin  PlayerP0  PlayerP1  Expl.
; 1    SWCHA.4   SWCHA.0   Up     (0=Moved, 1=Not moved)
; 2    SWCHA.5   SWCHA.1   Down   ("")
; 3    SWCHA.6   SWCHA.2   Left   ("")
; 4    SWCHA.7   SWCHA.3   Right  ("")
SWCHA   =  $0280

;; SWCHB: switch register on the atai itself (difficulty, etc)
; Bit        Expl.
; SWCHB.0    Reset Button          (0=Pressed)
; SWCHB.1    Select Button         (0=Pressed)
; SWCHB.2    Not used
; SWCHB.3    Color Switch          (0=B/W, 1=Color) (Always 0 for SECAM)
; SWCHB.4-5  Not used
; SWCHB.6    P0 Difficulty Switch  (0=Beginner (B), 1=Advanced (A))
; SWCHB.7    P1 Difficulty Switch  (0=Beginner (B), 1=Advanced (A))
SWCHB   =  $0282

INTIM   =  $0284 ; read-only timer (8-bit)
TIM64T  =  $0296 ; set 64-cycle clock interval (53.6 usec/interval)

; RAM pointers
; top of the stack
STKTOP   = $FF
MVMT     = $8E ; last 4 bits store up, down, left, right joystick bits
               ; bit format, 1=pressed: RLDU (player 1) ---- (player 2)
CLOCK    = $89 ; master frame count timer
PFCOLOR  = $F5 ; wall color
               ; 82h = dark blue (default, diff mode 1)

POS_X    = $AA ; player X position (horizontal)
POS_Y    = $B5 ; player Y position (vertical)

FLOORNO  = $8D ; Which floor we're on 00-04
GAMEMODE = $CC ; Game level 1-9
LINENO   = $CD ; Counts the main game screen scan line (79 toal)
EYE_PTR  = $E3
UNKNOWN1 = $DD
UNKNOWN2 = $EE ; 1c on torch, 03 on eyes
UNKNOWN3 = $83 ; This has something to do with enemies
               ; distance to the player and ability to use
               ; a torch
LIVES    = $96 ; holds number of lives

; Entry point (START)
;LF000
START:
       SEI            ;2 disable interrupts (6507 has no support)
       CLD            ;2 disable decimal mode
       LDX   #STKTOP  ;2 X register = stack address
       TXS            ;2 Init stack (stack register = X = FFh)
       STX   $EB      ;3 stores X to $EB
                      ;2 $EB could be a scan line counter?
       INX            ;2 X++
       STX   GAMEMODE ;3 Start at game mode 1 (0h)
       TXA            ;2 sets A to what was at GAMEMODE

; This is kind of a initial sync loop.
LF00B: STA    VSYNC,X ;4
       INX            ;2
       BPL    LF00B   ;2 loop while X is positive (until X overflows)
       ; initialization routines...
       JSR    LF082   ;6
       JSR    LF141   ;6

; This is the beginning of the main game loop
;LF016
MAIN:  LDA    #$82    ;2 Get some bits to write ...
       STA    WSYNC   ;3 ... to WSYNC. This kicks off new frame draw.
                      ; NOTE: writing to WSYNC will halt execution
                      ; until the beam is at the left edge of screen.
       ; Produce the three lines of vertical sync
       STA    VBLANK  ;3
       STA    VSYNC   ;3
       STA    WSYNC   ;3
       STA    WSYNC   ;3
       LDA    #0      ;2 A=0, do this here to save us cycles later?
       STA    WSYNC   ;3 Wait for new line.
       ; This begins the 37 lines of vertical blank
       STA    VSYNC   ;3 L=2 End of vertical sync pulse
       ; We're in vblank now ...
       INC    CLOCK   ;5 increment frame count timer (only done here)
       LDA    #45     ;2 45 (2Dh) intervals of 64-cycles = 2880 cycles total
       STA    TIM64T  ;4 init the timer for VBLANK time
       JSR    LF079   ;6 This goes into some kind of state mgmt loop
       LDA    $99     ;3
       AND    #$43    ;2
       BNE    LF03E   ;2
       JSR    LF1A5   ;6
       JSR    LF155   ;6
LF03E: JSR    LF2B7   ;6
LF041: LDA    INTIM   ;  Waits for vertical blanking to complete ...
       BNE    LF041
       STA    VBLANK  ;  Begin screen draw
       LDA    #228    ;2 E4h originally
       STA    TIM64T  ;4 Initialize the timer for screen draw
       JSR    STARTPF ;6

; Wait for timer to hit zero. This routine gets
; ran at the bottom of the screen while drawing
; blank space beneath the floor and game number
; display.
LF050: LDA    INTIM   ;4
       BNE    LF050   ;2
       STA    WSYNC   ;3
       LDA    #$82    ;2
       STA    VBLANK  ;3
       LDA    #$24    ;2
       STA    TIM64T  ;4
       BIT    $99     ;3
       BVS    LF070   ;2
       JSR    LF8AC   ;6
       JSR    LFB68   ;6 an input processing subroutine
       JSR    COLLIDE0;6 collision detection subroutine
       JSR    LFC3C   ;6 a subroutine for drawing the bottom screen
LF070: LDA    INTIM   ;4 reads the timer to A
       BNE    LF070   ;2 loops until the timer runs to zero (completes)
       STA    VBLANK  ;3 sets VBLANK with A
       BEQ    MAIN    ;2
LF079: LDA    SWCHB   ;4 loads the atari switch states to A
       ROR            ;2 rotate bit right, accumulator
       BCC    LF082   ;2 branch if the difficulty atari switch is set
       JMP    LF125   ;3

; This swaps the memory locations of $EB and $EC
; The following initializes for a loop in LF08E, which runs
; from 0x80 to 0x96 (22 loops)
LF082: LDX    $EB     ;3
       LDA    $EC     ;3
       STA    $EB     ;3
       STX    $EC     ;3
       LDX    #128    ;2 80h, o.g.
       LDA    #0      ;2

LF08E: STA    VSYNC,X ;4
       INX            ;2
       CPX    #$96    ;2 Checks to see if X == $96 ...
       BNE    LF08E   ;2 ... loops if not true. Runs 22 times.
       LDX    #$09    ;2 Initializes so A will get last item in DATA2
;LF097: Copies 10-byte array from DATA2 to $96
COPY0: LDA    DATA2,X ;4
       STA    LIVES,X ;4 Then stores item from DATA2 in $96 + X offset
       DEX            ;2
       BPL    COPY0   ;2
       JSR    LF0E7   ;6
       LDX    #$03    ;2
       STX    $A5     ;3
       INX            ;2
LF0A7: LDA    $A5,X   ;4
       STA    $A0,X   ;4
       LDA    $B0,X   ;4
       STA    $AB,X   ;4
       LDA    $BB,X   ;4
       STA    $B6,X   ;4
       DEX            ;2
       BPL    LF0A7   ;2
       LDY    GAMEMODE;3
       CPY    #$02    ;2
       BNE    LF0C7   ;2
       INX            ;2
       STX    $A0     ;3
       LDX    #$74    ;2
       STX    $AB     ;3
       LDX    #132    ;2 84h hex (original)
       STX    $B6     ;3
LF0C7: LDA    #$35    ;2
       STA    CTRLPF  ;3
       JSR    LFD0A   ;6
       LDX    #$04    ;2
       LDA    GAMEMODE;3
       CMP    #$04    ;2
       BCS    LF0D8   ;2
       LDX    #$02    ;2
LF0D8: STX    $EA     ;3
       LDA    #$80    ;2
       STA    POS_X   ;3
       LDA    #$86    ;2
       STA    POS_Y   ;3
       LDA    #$26    ;2
       STA    $CB     ;3
       RTS            ;6

; This subroutine randomizes some data in RAM based on bit
; shifts and rolls between RAM $EC and $EB. It does this based
; on an input value (A) and also on arrays in memory RAM $A5
LF0E7: JSR    LF4C9   ;6 Performs some kind of bit mixing with $EC & EB ...
       AND    #$07    ;2 ... then checks the result of above ...
       CMP    #$06    ;2 ... to see if it's result is >= $06.
                      ;  (CMP sets carry bit if A >= M)
       BCS    LF0E7   ;2 Loop if not.
       STA    $D0     ;3 The result of the above, once it's greater than
                      ;  6 is set to RAM: $D0.
       LDX    #$04    ;2
LF0F4: JSR    LF4C9   ;6 Then we do it again.
       AND    #$03    ;2
       STA    $A5,X   ;4 Stores result as RAM $A5[X]
       TAY            ;2 Y = A
       TXA            ;2 A = X
LF0FD: CLC            ;2
       ADC    $D0     ;3
       CMP    #$06    ;2
       BCC    LF106   ;2
       SBC    #$06    ;2
LF106: CMP    $9C     ;3
       BNE    LF112   ;2
       CPY    FLOORNO ;3
       BNE    LF112   ;2
       LDA    #$05    ;2
       BNE    LF0FD   ;2
LF112: STA    $C5,X   ;4
       STA    $C0,X   ;4
       TAY            ;2
       LDA    LFFAA,Y ;4
       STA    $B0,X   ;4
       LDA    LFFBD,Y ;4
       STA    $BB,X   ;4
       DEX            ;2
       BPL    LF0F4   ;2
       RTS            ;6

LF125: ROR            ;2
       BCC    LF12D   ;2
       LDX    #$01    ;2
       STX    $E5     ;3
LF12C: RTS            ;6

LF12D: DEC    $E5     ;5
       BNE    LF12C   ;2
       LDA    #$2D    ;2
       STA    $E5     ;3
       INC    GAMEMODE;5
       LDA    GAMEMODE;3
       CMP    #$09    ;2
       BNE    LF13F   ;2
       LDA    #0      ;2
LF13F: STA    GAMEMODE;3

; Initializes some game state variables
LF141: JSR    LF194   ;6
       LDA    #64     ;2 40h originally
       STA    $99     ;3
       LDA    #16     ;2 10h, initializes mem 84h
       STA    $84     ;3
       LDA    #0      ;2
       STA    $85     ;3
       STA    $80     ;3
       STA    UNKNOWN3;3
       RTS            ;6

LF155: BIT    $9B     ;3
       BPL    LF17E   ;2
       LDA    FLOORNO ;3
       BNE    LF17E   ;2
       LDA    #$02    ;2
       CMP    $9C     ;3
       BNE    LF17E   ;2
       CMP    $9A     ;3
       BNE    LF179   ;2
       LDA    $9D     ;3
       CMP    #$08    ;2
       BNE    LF179   ;2
       LDA    #$01    ;2
       STA    CLOCK     ;3
       STA    $8A     ;3
       LDA    #$44    ;2
       STA    $99     ;3
       BNE    LF17E   ;2
LF179: LDX    #$02    ;2
       JSR    LF88C   ;6
LF17E: DEC    $87     ;5
       BNE    LF188   ;2
       LDA    #$3C    ;2
       STA    $87     ;3
       DEC    $DF     ;5
LF188: BIT    $8A     ;3
       BVC    LF19E   ;2
       LDA    UNKNOWN3;3
       BNE    LF194   ;2
       LDA    $DF     ;3
       BNE    LF19E   ;2
LF194: LDA    #0      ;2
       STA    $85     ;3
       LDA    $8A     ;3
       AND    #$07    ;2
       STA    $8A     ;3
LF19E: LDA    $8A     ;3
       EOR    #$80    ;2
       STA    $8A     ;3
       RTS            ;6

LF1A5: JSR    LF1BF   ;6
       LDA    #0      ;2
       STA    $91     ;3
       LDA    POS_Y   ;3
       CMP    #$26    ;2
       BCC    LF1BC   ;2
       CMP    #$D7    ;2
       BCS    LF1BA   ;2
       LDA    #$26    ;2
       BNE    LF1BC   ;2
LF1BA: SBC    #$AF    ;2
LF1BC: STA    $CB     ;3
       RTS            ;6

LF1BF: LDA    #0      ;2
       STA    $E7     ;3
       STA    $E8     ;3
       LDA    $9B     ;3
       AND    #$7F    ;2
       STA    $9B     ;3
       BIT    $91     ;3
       BVS    LF206   ;2
       BIT    MVMT    ;3
       BVS    LF1E3   ;2
       BPL    LF206   ;2
       INC    $E7     ;5
       LDA    POS_X   ;3
       CMP    #$94    ;2
       BEQ    LF254   ;2
       JSR    LF272   ;6
       JMP    LF1EE   ;3
LF1E3: DEC    $E7     ;5
       LDA    POS_X   ;3
       CMP    #$04    ;2
       BEQ    LF254   ;2
       JSR    LF27D   ;6
LF1EE: JSR    LF295   ;6
       BNE    LF201   ;2
       JSR    LF28A   ;6
       BNE    LF201   ;2
       LDA    POS_X   ;3
       CLC            ;2
       ADC    $E7     ;3
       STA    POS_X   ;3
       BNE    LF206   ;2
LF201: LDX    #$03    ;2
       JSR    LF88C   ;6
LF206: BIT    $91     ;3
       BMI    LF253   ;2 return
       LDA    MVMT    ;3
       AND    #$30    ;2
       BEQ    LF253   ;2 return
       CMP    #$20    ;2
       BEQ    LF22E   ;2
       INC    $E8     ;5
       LDA    POS_Y   ;3
       CMP    #$FB    ;2
       BEQ    LF260   ;2
       JSR    LF26B   ;6
       JSR    LF29C   ;6
       BNE    LF24E   ;2
       JSR    LF279   ;6
       JSR    LF29C   ;6
       BNE    LF24E   ;2
       BEQ    LF246   ;2
LF22E: DEC    $E8     ;5
       LDA    POS_Y   ;3
       CMP    #$01    ;2
       BEQ    LF260   ;2
       JSR    LF26B   ;6
       JSR    LF28E   ;6
       BNE    LF24E   ;2
       JSR    LF279   ;6
       JSR    LF28E   ;6
       BNE    LF24E   ;2
LF246: LDA    POS_Y   ;3
       CLC            ;2
       ADC    $E8     ;3
       STA    POS_Y   ;3
       RTS            ;6

LF24E: LDX    #$03    ;2
       JSR    LF88C   ;6
LF253: RTS            ;6

LF254: LDA    $9B     ;3
       ORA    #$80    ;2
       STA    $9B     ;3
       CMP    #$8F    ;2
       BEQ    LF201   ;2
       BNE    LF206   ;2
LF260: LDA    $9B     ;3
       ORA    #$80    ;2
       STA    $9B     ;3
       CMP    #$8F    ;2
       BEQ    LF24E   ;2
       RTS            ;6

LF26B: LDA    POS_X   ;3
       CLC            ;2
       ADC    #$07    ;2
       BNE    LF281   ;2
LF272: LDA    POS_X   ;3
       CLC            ;2
       ADC    #$08    ;2
       BNE    LF281   ;2
LF279: LDA    POS_X   ;3
       BNE    LF281   ;2
LF27D: LDY    POS_X   ;3
       DEY            ;2
       TYA            ;2
LF281: LSR            ;2
       LSR            ;2
       LSR            ;2
       LSR            ;2
       TAY            ;2
       DEY            ;2
       STY    $D4     ;3
       RTS            ;6

LF28A: LDA    POS_Y   ;3
       BNE    LF2A1   ;2
LF28E: LDY    POS_Y   ;3
       DEY            ;2
       TYA            ;2
       JMP    LF2A1   ;3
LF295: LDA    POS_Y   ;3
       CLC            ;2
       ADC    #$02    ;2
       BNE    LF2A1   ;2
LF29C: LDA    POS_Y   ;3
       CLC            ;2
       ADC    #$03    ;2
LF2A1: LSR            ;2
       LSR            ;2
       LSR            ;2
       LSR            ;2
       TAX            ;2
       LDY    $D4     ;3
       TYA            ;2
       AND    #$08    ;2
       BEQ    FLOORMASK ;2
       CPX    #$08    ;2
       RTS            ;6

; this routine handles the floor plan playfield sprites
; X appears to be the scanline
;FLOORMASK:
FLOORMASK:
       LDA    FLOOR0,X ;4
       AND    MASK0,Y ;4
       RTS            ;6

LF2B7: LDA    #$30    ;2
       STA    NUSIZ0  ;3
       LDA    #0      ;2
       STA    COLUBK  ;3
       STA    $F1     ;3
       STA    ENAM0   ;3
       STA    ENABL   ;3 Ball sprite
       STA    NUSIZ1  ;3
       LDA    $99     ;3
       CMP    #$02    ;2
       BEQ    LF2D8   ;2
       AND    #$04    ;2
       BEQ    LF2DB   ;2
       LDA    CLOCK     ;3
       BNE    LF2D8   ;2
       JSR    LF141   ;6
LF2D8: JMP    LF315   ;3
LF2DB: LDA    $80     ;3
       BEQ    LF2ED   ;2
       JSR    LF57D   ;6
       LDA    CLOCK     ;3
       LSR            ;2
       AND    #$07    ;2
       TAX            ;2
       LDA    LFF92,X ;4
       BNE    LF318   ;2
LF2ED: LDX    $81     ;3
       BEQ    LF300   ;2
       LDX    $E9     ;3
       LDA    #$08    ;2
       STA    $ED     ;3
       LDA    FLOORNO ;3
       STA    $A0,X   ;4
       JSR    LF526   ;6
       BPL    LF315   ;2
LF300: BIT    $8A     ;3
       BVC    LF312   ;2
       BIT    $8A     ;3
       BMI    LF30D   ;2
       JSR    LF4F1   ;6
       BPL    LF315   ;2
LF30D: JSR    LF57D   ;6
       BPL    LF34D   ;2
LF312: JSR    LF57D   ;6
LF315: LDA    SWCHA   ;4
LF318: LDY    #$E7    ;2
       STY    $D8     ;3
       STY    $D9     ;3
       STY    $DA     ;3
       LDY    #$A5    ;2
       LDX    #$01    ;2
       ROL            ;2
       BCS    LF329   ;2
       LDY    #$C6    ;2
LF329: ROL            ;2
       BCS    LF32E   ;2
       LDY    #$63    ;2
LF32E: ROL            ;2
       BCS    LF332   ;2
       DEX            ;2
LF332: ROL            ;2
       BCS    LF336   ;2
       INX            ;2
; Set up eyes
; This could set up the eyeball graphic sprite
; into memory via the EYE_PTR $E3, which leads
; to D8 (into ram) when the scanline is going
; to draw the start of the centered eyes.
LF336: TYA            ;2
       STA    $D8,X   ;4
       LDA    #$D8    ;2
       STA    EYE_PTR ;3
       LDA    #0      ;2
       STA    $E4     ;3
       LDA    #$03    ;2
       STA    $EE     ;3
       LDY    #$01    ;2
       LDA    $CB     ;3
       LDX    POS_X   ;3
       BNE    LF397   ;2
LF34D: LDA    $CF     ;3
       STA    $E0     ;3
       LDA    GAMEMODE;3
       BEQ    LF35F   ;2
       BIT    $88     ;3
       BMI    LF35F   ;2
       LDA    #$02    ;2
       STA    ENABL   ;3 ball sprite
       STA    ENAM0   ;3
LF35F: LDA    CLOCK   ;3
       AND    #$06    ;2
       LSR            ;2
       TAX            ;2
       LDA    LFF9A,X ;4 eyeball sprite?
       STA    $E3     ;3
       LDA    #STKTOP ;2
       STA    $E4     ;3
       LDA    POS_X   ;3
       SEC            ;2
       SBC    #$0D    ;2
       BCS    LF37B   ;2
       ADC    #$A0    ;2
       LDX    #$97    ;2
       BNE    LF381   ;2 Jump if X != 0. NOTE: BNE jumps if z flag is 0 
                      ;  LDX/A/Y, the z flag is 1 when the result is zero, and
                      ;  zero when the result is nonzero
LF37B: CMP    #132    ;2 84h original
       BCC    LF387   ;2
       LDX    #0      ;2
LF381: STX    $E0     ;3
       LDX    #$02    ;2
       STX    ENABL   ;3 ball sprite
LF387: TAX            ;2
       LDA    #$1C    ;2
       STA    $EE     ;3
       LDA    #$07    ;2
       STA    NUSIZ1  ;3
       LDY    #$04    ;2
       LDA    $CB     ;3
       SEC            ;2
       SBC    #$0C    ;2
; Might set background PF color based on game mode
; and floor
LF397: STA    $F0     ;3
       STX    $DD     ;3
       STY    $F2     ;3
       LDY    #$01    ;2
       STY    $F3     ;3
       DEY            ;2
       STY    $F4     ;3
       STY    PFCOLOR ;3
       LDA    #$08    ;2
       CLC            ;2
       ADC    FLOORNO ;3
       LDX    GAMEMODE;3
       BNE    LF3B3   ;2
       STA    PFCOLOR ;3
       BPL    LF3B5   ;2
LF3B3: STA    $F4     ;3
LF3B5: LDA    $80     ;3
       BEQ    LF3BD   ;2
       LDA    #$03    ;2
       BNE    LF3C8   ;2
LF3BD: LDA    UNKNOWN3;3
       BEQ    LF3D5   ;2
       BIT    SWCHB   ;4
       BVS    LF3D5   ;2
       LDA    #$27    ;2
LF3C8: AND    CLOCK   ;3
       BNE    LF3D5   ;2
       LDA    $EB     ;3
       ROR            ;2
       BCC    LF3D5   ;2
       LDY    #$01    ;2
       STY    PFCOLOR ;3
LF3D5: LDX    #$F7    ;2
       LDY    #0      ;2
       STY    $D0     ;3
       LDA    SWCHB   ;4
       AND    #$08    ;2
       BNE    LF3E8   ;2
       LDA    #$0C    ;2
       STA    $D0     ;3
       LDX    #$07    ;2
; TODO: i think this has to do with the cycling BG color
LF3E8: STX    $D3     ;3
       LDA    $84     ;3
       AND    #$10    ;2
       BEQ    LF3FF   ;2
       LDA    CLOCK   ;3
       BNE    LF3F6   ;2
       INC    $84     ;5 mem 84h increments every FF frames
LF3F6: LDA    $84     ;3
       ORA    #$10    ;2
       STA    $84     ;3
       TAY            ;2
       BNE    LF401   ;2
LF3FF: LDX    #STKTOP ;2
LF401: STY    $D1     ;3
       STX    $D2     ;3
       LDX    #$04    ;2
LF407: LDA    $F1,X   ;4
       CLC            ;2
       ADC    $D0     ;3
       TAY            ;2
       LDA    LFFE4,Y ;4
       EOR    $D1     ;3
       AND    $D2     ;3
       STA    $F1,X   ;4
       DEX            ;2
       BPL    LF407   ;2
       LDA    $F2     ;3
       STA    COLUP1  ;3
       LDA    $F1     ;3
       CMP    #$AA    ;2
       BNE    LF429   ;2
       LDA    $D3     ;3
       ORA    #$08    ;2
       AND    CLOCK   ;3
LF429: STA    COLUP0  ;3
       LDA    $99     ;3
       AND    #$04    ;2
       BEQ    LF437   ;2
       LDA    CLOCK   ;3
       AND    $D3     ;3
       STA    PFCOLOR ;3
LF437: LDX    #$04    ;2
       LDA    $99     ;3
       CMP    #$02    ;2
       BNE    LF441   ;2
       LDX    #$01    ;2
LF441: LDA    #$02    ;2
       CPX    #$02    ;2
       BCS    LF448   ;2
       LSR            ;2  shift right one bit (accumulator)
LF448: CLC            ;2
       ADC    $DC,X   ;4
       LDY    #$02    ;2
       SEC            ;2
LF44E: INY            ;2
       SBC    PF2     ;3
       BCS    LF44E   ;2
       EOR    #STKTOP ;2
       SBC    #$06    ;2
       ASL            ;2 arithmetic shift left one bit, accumulator
       ASL            ;2
       ASL            ;2
       ASL            ;2
       STA    WSYNC   ;3
LF45D: DEY            ;2
       BPL    LF45D   ;2
       STA    RESP0,X ;4
       STA    HMP0,X  ;4
       DEX            ;2
       BPL    LF441   ;2
       STA    WSYNC   ;3
       STA    HMOVE   ;3
       LDA    #79     ;2 4Fh
       STA    LINENO  ;3
       STA    $D3     ;3
       LDA    #0      ;2
       STA    $D4     ;3
       BIT    $8A     ;3
       BVC    LF4A1   ;2
       BIT    $88     ;3
       BMI    LF4A1   ;2
       LDA    GAMEMODE;3
       BEQ    LF4A1   ;2
       LDA    $8C     ;3
       BEQ    LF48C   ;2
       JSR    LF4DB   ;6
       BMI    LF48C   ;2
       STA    $D4     ;3
LF48C: LDA    $8B     ;3
       BEQ    LF4A1   ;2
       TAX            ;2
       JSR    LF4DB   ;6
       CMP    #$50    ;2
       BCS    LF4A1   ;2
       STA    $D3     ;3
       LDA    #$08    ;2
       STA    $D0     ;3
       TXA            ;2
       BNE    LF4AF   ;2
LF4A1: LDA    POS_Y   ;3
       CMP    #$26    ;2
       BCC    LF4BD   ;2
       CMP    #$D6    ;2
       BCS    LF4C3   ;2
       ADC    #$2A    ;2
       STA    $D0     ;3
LF4AF: LSR            ;2 Logical Shift One Bit Right (accumulator)
       LSR            ;2
       LSR            ;2
       LSR            ;2
       EOR    #$0F    ;2
       STA    $D1     ;3
       ASL            ;2
       ADC    $D1     ;3
       TAX            ;2
       BPL    LF4C9   ;2
LF4BD: LDX    #$1E    ;2
       LDA    #0      ;2
       BEQ    LF4C7   ;2
LF4C3: LDX    #0      ;2
       LDA    #$0F    ;2
LF4C7: STA    $D0     ;3
; This might be some kind of deterministic PRNG
LF4C9: LDA    $EC     ;3 Loads RAM $EC into A (these get swapped every cycle)
       EOR    $EB     ;3 XOR $EC and $EB to A
       ASL            ;2 Multiply result by two ...
       ASL            ;2 ... and again.
       ROL    $EB     ;5 Rotate bits left in $EB ...
       ROL    $EC     ;5 ... and $EC
       LDA    $EB     ;3
       RTS            ;6

LF4D6: .byte $07,$01,$06,$06,$06
LF4DB: CLC            ;2
       ADC    $CB     ;3
       SEC            ;2
       SBC    POS_Y   ;3
       RTS            ;6

LF4E2: TXA            ;2
       CPX    #$02    ;2
       BCC    LF4E9   ;2
       LDA    $9B,X   ;4
LF4E9: ASL            ;2
       ASL            ;2
       ASL            ;2
       ADC    #$08    ;2
       LDX    #$FE    ;2
       RTS            ;6

LF4F1: LDX    $86     ;3
LF4F3: DEX            ;2
       BPL    LF505   ;2
       LDX    #$04    ;2
       BNE    LF505   ;2
LF4FA: CPX    $86     ;3
       BNE    LF4F3   ;2
LF4FE: LDA    #0      ;2
       STA    $DC     ;3
       STA    $ED     ;3
       RTS            ;6

LF505: CPX    $9A     ;3
       BEQ    LF4FA   ;2
       CPX    #$02    ;2
       BCC    LF511   ;2
       LDA    $9B,X   ;4
       BMI    LF4FA   ;2
LF511: LDA    GAMEMODE;3
       CMP    #$02    ;2
       BCS    LF51B   ;2
       CPX    #0      ;2
       BEQ    LF4FA   ;2
LF51B: LDA    $A0,X   ;4
       CMP    FLOORNO ;3
       BNE    LF4FA   ;2
       JSR    LF53D   ;6
       BEQ    LF4FA   ;2
LF526: LDY    $81     ;3
       BNE    LF52F   ;2
       LDA    LF4D6,X ;4
       STA    $F1     ;3
LF52F: STX    $86     ;3
       JSR    LF4E2   ;6
       STA    $E1     ;3
       STX    $E2     ;3
       LDA    #$08    ;2
       STA    $ED     ;3
       RTS            ;6

LF53D: LDA    POS_X   ;3
       SEC            ;2
       SBC    $AB,X   ;4
       BCS    LF548   ;2
       EOR    #STKTOP ;2
       ADC    #$01    ;2
LF548: STA    $D0     ;3
       LDA    POS_Y   ;3
       SEC            ;2
       SBC    $B6,X   ;4
       BCS    LF555   ;2
       EOR    #STKTOP ;2
       ADC    #$01    ;2
LF555: STA    $D1     ;3
       CMP    $D0     ;3
       BCS    LF55E   ;2
       LSR            ;2
       BPL    LF560   ;2
LF55E: LSR    $D0     ;5
LF560: CLC            ;2
       ADC    $D0     ;3
       CMP    #$11    ;2
       BCS    LF595   ;2
       LDA    $AB,X   ;4
       STA    $DC     ;3
       LDA    $B6,X   ;4
LF56D: JSR    LF4DB   ;6
       CMP    #$50    ;2
       BCC    LF578   ;2
       CMP    #$F9    ;2
       BCC    LF595   ;2
LF578: STA    $EF     ;3
       LDA    #$01    ;2
       RTS            ;6

LF57D: LDX    $85     ;3
LF57F: DEX            ;2
       BPL    LF598   ;2
       LDX    $EA     ;3
       BIT    $8A     ;3
       BVC    LF598   ;2
       INX            ;2
       JSR    LF5E7   ;6
       BEQ    LF591   ;2
       STX    $85     ;3
       RTS            ;6

LF591: CPX    $85     ;3
       BNE    LF57F   ;2
LF595: JMP    LF4FE   ;3
LF598: JSR    LF59E   ;6
       BEQ    LF591   ;2
       RTS            ;6

LF59E: LDA    $A5,X   ;4
       CMP    FLOORNO ;3
       BNE    LF595   ;2
       LDA    GAMEMODE;3
       BEQ    LF5BA   ;2
       LDA    $80     ;3
       BNE    LF5BA   ;2
       LDA    UNKNOWN3;3
       BEQ    LF595   ;2
       LDA    $C5,X   ;4
       CMP    $9C     ;3
       BEQ    LF5BA   ;2
       CMP    $97     ;3
       BNE    LF595   ;2
LF5BA: INX            ;2
       STX    $F1     ;3
       DEX            ;2
       LDA    $B0,X   ;4
       STA    $DC     ;3
       LDA    $BB,X   ;4
       JSR    LF56D   ;6
       BEQ    LF591   ;2
       STX    $85     ;3
       LDA    #$50    ;2
       CLC            ;2
       ADC    LF5E2,X ;4
       BIT    $EB     ;3
       BVS    LF5D7   ;2
       ADC    #$0A    ;2
LF5D7: STA    $E1     ;3
       LDA    #$FE    ;2
       STA    $E2     ;3
       LDA    #$0A    ;2
       STA    $ED     ;3
       RTS            ;6

LF5E2: .byte $00,$14,$28,$28,$28
LF5E7: LDA    $9B     ;3
       AND    #$0F    ;2
       CMP    #$0F    ;2
       BEQ    LF62B   ;2
       TAY            ;2
       CPY    #$04    ;2
       BCC    LF5F9   ;2
       TYA            ;2
       AND    #$03    ;2
       EOR    #$01    ;2
LF5F9: STA    $D0     ;3
       ASL            ;2
       ASL            ;2
       ASL            ;2
       ASL            ;2
       CLC            ;2
       ADC    $D0     ;3
       ADC    #$8C    ;2
       STA    $E1     ;3
       LDA    #0      ;2
       ADC    #$FE    ;2
       STA    $E2     ;3
       TYA            ;2
       AND    #$03    ;2
       TAY            ;2
       LDA    LFF88,Y ;4
       JSR    LF4DB   ;6
       STA    $EF     ;3
       LDY    $9C     ;3
       LDA    LFF8C,Y ;4
       STA    $DC     ;3
       LDA    #0      ;2
       STA    $F1     ;3
       LDA    #$11    ;2
       STA    $ED     ;3
       LDA    #$35    ;2
       STA    NUSIZ0  ;3
LF62B: RTS            ;6

; Entry point to drawing new playfield. Begins with
; collusion clearing and some setup, then draws
; field and sprites
;LF62C:
STARTPF:
       STA    CXCLR   ;3 Clear collision latches, we're drawing a new
                      ;  playfield
       LDY    #0      ;2
LF630: LDA    LINENO  ;3
       CMP    $D3     ;3
       BEQ    LF63E   ;2
       STA    WSYNC   ;3
       STA    WSYNC   ;3
       DEC    LINENO  ;5
       BPL    LF630   ;2

LF63E: LDA    $D0     ; (initially 175)
       AND    #$0F
       BNE    LF647   ; We don't need to move to the next part of
                      ; the playfield (it gets repeated vertically)
       INX            ; Use the next playfield sprites (organized in
                      ; blocks of three (PF0,PF1,PF2)
       INX
       INX

; This subroutine handles drawing the background playfield
; Playfield breakdown
;   PF0: ABCD ----
;   PF1: EFGH IJKL
;   PF2: MNOP QRST
;
; Line result (spaces just highlight PF registers)
; Mirrored:
;   DCBA EFGHIJKL TSRQPONM MNOPQRST LKJIHGFE ABCD
; Repeated:
;   DCBA EFGHIJKL TSRQPONM DCBA EFGHIJKL TSRQPONM

; Main playfield kernel
; Params:
;   Y = player 1 graphic sprite
;   X = playfield sprite offset
;
; Initial ram settings:
;   LINENO = 79 (4Fh)
LF647: STA    WSYNC   ;3
       LDA    PFCOLOR ;3 loads wall color from RAM
       STA    COLUPF  ;3 sets background color
       STY    GRP1    ;3 draw player 1 sprite (passed in above, via Y)
       LDA    GFX0,X  ;4 load playfield sprite to A
       STA    PF0     ;3 draw playfield 0
       LDA    GFX1,X  ;4
       STA    PF1     ;3 loads $F0 to playfield 1 RAM location
       LDA    GFX2,X  ;4
       STA    PF2     ;3 selects a sprite from SPRITES1, draws to playfield
                      ;  this could be a piece of text, etc
       LDA    LINENO  ;3
       SEC            ;2 set carry ...
       SBC    $EF     ;3 subtract with carry (A-mem(EF)) (first pass: A - 61 == 12)
       CMP    $ED     ;3 (initially, EDh=0)
       BCS    LF68D   ;2 branch when carry set (yes, on first)
       TAY            ;2
       LDA    ($E1),Y ;5
       TAY            ;2
LF66B: DEC    LINENO  ;5 LINENO--
       LDA    LINENO  ;3
       CMP    $D4     ;3 LINENO == mem($D4)
       BEQ    LF691   ;2
       DEC    $D0     ;5
       STA    WSYNC   ;3
       STY    GRP0    ;3 draws player 0 sprite from Y
       LDA    LINENO  ; This block checks to see if
       SEC            ; our scanline is close to the same line ($F0)
       SBC    $F0     ; as the player (EE=3 if eyes, more if torch)
       CMP    $EE     ; and then jumps to set player sprite in Y.
       BCS    LF689   ;
       TAY            ;2
       LDA    (EYE_PTR),Y ;5 mem(PTR($E3)+Y), pointer into mem for the eyes
       TAY            ;2 set eye graphic into Y for later drawing
       JMP    LF63E   ;3
LF689: LDY    #0      ;2
       BEQ    LF63E   ;2
LF68D: LDY    #0      ;2
       BEQ    LF66B   ;2 always jump to LF66B (clear player 1 sprite)
LF691: STA    WSYNC   ;3
       LDA    #0      ;2
       STA    GRP0    ;3 draws A ref sprite via GRP0
       STA    GRP1    ;3 draws A reg sprite via GRP1
       STA    PF1     ;3
       STA    PF2     ;3
       STA    ENAM0   ;3
       STA    ENABL   ;3 ball sprite
LF6A1: DEC    LINENO  ;5
       BMI    LF6AB   ;2
       STA    WSYNC   ;3
       STA    WSYNC   ;3
       BPL    LF6A1   ;2
LF6AB: STA    WSYNC   ;3
       LDA    $F4     ;3
       STA    COLUBK  ;3
       LDY    #STKTOP ;2
       STY    PF0     ;3
       INY            ;2
       STY    COLUPF  ;3
       LDX    #$07    ;2
       STA    WSYNC   ;3
LF6BC: DEX            ;2
       BPL    LF6BC   ;2
       STA    RESP0   ;3
       LDA    #$40    ;2
       STA    HMP0    ;3
       STA    WSYNC   ;3
       STA    HMOVE   ;3
       LDA    #$32    ;2
       STA    NUSIZ0  ;3
       STA    NUSIZ1  ;3
       STX    $DB     ;3
       STX    $D9     ;3
       STX    $D7     ;3
       STX    $D5     ;3
       LDA    $82     ;3
       AND    #$F0    ;2
       LSR            ;2
       STA    $D4     ;3
       LDA    $82     ;3
       AND    #$0F    ;2
       ASL            ;2
       ASL            ;2
       ASL            ;2
       STA    $D6     ;3
       LDA    LIVES   ;3
       ASL            ;2
       ASL            ;2
       ASL            ;2
       STA    $D8     ;3
       BIT    $99     ;3
       BVC    LF6F6   ;2
       LDY    GAMEMODE;3
       BPL    LF6F8   ;2
LF6F6: LDY    FLOORNO ;3
LF6F8: INY            ;2
       TYA            ;2
       ASL            ;2
       ASL            ;2
       ASL            ;2
       STA    $DA     ;3
       LDX    $9A     ;3
       JSR    LF4E2   ;6
       STX    $D1     ;3
       BIT    $9A     ;3
       BPL    LF70C   ;2
       LDA    #$6A    ;2
LF70C: STA    $D0     ;3
       LDA    $F3     ;3
       STA    COLUP0  ;3
       STA    COLUP1  ;3
       LDY    #7
; Draw floor no. (bottom of the screen, above torch count & lives)
LF716: STA    WSYNC
       LDA    ($DA),Y ; number sprite pointer + Y index
       STA    GRP0    ; draw floor number
       LDX    #5
; Draw urn piece
LF71E: DEX            ; wait until score is drawn
       BPL    LF71E
       LDA    ($D0),Y ; urn piece sprite pointer + Y index
       STA    GRP0    ; draw urn piece sprite
       DEY            ; decrement sprite line counter
       BPL    LF716   ; draw rest of sprites if >= 0
       INY            ; Y = 0
       STY    WSYNC   ; start next line ...
       STY    GRP0    ; clear out sprites, we want to leave blank
       STY    GRP1    ;   space between the floor/urn & torches/lives
       LDX    #3
       STA    WSYNC
; Setup for torch count and lives drawing
LF733: DEX
       NOP            ; cycle counting is important here!
       BPL    LF733
       LDA    $D0
       STA    RESP0   ; TODO: look these up, I thought we cleared
       STA    RESP1   ;   the sprites out above
       LDA    #$34    ; set the sprite draw mode: double wide
       STA    NUSIZ0  ; for player sprite 0 (first torch no digit)
       STA    NUSIZ1  ; and for player sprite 1 (second torch no digit)
       LDY    #7
; Draw torch count, this appears at the bottom
; left of the screen and is the first thing drawn
; LF745:
DRAW_TORCH_CT:
       STA    WSYNC   ;3 wait for sync
       LDA    ($D4),Y ;5 load number sprite
       STA    GRP0    ;3 draw torch count digit 1
       LDA    ($D6),Y ;5 load number sprite
       STA    GRP1    ;3 draw torch count digit 2
       LDX    #$03    ;2
; wait for floor number sprite to be drawn on the left 
; side of the screen (wait for scan line to get to
; the right hand side of screen) ...
LF751: DEX            ;2
       BPL    LF751   ;2
       LDA    LINENO  ;3
       INX            ;2
       STX    GRP0    ;3 draws sprite in X via GRP0
       LDA    ($D8),Y ;5
       STA    GRP1    ;3 draw eyes or lives (if in bottom)
       DEY            ;2
       BPL    DRAW_TORCH_CT
       INY            ;2
       STY    GRP1    ;3 draws Y reg sprite via GRP1
       LDA    $99     ;3
       BNE    LF7A3   ;2
       LDA    UNKNOWN3;3
       BNE    LF773   ;2
       LDX    $98     ;3
       CPX    #$7F    ;2
       BEQ    LF7DB   ;2
       STA    $92     ;3
LF773: LDA    $98     ;3
       TAY            ;2
       CMP    #$7F    ;2
       BEQ    LF78A   ;2
       AND    #$0F    ;2
       BNE    LF78C   ;2
       TYA            ;2
       BEQ    LF78A   ;2
       BMI    LF78A   ;2
       LDX    $EB     ;3
       LDA    START,X ;4 Very strange, investigate more
       AND    #$01    ;2
LF78A: STA    $92     ;3
LF78C: LDA    $92     ;3
       BNE    LF794   ;2
       INC    $98     ;5
       BNE    LF796   ;2
LF794: DEC    $98     ;5
LF796: LDX    #$08    ;2
       TYA            ;2
       LSR            ;2
       LSR            ;2
       LSR            ;2
       ORA    #$10    ;2
       TAY            ;2
       EOR    #$0F    ;2
       BPL    LF7DD   ;2
LF7A3: LSR            ;2
       BCC    LF7C4   ;2
       LDA    $8F     ;3
       BNE    LF7B7   ;2
       LDA    $EB     ;3
       AND    #$70    ;2
       LSR            ;2
       LSR            ;2
       CLC            ;2
       ADC    #$10    ;2
       STA    $8F     ;3
       BPL    LF7DB   ;2
LF7B7: LDA    $8F     ;3
       EOR    #STKTOP ;2
       TAY            ;2
       DEC    $8F     ;5
       LDX    #$08    ;2
       LDA    #$0C    ;2
       BPL    LF7DD   ;2
LF7C4: CMP    #$22    ;2
       BNE    LF7DB   ;2
       LDA    CLOCK   ;3
       LSR            ;2
       LSR            ;2
       LSR            ;2
       LSR            ;2
       AND    #$03    ;2
       TAX            ;2
       LDA    LFF9E,X ;4
       TAY            ;2
       LDA    #$09    ;2
       LDX    #$0C    ;2
       BPL    LF7DD   ;2

; Audio volume 0
LF7DB: LDA    #0      ;2
; Begin sound effects routine
; Params:
;   A - volume
;   Y - frequency
;   X - noise content & additional freq
LF7DD: STX    AUDC0   ;3
       STY    AUDF0   ;3
       STA    AUDV0   ;3
       LDA    $99     ;3
       BNE    LF841   ;2
       LDY    $93     ;3
       LDA    $90     ;3
       BNE    LF7F4   ;2
       STA    AUDV1   ;3
       STA    $93     ;3
       JMP    LF875   ;3
LF7F4: DEC    $90     ;5
       CPY    #$02    ;2
       BNE    LF805   ;2
       LDX    #$06    ;2
       LDA    $90     ;3
       EOR    #$03    ;2
LF800: TAY            ;2
       LDA    #$08    ;2
       BNE    LF843   ;2
LF805: CPY    #$03    ;2
       BNE    LF80F   ;2
       LDX    #$02    ;2
LF80B: LDA    $90     ;3
       BPL    LF800   ;2
LF80F: CPY    #$04    ;2
       BNE    LF81B   ;2
       LDA    #$06    ;2
       LDX    #$08    ;2
       LDY    #$0F    ;2
       BNE    LF843   ;2
LF81B: CPY    #$05    ;2
       BNE    LF829   ;2
       LDA    $90     ;3
       EOR    #$03    ;2
LF823: LDY    #$08    ;2
       LDX    #$06    ;2
       BNE    LF843   ;2
LF829: CPY    #$06    ;2
       BNE    LF831   ;2
       LDA    $90     ;3
       BPL    LF823   ;2
LF831: CPY    #$07    ;2
       BNE    LF839   ;2
       LDX    #$04    ;2
       BNE    LF80B   ;2
LF839: CPY    #$08    ;2
       BNE    LF84A   ;2
       LDX    #$08    ;2
       BNE    LF80B   ;2
LF841: LDY    #0      ;2
LF843: STY    AUDV1   ;3
       STA    AUDF1   ;3
       STX    AUDC1   ;3
       RTS            ;6

LF84A: LDA    $90     ;3
       LSR            ;2
       LSR            ;2
       LSR            ;2
       LSR            ;2
       CPY    #$09    ;2
       BEQ    LF85A   ;2
       CPY    #$0A    ;2
       BNE    LF864   ;2
       EOR    #$03    ;2
LF85A: TAX            ;2
       LDA    LFFA2,X ;4
       LDX    #$06    ;2
       LDY    #$0A    ;2
       BNE    LF843   ;2
LF864: CPY    #$0B    ;2
       BNE    LF875   ;2
       LDA    $90     ;3
       TAY            ;2
       AND    #$04    ;2
       BEQ    LF841   ;2
       LDX    #$06    ;2
       LDA    #$01    ;2
       BNE    LF843   ;2
LF875: LDA    SWCHA   ;4
       EOR    #STKTOP ;2
       BEQ    LF841   ;2
       LDA    CLOCK   ;3
       AND    #$07    ;2
       CMP    #$03    ;2
       BCS    LF841   ;2
       LDY    #$0F    ;2
       LDX    #$0B    ;2
       LDA    #$18    ;2
       BNE    LF843   ;2
LF88C: CPX    $93     ;3
       BEQ    LF89F   ;2
       CPX    #$03    ;2
       BNE    LF898   ;2
       LDA    $93     ;3
       BNE    LF89F   ;2
LF898: STX    $93     ;3
       LDA    LF8A0,X ;4
       STA    $90     ;3
LF89F: RTS            ;6

LF8A0: .byte $FF,$FF,$15,$15,$01,$04,$04,$0A,$15,$3F,$3F,$1F
LF8AC: LDA    $99     ;3
       AND    #$FD    ;2
       STA    $99     ;3
       ROR            ;2
       BCC    LF8EC   ;2
       DEC    $80     ;5
       BNE    LF8EC   ;2
       LDA    #0      ;2
       STA    $99     ;3
       STA    $93     ;3
       LDA    LIVES   ;3
       BNE    LF8C6   ;2
       JSR    LF141   ;6
LF8C6: JSR    LF0E7   ;6
       STA    CXCLR   ;3
       LDA    $CA     ;3
       CMP    #$01    ;2
       BNE    LF8EB   ;2
       LDY    GAMEMODE;3
       CPY    #$06    ;2
       BCC    LF8EB   ;2
       LDX    $9A     ;3
       BMI    LF8EB   ;2
       LDA    $A6     ;3
       STA    $A0,X   ;4
       LDA    $B1     ;3
       STA    $AB,X   ;4
       LDA    $BC     ;3
       STA    $B6,X   ;4
       LDA    #STKTOP ;2
       STA    $9A     ;3
LF8EB: RTS            ;6

LF8EC: LDA    CLOCK   ;3
       AND    #$07    ;2
       STA    LINENO  ;3
       TAY            ;2
       LDA    #$88    ;2
       AND    MASK0,Y ;4
       STA    $DA     ;3
       LDA    #0      ;2
       STA    UNKNOWN3;3
       LDX    $EA     ;3
LF900: LDA    $A5,X   ;4
       CMP    FLOORNO ;3
       BNE    LF945   ;2
       LDA    $C5,X   ;4
       CMP    $9C     ;3
       BEQ    LF910   ;2
       CMP    $97     ;3
       BNE    LF945   ;2
LF910: INC    UNKNOWN3;5
       LDA    $80     ;3
       BNE    LF94C   ;2
       TXA            ;2
       BNE    LF91F   ;2
       LDA    GAMEMODE;3
       CMP    #$07    ;2
       BCS    LF925   ;2
LF91F: LDA    $9A     ;3
       CMP    #$01    ;2
       BEQ    LF94C   ;2
LF925: JSR    LFA6F   ;6
       BEQ    LF9A0   ;2
       LDA    POS_X   ;3
       SEC            ;2
       SBC    LFDEC,X ;4
       BCS    LF934   ;2
       LDA    POS_X   ;3
LF934: STA    $D0     ;3
       LDA    POS_Y   ;3
       SEC            ;2
       SBC    LFDF1,X ;4
       BCS    LF940   ;2
       LDA    POS_Y   ;3
LF940: STA    $D1     ;3
       JMP    LF983   ;3
LF945: JSR    LFA68   ;6
       BEQ    LF9A0   ;2
       BNE    LF950   ;2
LF94C: LDA    $DA     ;3
       BEQ    LF9A0   ;2
LF950: LDA    $C0,X   ;4
       AND    #$3F    ;2
       TAY            ;2
       AND    #$0F    ;2
       STA    $D0     ;3
       TYA            ;2
       LSR            ;2
       LSR            ;2
       LSR            ;2
       LSR            ;2
       TAY            ;2
       LDA    LFFA6,Y ;4
       CLC            ;2
       ADC    $D0     ;3
       TAY            ;2
       LDA    LFFAA,Y ;4
       STA    $D0     ;3
       LDA    LFFBD,Y ;4
       STA    $D1     ;3
       CMP    $BB,X   ;4
       BNE    LF983   ;2
       LDA    $B0,X   ;4
       CMP    $D0     ;3
       BNE    LF983   ;2
       LDA    $95     ;3
       ORA    MASK0,X ;4
       STA    $95     ;3
       BNE    LF9A0   ;2
LF983: LDA    $B0,X   ;4
       CMP    $D0     ;3
       BCC    LF98F   ;2
       BEQ    LF991   ;2
       DEC    $B0,X   ;6
       BNE    LF991   ;2
LF98F: INC    $B0,X   ;6
LF991: LDA    $BB,X   ;4
       CMP    $D1     ;3
       BCC    LF99E   ;2
       BEQ    LF9A0   ;2
       DEC    $BB,X   ;6
       JMP    LF9A0   ;3
LF99E: INC    $BB,X   ;6
LF9A0: DEX            ;2
       BMI    LF9A6   ;2
       JMP    LF900   ;3
LF9A6: LDX    #0      ;2
       LDA    $95     ;3
LF9AA: ROR            ;2
       BCS    LF9B5   ;2
       INX            ;2
       CPX    $EA     ;3
       BEQ    LF9AA   ;2
       BCC    LF9AA   ;2
       RTS            ;6

LF9B5: LDA    $95     ;3
       EOR    MASK0,X ;4
       STA    $95     ;3
       LDA    $C0,X   ;4
       TAY            ;2
       AND    #$30    ;2
       BEQ    LF9F3   ;2
       CMP    #$10    ;2
       BNE    LF9DB   ;2
       TYA            ;2
       AND    #$C0    ;2
       STA    $D0     ;3
       CLC            ;2
       ROL            ;2 rotate 1 bit left
       ROL            ;2
       ROL            ;2
       TAY            ;2
       LDA    LFFD0,Y ;4
       CLC            ;2
       ADC    $C5,X   ;4
       STA    $C5,X   ;4
       BPL    LF9EC   ;2
LF9DB: CMP    #$30    ;2
       BEQ    LF9E3   ;2
       DEC    $A5,X   ;6
       BPL    LF9E5   ;2
LF9E3: INC    $A5,X   ;6
LF9E5: TYA            ;2
       AND    #$C0    ;2
       EOR    #$40    ;2
       STA    $D0     ;3
LF9EC: LDA    $C5,X   ;4
       ORA    $D0     ;3
       STA    $C0,X   ;4
       RTS            ;6

LF9F3: TYA            ;2
       ROL            ;2
       ROL            ;2
       ROL            ;2
       AND    #$03    ;2
       EOR    #$01    ;2
       STA    $D4     ;3
       LDA    $EB     ;3
       AND    #$03    ;2
       STA    $D3     ;3
       STA    $D2     ;3
       BPL    LFA13   ;2
LFA07: INC    $D2     ;5
       LDA    $D2     ;3
       AND    #$03    ;2
       STA    $D2     ;3
       CMP    $D3     ;3
       BEQ    LFA1D   ;2
LFA13: CMP    $D4     ;3
       BEQ    LFA07   ;2
       JSR    LFA1F   ;6
       BEQ    LFA07   ;2
       RTS            ;6

LFA1D: LDA    $D4     ;3
LFA1F: STA    $D6     ;3
       LDA    $A5,X   ;4
       STA    $D7     ;3
       LDA    $C5,X   ;4
       STA    $D0     ;3
       JSR    LFDAE   ;6
       BMI    LFA41   ;2
       BEQ    LFA39   ;2
       LDA    GAMEMODE;3
       CMP    #$08    ;2
       BEQ    LFA39   ;2
       TXA            ;2
       BNE    RET1    ;2
LFA39: LDA    $D8     ;3
       STA    $D0     ;3
       LDA    #$01    ;2
       BNE    LFA56   ;2
LFA41: CMP    #STKTOP ;2
       BEQ    RET1    ;2
       LDY    $A5,X   ;4
       LDA    $C5,X   ;4
       JSR    LFD4C   ;6
       BCS    RET1    ;2
       BNE    LFA54   ;2
       LDA    #$02    ;2
       BNE    LFA56   ;2
LFA54: LDA    #$03    ;2
LFA56: ASL    $D6     ;5
       ASL    $D6     ;5
       ORA    $D6     ;3
       ASL            ;2
       ASL            ;2
       ASL            ;2
       ASL            ;2
       ORA    $D0     ;3
       STA    $C0,X   ;4
       RTS            ;6

; Exit subroutine, sets return val to zero
;LFA65:
RET1:  LDA    #0      ;2
       RTS            ;6

LFA68: LDA    $94     ;3
       AND    MASK0,X ;4
       BEQ    LFA83   ;2
LFA6F: LDA    GAMEMODE;3
       CMP    #$07    ;2
       BCC    LFA7A   ;2
       LDA    LFA8B,X ;4
       BNE    LFA7D   ;2
LFA7A: LDA    LFA86,X ;4
LFA7D: LDY    LINENO  ;3
       AND    MASK0,Y ;4
       RTS            ;6

LFA83: LDA    $DA     ;3
       RTS            ;6

LFA86: .byte $AA,$91,$88,$88,$88
LFA8B: .byte $AA,$AA,$91,$91,$91

; collision detection subroutine
;LFA90:
COLLIDE0:
       LDA    $80     ;3
       BNE    RET1    ;2
       BIT    CXPPMM  ;3 collision detect with missile
       BPL    RET1    ;2 jump if no collision (neg bit not set)
       LDA    $81     ;3
       BNE    RET1    ;2
       BIT    $8A     ;3
       BVC    LFAA3   ;2
       BPL    LFAAC   ;2
       RTS            ;6

LFAA3: LDA    $85     ;3
       CMP    $EA     ;3
       BEQ    LFB0C   ;2
       BCC    LFB0C   ;2
       RTS            ;6

LFAAC: LDX    #$06    ;2
       JSR    LF88C   ;6
       LDA    $9A     ;3
       BMI    LFAFE   ;2
       CMP    $86     ;3
       BEQ    LFAFE   ;2
       CMP    #$02    ;2
       BCC    LFAFE   ;2
       LDA    $86     ;3
       CMP    #$02    ;2
       BCC    LFAFE   ;2
       CLC            ;2
       ADC    $9A     ;3
       TAY            ;2
       LDX    #$07    ;2
       JSR    LF88C   ;6
       LDA    $9E     ;3
       ORA    $9F     ;3
       BPL    LFAE5   ;2
       LDX    #$0B    ;2
       JSR    LF88C   ;6
       LDA    #$08    ;2
       STA    $9D     ;3
       LDA    #STKTOP ;2
       STA    $9E     ;3
       STA    $9F     ;3
       LDA    #$02    ;2
       BNE    LFB00   ;2
LFAE5: LDX    #$02    ;2
       CPY    #$05    ;2
       BNE    LFAEC   ;2
       DEX            ;2
LFAEC: LDA    #STKTOP ;2
       STA    $9D,X   ;4
       LDX    #0      ;2
       CPY    #$07    ;2
       BNE    LFAF7   ;2
       INX            ;2
LFAF7: STY    $9D,X   ;4
       INX            ;2
       INX            ;2
       TXA            ;2
       BNE    LFB00   ;2
LFAFE: LDA    $86     ;3
LFB00: PHA            ;3
       BIT    $9A     ;3
       BMI    LFB08   ;2
       JSR    LFBE2   ;6
LFB08: PLA            ;4
       STA    $9A     ;3
LFB0B: RTS            ;6

LFB0C: LDA    GAMEMODE;3
       CMP    #$07    ;2
       BCC    LFB16   ;2
       LDA    $85     ;3
       BEQ    LFB1C   ;2
LFB16: LDA    $9A     ;3
       CMP    #$01    ;2
       BEQ    LFB0B   ;2
LFB1C: LDX    #$01    ;2
       STX    $99     ;3
       LDX    #STKTOP ;2
       STX    $80     ;3
       DEC    LIVES   ;5
       LDA    $85     ;3
       STA    $CA     ;3
       RTS            ;6

LFB2B: LDA    GAMEMODE;3
       CMP    #$05    ;2
       BCC    INPUT0  ;2
       LDA    $9B     ;3
       AND    #$07    ;2
       ASL            ;2
       ASL            ;2
       STA    $D0     ;3
       ASL            ;2
       ASL            ;2
       ASL            ;2
       ASL            ;2
       ORA    $D0     ;3
       AND    #$D0    ;2
       ORA    #$20    ;2
       ORA    $9C     ;3
       STA    $D0     ;3
LFB47: LDX    $EA     ;3
LFB49: LDA    #0      ;2
       STA    $94     ;3
LFB4D: LDA    $A5,X   ;4
       CMP    FLOORNO ;3
       BNE    LFB64   ;2
       LDA    $C5,X   ;4
       CMP    $9C     ;3
       BNE    LFB64   ;2
       LDA    $D0     ;3
       STA    $C0,X   ;4
       LDA    $94     ;3
       ORA    MASK0,X ;4
       STA    $94     ;3
LFB64: DEX            ;2
       BPL    LFB4D   ;2
       RTS            ;6

; This is some kind of an input processing subroutine
LFB68: LDA    $9B     ;3
       AND    #$0F    ;2
       CMP    #$0F    ;2
       BEQ    INPUT0  ;2
       BIT    $9B     ;3
       BPL    LFB90   ;2
       BVS    INPUT0  ;2
       JSR    LFB2B   ;6
       LDX    #$09    ;2
       LDA    $9B     ;3
       AND    #$04    ;2
       BEQ    LFB85   ;2
       INC    FLOORNO ;5
       BNE    LFB88   ;2
LFB85: DEC    FLOORNO ;5
       INX            ;2
LFB88: JSR    LF88C   ;6
       JSR    LFD0A   ;6
       ORA    #$40    ;2
LFB90: STA    $9B     ;3

; Torch input control (contains only reference to INPT4 reg)
;LFB92:
INPUT0:
       LDA    SWCHA   ;4 joystick movement state to reg A
                      ;  7Fh = right, BFh = left, EFh = up, DFh = down
                      ;  Bottom 4 bits 1, assuming no second control mvmts
       AND    #$F0    ;2 zeros out joystick 4 bits (lower 4)
       EOR    #$F0    ;2 since the register encodes "no movement" as ones
                      ;  we invert that encoding here to 1 means move
                      ;  in a direction
       STA    MVMT    ;3 RAM MVMT ($8E) holds movement bits, where 1=pressed
                      ;  this is the opposite of the usual SWCHA format
       LDA    UNKNOWN3;3
       BNE    LFBD4   ;2 ?? jump if we're close to an enemy?
       LDA    INPT4   ;3 A=joystick button state ($bc, 10111100 if pressed)
       ROL            ;2 rotate left w/ wrap around ($79, 01111001)
       ROR    $E6     ;5 rotate right, RAM E6 (previous button press state?)
       LDA    $E6     ;3 load RAM E6 into A
       CMP    #$7F    ;2 compare result with 7F (0111 1111)
       BNE    LFBD4   ;2 jump if not equal
       BIT    $8A     ;3
       BVS    LFBC2   ;2
       LDA    #$14    ;2
       STA    $DF     ;3
       SED            ;2
       LDA    $82     ;3
       CLC            ;2
       ADC    #$01    ;2
       STA    $82     ;3
       CLD            ;2
       LDA    $8A     ;3
       ORA    #$40    ;2
       STA    $8A     ;3
       RTS            ;6

LFBC2: BIT    $9A     ;3
       BMI    LFBD4   ;2
       LDX    #$05    ;2
       JSR    LF88C   ;6
       JSR    LFBE2   ;6
       LDA    #STKTOP ;2
       STA    $9A     ;3
       BNE    RET0    ;2

LFBD4: LDA    $81     ;3 move RAM 81 into A
       BEQ    RET0    ;2 if A == 0, return
       BIT    CXP0FB  ;3
       BMI    LFBFB   ;2
       LDA    #0      ;2
       STA    $81     ;3
       BEQ    RET0    ;2
LFBE2: LDA    #$05    ;2
       STA    $81     ;3
       LDA    $9A     ;3
       STA    $E9     ;3
       LDA    $E7     ;3
       TAY            ;2
       ORA    $E8     ;3
       BEQ    LFBFB   ;2
       INY            ;2
       INY            ;2
       LDX    $E8     ;3
       INX            ;2
       JSR    LFC0B   ;6
       BCC    RET0    ;2
LFBFB: DEC    $81     ;5
       BEQ    RET0    ;2
       LDX    $81     ;3
       DEX            ;2
       LDY    $81     ;3
       DEY            ;2
       JSR    LFC0B   ;6
       BCS    LFBFB   ;2
;LFC0A:
RET0: RTS             ;6

LFC0B: LDA    LFD03,X ;4
       LDX    POS_Y   ;3
       CPX    #$F4    ;2
       BCC    LFC18   ;2
       CMP    #$0A    ;2
       BEQ    LFC3A   ;2
LFC18: CLC            ;2
       ADC    POS_Y   ;3
       CMP    #$F4    ;2
       BCS    LFC3A   ;2
       LDX    $E9     ;3
       STA    $B6,X   ;4
       JSR    LF4DB   ;6
       STA    $EF     ;3
       LDA    LFD02,Y ;4
       CLC            ;2
       ADC    POS_X   ;3
       CMP    #$97    ;2
       BCS    LFC3A   ;2
       LDX    $E9     ;3
       STA    $AB,X   ;4
       STA    $DC     ;3
       CLC            ;2
       RTS            ;6

LFC3A: SEC            ;2
       RTS            ;6

;; This subroutine runs when the lower, display chunk
;; of the screen is being drawn. It loops five times.
; Initialize: $D0 = 5, X = 0, for LFC42
LFC3C: LDA    #$05    ;2
       STA    $D0     ;3 initialize RAM $D0 = 5
       LDX    #0      ;2 X = 0
; Movement subroutine check. RAM MVMT ($8E) data masks are
; stored in DATA0, which are then used to pull data from DATA1
; RAM $D0 is a param here, specifying an offset in DATA0
LFC42: LDY    $D0     ;3 Y = RAM $D0 (initialized to 5)
       LDA    DATA0,Y ;4 A = DATA0[Y]
       TAY            ;2 move data to Y
       LDA    MVMT    ;3 (load eye ptr base?)
       AND    DATA1,Y ;4 AND A with DATA1[Y]
       BEQ    LFC61   ;2 jump if above result is zero
       LDA    POS_X   ;3
       CPY    #$02    ;2
       BCC    LFC57   ;2
       LDA    POS_Y   ;3
LFC57: CMP    LFC68,X ;4
       BEQ    LFCA5   ;2
       CMP    LFC69,X ;4
       BEQ    LFC7E   ;2
LFC61: INX            ;2 increment X twice (w/ below)
       INX            ;2
       DEC    $D0     ;5 decrement $D0
       BPL    LFC42   ;2
LFC67: RTS            ;6

LFC68: .byte $54
LFC69: .byte $5C,$A4,$AC,$58,$4F,$A8,$9F,$4F,$47,$48,$50
;LFC74:
DATA0: .byte $01,$00,$03,$03,$02,$02
;DATA1:
DATA1: .byte $40,$80,$10,$20
LFC7E: BIT    $88     ;3
       BPL    LFC67   ;2
       TYA            ;2
       ORA    #$80    ;2
       CMP    $88     ;3
       BEQ    LFC8D   ;2
       LDA    $97     ;3
       STA    $9C     ;3
LFC8D: JSR    LFCD8   ;6
       STY    $88     ;3
       LDA    LFFD0,Y ;4
       CLC            ;2
       ADC    $9C     ;3
       STA    $9C     ;3
       LDA    #STKTOP ;2
       STA    $97     ;3
       JSR    LFD0A   ;6
       LDX    #$08    ;2
       BNE    LFCD1   ;2
LFCA5: BIT    $88     ;3
       BMI    LFC67   ;2
       TYA            ;2
       JSR    LFDA6   ;6
       BEQ    LFCB3   ;2
       LDA    $9A     ;3
       BNE    LFCC6   ;2
LFCB3: LDY    $D6     ;3
       LDA    LFFD0,Y ;4
       CLC            ;2
       ADC    $9C     ;3
       STA    $97     ;3
       TYA            ;2
       ORA    #$80    ;2
       STA    $88     ;3
       LDX    #$04    ;2
       BNE    LFCD1   ;2

; a draw sprite routine. this contains the only
; reference to the main block of data containing
; the various item and monster sprites
LFCC6: LDA    $D6     ;3
       LSR            ;2
       TAX            ;2
       LDA    SPRITES0,X ;4
       STA    $91     ;3
       LDX    #$02    ;2
LFCD1: LDA    GAMEMODE;3
       BEQ    LFC67   ;2
       JMP    LF88C   ;3
LFCD8: STY    $D5     ;3
       LDA    GAMEMODE;3
       CMP    #$05    ;2
       BCC    LFCFF   ;2
       LDA    LFD06,Y ;4
       ORA    #$10    ;2
       STA    $D0     ;3
       TYA            ;2
       JSR    LFDA6   ;6
       PHA            ;3
       LDA    $D0     ;3
       ORA    $D8     ;3
       STA    $D0     ;3
       PLA            ;4
       BEQ    LFCFC   ;2
       LDX    #0      ;2
       JSR    LFB49   ;6
       BMI    LFCFF   ;2
LFCFC: JSR    LFB47   ;6
LFCFF: LDY    $D5     ;3
       RTS            ;6

LFD02: .byte $00
LFD03: .byte $0A,$00,$F6
LFD06: .byte $00,$40,$80,$C0
LFD0A: LDA    #$02    ;2
       JSR    LFD6C   ;6
       STA    $8B     ;3
       LDA    #$03    ;2
       JSR    LFD6C   ;6
       STA    $8C     ;3
       LDA    #0      ;2
       JSR    LFD6C   ;6
       BEQ    LFD25   ;2
       LDA    #$47    ;2
       LDX    #$3F    ;2
       BNE    LFD29   ;2
LFD25: LDA    #$4F    ;2
       LDX    #$57    ;2
LFD29: STA    $CF     ;3
       STX    $DE     ;3
       LDY    FLOORNO ;3
       LDA    $9C     ;3
       JSR    LFD4C   ;6
       BCS    LFD42   ;2
       BEQ    LFD3A   ;2
       LDA    #$04    ;2
LFD3A: LDX    $9C     ;3
       ORA    LFD46,X ;4
LFD3F: STA    $9B     ;3
       RTS            ;6

LFD42: LDA    #$0F    ;2
       BNE    LFD3F   ;2
LFD46: .byte $03 ;.SLO;8
       .byte $03 ;.SLO;8
       ORA    ($00,X) ;6
       .byte $02 ;.JAM;0
       .byte $02 ;.JAM;0
LFD4C: STA    $D8     ;3
       TYA            ;2
       JSR    LFDCA   ;6
       STY    $D7     ;3
       LDA    LFDDD,Y ;4
       LDY    $D8     ;3
       AND    MASK0,Y ;4
       BEQ    LFD6A   ;2
       LDY    $D7     ;3
       LDA    LFDE5,Y ;4
       LDY    $D8     ;3
       CLC            ;2
       AND    MASK0,Y ;4
       RTS            ;6

LFD6A: SEC            ;2
       RTS            ;6

LFD6C: STA    $D6     ;3
       LDA    FLOORNO ;3
       STA    $D7     ;3
       LDA    $9C     ;3
       JSR    LFD9A   ;6
       BMI    LFDC7   ;2
       LSR            ;2
       TAY            ;2
       LDA    LFD7F,Y ;4
       RTS            ;6

LFD7F: .byte $57,$A7,$4F
LFD82: .byte $04,$FF,$00,$80,$FF,$04,$01,$81,$05,$82,$02,$00,$83,$05,$03,$01
       .byte $06,$FF,$84,$02,$FF,$06,$85,$03
LFD9A: ASL            ;2
       ASL            ;2
       CLC            ;2
       ADC    $D6     ;3
       TAY            ;2
       LDA    LFD82,Y ;4
       STA    $D8     ;3
       RTS            ;6

LFDA6: STA    $D6     ;3
       LDA    FLOORNO ;3
       STA    $D7     ;3
       LDA    $9C     ;3
LFDAE: JSR    LFD9A   ;6
       BMI    LFDC9   ;2
       LDY    GAMEMODE;3
       CPY    #$02    ;2
       BCC    LFDC7   ;2
       LDA    $D7     ;3
       JSR    LFDCA   ;6
       LDA    LFDD5,Y ;4
       LDY    $D8     ;3
       AND    MASK0,Y ;4
       RTS            ;6

LFDC7: LDA    #0      ;2
LFDC9: RTS            ;6

LFDCA: LDY    GAMEMODE;3
       CPY    #$08    ;2
       BNE    LFDD3   ;2
       CLC            ;2
       ADC    #$04    ;2
LFDD3: TAY            ;2
       RTS            ;6

; 0dd5 |  X X  X|
; 0dd6 | X   XXX|
; 0dd7 |  XX X  |
; 0dd8 |  X X  X|
; 0dd9 |  X XXX |
; 0dda |   XXX X|
; 0ddb |  X X XX|
; 0ddc |    X X |
LFDD5: .byte $29,$47,$34,$29,$2E,$1D,$2B,$0A

; 0ddd |  X    X|
; 0dde |  XXX XX|
; 0ddf |  XXXXXX|
; 0de0 |  X  X X|
; 0de1 |  X X X |
; 0de2 |  XXXXXX|
; 0de3 |  XXXXXX|
; 0de4 |  X X X |
LFDDD: .byte $21,$3B,$3F,$25,$2A,$3F,$3F,$2A

; 0de5 |  X    X|
; 0de6 |   XX X |
; 0de7 |  X  X X|
; 0de8 |        |
; 0de9 |  X X X |
; 0dea |   X X X|
; 0deb |  X X X |
LFDE5: .byte $21,$1A,$25,$00,$2A,$15,$2A

; 0dec |        |
; 0ded |XXXXX X |
; 0dee |XXXXX X |
; 0def |     XX |
; 0df0 |     XX |
LFDEC: .byte $00,$FA,$FA,$06,$06

; 0df1 |        |
; 0df2 |XXXXXXX |
; 0df3 |     XX |
; 0df4 |XXXXXXX |
; 0df5 |     XX |
LFDF1: .byte $00,$FE,$06,$FE,$06

;0df1 |        |
;0df2 |XXXXXXX |
;0df3 |     XX |
;0df4 |XXXXXXX |
;0df5 |     XX |
;LFDF6:
DATA2: .byte $09,$FF,$7F,$02,$FF,$0F,$02,$02,$03,$04

; This has something to do with the scan line
; playfield floorplan rendering routine. possible
; used as a bitmask for selecting the proper
; scanline <-> floorplan sprite mapping?
;
; Another note: since the floorplan scrolls with the
; user (the whole room isn't displayed at once) only
; a part of it is showing. It looks like only five
; blocks of the floorplan (see FLOOR0 data) are rendered
; on-screen at a time and a last (non-floor one) reserved
; for the score and collected items. So that would explain
; why there are six 1 bits set here here.
;
;0e00 |       X|
;0e01 |      X |
;0e02 |     X  |
;0e03 |    X   |
;0e04 |   X    |
;0e05 |  X     |
;LFE00:
MASK0: .byte $01,$02,$04,$08,$10,$20

;0e06 | X      |
;0e07 |X       |
;0e08 |        |
;0e09 |        |
;0e0a |     XXX|
;0e0b |XXXXXX X|
;0e0c |X X  XXX|
;0e0d |        |
;0e0e |        |
;0e0f |        |
;0e10 |       X|
;0e11 |      X |
;0e12 |     X  |
;0e13 |  X X   |
;0e14 |X  X    |
;0e15 | XX X   |
;0e16 | XX     |
;0e17 |X  X    |
;0e18 |        |
;0e19 |        |
;0e1a |  X     |
;0e1b |XXXX    |
;0e1c |X X     |
;0e1d |XXX     |
;0e1e |        |
;0e1f |        |
;0e20 |  XXXX  |
;0e21 |   XX   |
;0e22 |   XX   |
;0e23 |    X   |
;0e24 |   XX   |
;0e25 |   X    |
;0e26 |  XXXX  |
;0e27 |   XX   |
;0e28 |        |
;0e29 |        |
;0e2a |     X  |
;0e2b |     XXX|
;0e2c |     XX |
;0e2d |    XXXX|
;0e2e |        |
;0e2f |        |
;0e30 |  XXXX  |
;0e31 |   XX   |
;0e32 |  XXX   |
;0e33 |XXXXX   |
;0e34 |X XXX   |
;0e35 |XXXX    |
;0e36 |  XXXX  |
;0e37 |   XX   |
;0e38 |        |
;0e39 |        |
;0e3a |  X  X  |
;0e3b |XXXX XXX|
;0e3c |X X  X X|
;0e3d |XXX XXXX|
;0e3e |        |
;0e3f |        |
;0e40 |  XXXX  |
;0e41 |   XX   |
;0e42 |   XXX  |
;0e43 |    XXXX|
;0e44 |   XXX X|
;0e45 |   XXXXX|
;0e46 |  XXXX  |
;0e47 |   XX   |

;0e48 |  XXXX  |
;0e49 |   XX   |
;0e4a |  XXXX  |
;0e4b |XXXXXXXX|
;0e4c |X XXXX X|
;0e4d |XXXXXXXX|
;0e4e |  XXXX  |
;0e4f |   XX   |
;0e50 |        |
;0e51 |        |
;0e52 |        |
;0e53 |    XX  |
;0e54 |  XXXX  |
;0e55 | XXXXX  |
;0e56 | XXXXX X|
;0e57 | XXXXXX |
;0e58 |XX X X  |
;0e59 |X XXX   |
;0e5a |        |
;0e5b |        |
;0e5c |XXX     |
;0e5d | XXXX   |
;0e5e |  XXXX  |
;0e5f |  XXXXX |
;0e60 |  XXXXX |
;0e61 |X XXXXX |
;0e62 | XX X XX|
;0e63 |   XXX X|

;0e64 |X      X|
;0e65 |X      X|
;0e66 |XX    XX|
;0e67 | XX  XX |
;0e68 | XXXXXX |
;0e69 |  XXXX  |
;0e6a |        |
;0e6b |        |
;0e6c |        |
;0e6d |        |
;0e6e |        |
;0e6f |        |
;0e70 |        |
;0e71 |        |
;0e72 |  XXXX  |
;0e73 | XXXXXX |
;0e74 | XX  XX |
;0e75 |XX    XX|
;0e76 |X      X|
;0e77 |X      X|

;0e78 |  X  X  |
;0e79 |  X  X  |
;0e7a |  X  X  |
;0e7b |X  XX  X|
;0e7c | XXXXXX |
;0e7d |   XX   |
;0e7e | XX  XX |
;0e7f |X      X|
;0e80 |        |
;0e81 |        |
;0e82 |        |
;0e83 |        |
;0e84 |XXX  XXX|
;0e85 |   XX   |
;0e86 | XXXXXX |
;0e87 |X  XX  X|
;0e88 |  X  X  |
;0e89 | X    X |
;0e8a | X    X |
;0e8b |        |
;0e8c |X XX XXX|
;0e8d |X XX XXX|
;0e8e |X XX XXX|
;0e8f |X XX XXX|
;0e90 |X XX XXX|
;0e91 |X XX XXX|
;0e92 |X XX XXX|
;0e93 |X XX XXX|
;0e94 |X XX XXX|
;0e95 |X XX XXX|
;0e96 |X XX XXX|
;0e97 |X XX XXX|
;0e98 |X XX XXX|
;0e99 |X XX XXX|
;0e9a |X XX XXX|
;0e9b |X XX XXX|
;0e9c |X XX XXX|
;0e9d |XXX XX X|
;0e9e |XXX XX X|
;0e9f |XXX XX X|
;0ea0 |XXX XX X|
;0ea1 |XXX XX X|
;0ea2 |XXX XX X|
;0ea3 |XXX XX X|
;0ea4 |XXX XX X|
;0ea5 |XXX XX X|
;0ea6 |XXX XX X|
;0ea7 |XXX XX X|
;0ea8 |XXX XX X|
;0ea9 |XXX XX X|
;0eaa |XXX XX X|
;0eab |XXX XX X|
;0eac |XXX XX X|
;0ead |XXX XX X|
;0eae |XXXXXXXX|
;0eaf |XXXXXXXX|
;0eb0 |XXXXXXXX|
;0eb1 |XXXXXXXX|
;0eb2 |XXXXXXXX|
;0eb3 |XXXXXXXX|
;0eb4 |        |
;0eb5 |        |
;0eb6 |XXXXXXXX|
;0eb7 |XXXXXXXX|
;0eb8 |XXXXXXXX|
;0eb9 |XXXXXXXX|
;0eba |        |
;0ebb |        |
;0ebc |XXXXXXXX|
;0ebd |XXXXXXXX|
;0ebe |        |
;0ebf |XXXXXXXX|
;0ec0 |XXXXXXXX|
;0ec1 |        |
;0ec2 |        |
;0ec3 |XXXXXXXX|
;0ec4 |XXXXXXXX|
;0ec5 |XXXXXXXX|
;0ec6 |XXXXXXXX|
;0ec7 |        |
;0ec8 |        |
;0ec9 |XXXXXXXX|
;0eca |XXXXXXXX|
;0ecb |XXXXXXXX|
;0ecc |XXXXXXXX|
;0ecd |XXXXXXXX|
;0ece |XXXXXXXX|
;0ecf |        |
;LFE06:
SPRITES0:
       .byte $40,$80,$00,$00,$07,$FD,$A7,$00,$00,$00,$01,$02,$04,$28,$90,$68
       .byte $60,$90,$00,$00,$20,$F0,$A0,$E0,$00,$00,$3C,$18,$18,$08,$18,$10
       .byte $3C,$18,$00,$00,$04,$07,$06,$0F,$00,$00,$3C,$18,$38,$F8,$B8,$F0
       .byte $3C,$18,$00,$00,$24,$F7,$A5,$EF,$00,$00,$3C,$18,$1C,$0F,$1D,$1F
       .byte $3C,$18,$3C,$18,$3C,$FF,$BD,$FF,$3C,$18,$00,$00,$00,$0C,$3C,$7C
       .byte $7D,$7E,$D4,$B8,$00,$00,$E0,$78,$3C,$3E,$3E,$BE,$6B,$1D,$81,$81
       .byte $C3,$66,$7E,$3C,$00,$00,$00,$00,$00,$00,$00,$00,$3C,$7E,$66,$C3
       .byte $81,$81,$24,$24,$24,$99,$7E,$18,$66,$81,$00,$00,$00,$00,$E7,$18
       .byte $7E,$99,$24,$42,$42,$00,$B7,$B7,$B7,$B7,$B7,$B7,$B7,$B7,$B7,$B7
       .byte $B7,$B7,$B7,$B7,$B7,$B7,$B7,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED
       .byte $ED,$ED,$ED,$ED,$ED,$ED,$ED,$ED,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00
       .byte $FF,$FF,$FF,$FF,$00,$00,$FF,$FF,$00,$FF,$FF,$00,$00,$FF,$FF,$FF
       .byte $FF,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$00

; These top blocks are the playfield. They're organized in groups
; of three, PF0, PF1, PF2. Indexed from LF647 by X. We have three ROM
; references into this block, one for each of the three pieces of the
; playfield we draw.
;0ed0 |XXXXXXXX|
;0ed1 |XXXX    |
;0ed2 |XXXXXXXX|
;
;0ed3 |XXXXXXXX|
;0ed4 |        |
;0ed5 |XXXX    |
;
;0ed6 |XXXXXXXX|
;0ed7 |        |
;0ed8 |XXXX    |
;
;0ed9 |XXXXXXXX|
;0eda |        |
;0edb |        |
;
;0edc |XXXXXXXX|
;0edd |        |
;0ede |XXXX    |
;
;0edf |XXXXXXXX|
;0ee0 |XXXX    |
;0ee1 |XXXXXXXX|
;
;0ee2 |XXXXXXXX|
;0ee3 |        |
;0ee4 |XXXX    |
; Thinnest level walls (side doors)
;0ee5 |   X    |
;0ee6 |        |
;0ee7 |        |
; ...
;0ee8 |XXXXXXXX|
;0ee9 |        |
;0eea |XXXX    |
;0eeb |XXXXXXXX|
;0eec |        |
;0eed |XXXX    |
;0eee |XXXXXXXX|
;0eef |XXXX    |
;0ef0 |XXXXXXXX|
;0ef1 |XXXXXXXX|
;0ef2 |        |
;0ef3 |XXXX    |
;0ef4 |XXXXXXXX|
;0ef5 |        |
;0ef6 |XXXX    |
;0ef7 |XXXXXXXX|
;0ef8 |        |
;0ef9 |        |
;0efa |XXXXXXXX|
;0efb |        |
;0efc |XXXX    |
;0efd |XXXXXXXX|
;0efe |XXXX    |
;0eff |XXXXXXXX|

;0f00 |   XXX  |
;0f01 |  XXXXX |
;0f02 | XXX  XX|
;0f03 | XX   XX|
;0f04 | XX   XX|
;0f05 | XX  XXX|
;0f06 |  XXXXX |
;0f07 |   XXX  |
;0f08 |  XXXXX |
;0f09 |   XXX  |
;0f0a |    XX  |
;0f0b |    XX  |
;0f0c |    XX  |
;0f0d |  XXXX  |
;0f0e |   XXX  |
;0f0f |    XX  |
;0f10 | XXXXXXX|
;0f11 | XX     |
;0f12 | XXX    |
;0f13 |  XXXXX |
;0f14 |      XX|
;0f15 |  XX  XX|
;0f16 | XX  XXX|
;0f17 |  XXXXX |
;0f18 |  XXXXX |
;0f19 | XX  XXX|
;0f1a |  XX  XX|
;0f1b |     XX |
;0f1c |    XX  |
;0f1d |     XXX|
;0f1e |  XX  XX|
;0f1f |   XXXX |
;0f20 |    XXXX|
;0f21 |     XX |
;0f22 |XXXXXXXX|
;0f23 |XX   XX |
;0f24 | XX  XX |
;0f25 | XX  XX |
;0f26 |XXX  XX |
;0f27 |     XXX|
;0f28 |  XXXXX |
;0f29 | XX  XXX|
;0f2a |  XX  XX|
;0f2b |      XX|
;0f2c |  XXXXX |
;0f2d |  XX    |
;0f2e |  XXX   |
;0f2f |   XXXXX|
;0f30 |  XXXXX |
;0f31 | XXX  XX|
;0f32 | XX  XXX|
;0f33 | XXXXXX |
;0f34 | XX     |
;0f35 | XX  XXX|
;0f36 |  XX  XX|
;0f37 |   XXXX |
;0f38 |  XXXX  |
;0f39 |   XX   |
;0f3a |    XX  |
;0f3b |    XX  |
;0f3c |     XX |
;0f3d |     XX |
;0f3e |XX    XX|
;0f3f | XXXXXXX|
;0f40 |  XXXXX |
;0f41 | XX  XXX|
;0f42 |XX    XX|
;0f43 |XX  XXXX|
;0f44 | XXXXXX |
;0f45 | XXX  XX|
;0f46 | XX  XX |
;0f47 |  XXXX  |
;0f48 |  XXXX  |
;0f49 | XX  XX |
;0f4a |      XX|
;0f4b |  XXXXXX|
;0f4c | XXX  XX|
;0f4d | XX   XX|
;0f4e | XX  XX |
;0f4f |  XXXX  |

; Big torch
; Displays 2/3 of torch draws
;0f50 |  XXXX  |
;0f51 |  XXXX  |
;0f52 |  XXXX  |
;0f53 |  XXXX  |
;0f54 | XXXXXX |
;0f55 | XXXXXX |
;0f56 | XXXXXX |
;0f57 | XXXXXX |
;0f58 |XXXXXXXX|
;0f59 |XXXXXXXX|
;0f5a |XXXXXXXX|
;0f5b |XXXXXXXX|
;0f5c |XXXXXXXX|
;0f5d |XXXXXXXX|
;0f5e |XXXXXXXX|
;0f5f |XXXXXXXX|
;0f60 |XXXXXXXX|
;0f61 |XXXXXXXX|
;0f62 |XXXXXXXX|
;0f63 |XXXXXXXX|
;0f64 | XXXXXX |
;0f65 | XXXXXX |
;0f66 | XXXXXX |
;0f67 | XXXXXX |
;0f68 |  XXXX  |
;0f69 |  XXXX  |
;0f6a |  XXXX  |
;0f6b |  XXXX  |

; Little torch
; Displays 1/3 of torch draws
;0f6c |   XX   |
;0f6d |   XX   |
;0f6e |   XX   |
;0f6f |   XX   |
;0f70 |  XXXX  |
;0f71 |  XXXX  |
;0f72 |  XXXX  |
;0f73 |  XXXX  |
;0f74 | XXXXXX |
;0f75 | XXXXXX |
;0f76 | XXXXXX |
;0f77 | XXXXXX |
;0f78 |XXXXXXXX|
;0f79 |XXXXXXXX|
;0f7a |XXXXXXXX|
;0f7b |XXXXXXXX|
;0f7c | XXXXXX |
;0f7d | XXXXXX |
;0f7e | XXXXXX |
;0f7f | XXXXXX |
;0f80 |  XXXX  |
;0f81 |  XXXX  |
;0f82 |  XXXX  |
;0f83 |  XXXX  |
;0f84 |   XX   |
;0f85 |   XX   |
;0f86 |   XX   |
;0f87 |   XX   |
;LFED0
GFX0: .byte $FF
;LFED1
GFX1: .byte $F0
;LFED2:
GFX2:
       .byte $FF,$FF,$00,$F0,$FF,$00,$F0,$FF,$00,$00,$FF,$00,$F0,$FF,$F0,$FF
       .byte $FF,$00,$F0,$10,$00,$00,$FF,$00,$F0,$FF,$00,$F0,$FF,$F0,$FF,$FF
       .byte $00,$F0,$FF,$00,$F0,$FF,$00,$00,$FF,$00,$F0,$FF,$F0,$FF,$1C,$3E
       .byte $73,$63,$63,$67,$3E,$1C,$3E,$1C,$0C,$0C,$0C,$3C,$1C,$0C,$7F,$60
       .byte $70,$3E,$03,$33,$67,$3E,$3E,$67,$33,$06,$0C,$07,$33,$1E,$0F,$06
       .byte $FF,$C6,$66,$66,$E6,$07,$3E,$67,$33,$03,$3E,$30,$38,$1F,$3E,$73
       .byte $67,$7E,$60,$67,$33,$1E,$3C,$18,$0C,$0C,$06,$06,$C3,$7F,$3E,$67
       .byte $C3,$CF,$7E,$73,$66,$3C,$3C,$66,$03,$3F,$73,$63,$66,$3C,$3C,$3C
       .byte $3C,$3C,$7E,$7E,$7E,$7E,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
       .byte $FF,$FF,$7E,$7E,$7E,$7E,$3C,$3C,$3C,$3C,$18,$18,$18,$18,$3C,$3C
       .byte $3C,$3C,$7E,$7E,$7E,$7E,$FF,$FF,$FF,$FF,$7E,$7E,$7E,$7E,$3C,$3C
       .byte $3C,$3C,$18,$18,$18,$18

;0f88 |X       |
;0f89 |X       |
;0f8a |XXX XXX |
;0f8b |      XX|
LFF88: .byte $80,$80,$EE,$03

;0f8c | XX XXXX|
;0f8d |   XXXXX|
;0f8e |X   X XX|
;0f8f |     X  |
;0f90 | XX XXXX|
;0f91 |   XXXXX|
LFF8C: .byte $6F,$1F,$8B,$04,$6F,$1F


;0f92 |XXX     |
;0f93 |X X     |
;0f94 |X XX    |
;0f95 |X  X    |
;0f96 |XX X    |
;0f97 | X X    |
;0f98 | XXX    |
;0f99 | XX     |
LFF92: .byte $E0,$A0,$B0,$90,$D0,$50,$70,$60

;0f9a | X X    |
;0f9b | XX XX  |
;0f9c | X X    |
;0f9d | X X    |
LFF9A: .byte $50,$6C,$50,$50

;0f9e |   X  XX|
;0f9f |   X  X |
;0fa0 |   X  XX|
;0fa1 |   X XXX|
LFF9E: .byte $13,$12,$13,$17

;0fa2 |    X  X|
;0fa3 |    X  X|
;0fa4 |    X XX|
;0fa5 |   X  X |
LFFA2: .byte $09,$09,$0B,$12

;0fa6 |        |
;0fa7 |     XX |
;0fa8 |    XX X|
;0fa9 |    XX X|
LFFA6: .byte $00,$06,$0D,$0D

;0faa | XXX X  |
;0fab |  X  X  |
;0fac | XXX X  |
;0fad |  X  X  |
;0fae | XXX X  |
;0faf |  X  X  |
;0fb0 | XXX X  |
;0fb1 |  X  X  |
;0fb2 | XXX X  |
;0fb3 |  X  X  |
;0fb4 | X  XX  |
;0fb5 | X  XX  |
;0fb6 | X  XX  |
;0fb7 | XXX X  |
;0fb8 |  X  X  |
;0fb9 |X  X X  |
;0fba |     X  |
;0fbb | XXX X  |
;0fbc |  X  X  |
LFFAA: .byte $74,$24,$74,$24,$74,$24,$74,$24,$74,$24,$4C,$4C,$4C,$74,$24,$94
       .byte $04,$74,$24

;0fbd |  X  X  |
;0fbe |  X  X  |
;0fbf |X    X  |
;0fc0 |X    X  |
;0fc1 |XX   X  |
;0fc2 |XX   X  |
;0fc3 | X X X  |
;0fc4 | X X X  |
;0fc5 |X X  X  |
;0fc6 |X X  X  |
;0fc7 |  X  X  |
;0fc8 |X    X  |
;0fc9 |XX   X  |
;0fca |        |
;0fcb |        |
;0fcc |X    X  |
;0fcd |X    X  |
;0fce |XXXX X  |
;0fcf |XXXX X  |
LFFBD: .byte $24,$24,$84,$84,$C4,$C4,$54,$54,$A4,$A4,$24,$84,$C4,$00,$00,$84
       .byte $84,$F4,$F4

;0fd0 |       X|
;0fd1 |XXXXXXXX|
;0fd2 |      X |
;0fd3 |XXXXXXX |
LFFD0: .byte $01,$FF,$02,$FE

; This looks a lot like the floor plan ...
;0fd4 |X XXXX X|
;0fd5 |   XX   |
;0fd6 |        |
;0fd7 |   XX   |
;0fd8 |   XX   |
;0fd9 |X XXXX X|
;0fda |   XX   |
;0fdb |   XX   |
;0fdc |        |
;0fdd |   XX   |
;0fde |X XXXX X|
;0fdf |   XX   |
;0fe0 |        |
;0fe1 |   XX   |
;0fe2 |   XX   |
;0fe3 |X XXXX X|
;LFFD4:
FLOOR0: .byte $BD,$18,$00,$18,$18,$BD,$18,$18,$00,$18,$BD,$18,$00,$18,$18,$BD

;0fe4 |        |
;0fe5 |    XX X|
;0fe6 |XX X XXX|
;0fe7 | X   XXX|
;0fe8 |  XX XXX|
;0fe9 |X  X XXX|
;0fea |X X X X |
;0feb |XX  X   |
;0fec |X     X |
;0fed |  XX  X |
;0fee |X XX  X |
;0fef |XXXX XX |
;0ff0 |        |
;0ff1 |    XX X|
;0ff2 |    X X |
;0ff3 |    X X |
;0ff4 |    X X |
;0ff5 |    X X |
;0ff6 |X X X X |
;0ff7 |    XX X|
;0ff8 |      X |
;0ff9 |     X  |
;0ffa |     XX |
;0ffb |    X   |
;0ffc |        |
;0ffd |XXXX    |
;0ffe |        |
;0fff |XXXX    |
LFFE4: .byte $00,$0D,$D7,$47,$37,$97,$AA,$C8,$82,$32,$B2,$F6,$00,$0D,$0A,$0A
       .byte $0A,$0A,$AA,$0D,$02,$04,$06,$08,$00,$F0,$00,$F0
