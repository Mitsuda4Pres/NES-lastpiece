.segment "HEADER"

    .byte "NES"
    .byte $1a
    .byte $02       ; 4 - 2*16k PRG ROM
    .byte $01       ; 5 - 8k CHR ROM
    .byte %00000001 ; 6 - mapper - horizontal mirroring
    .byte $00       ; 7
    .byte $00       ; 8 - 
    .byte $00       ; 9 - NTSC
    .byte $00
    ; filler
    .byte $00,$00,$00,$00,$00

;Can define game constants here, such as a global structure or variables
;;; PPU registers.
PPUCTRL         = $2000
PPUMASK         = $2001
PPUSTATUS       = $2002
OAMADDR         = $2003
OAMDATA         = $2004
PPUSCROLL       = $2005
PPUADDR         = $2006
PPUDATA         = $2007

;;; Other IO registers.
OAMDMA          = $4014
APUSTATUS       = $4015
JOYPAD1         = $4016
JOYPAD2         = $4017

.scope EntityType
    NoEntity = 0
    PlayerType = 1
    Bullet = 2
    Enemy = 3
.endscope

.struct Entity
    xpos .byte
    ypos .byte
    type .byte
.endstruct

.struct Metatile
    pos .byte       ;{0000}x pos {0000} ypos 0-15
    data .byte      ;{0000}flip h or v {0000} nametable address
.endstruct

.segment "STARTUP"

;I had to open of the zero page in nes.cfg to range from $0002 to $00FF. Idk if that will break something later.
.segment "ZEROPAGE"
controller: .res 1    ;reserve 1 byte for controller input
drawcomplete: .res 1
scrollx:    .res 1
scrolly:    .res 1
swap:       .res 1
screentag:  .res 1
MAXENTITIES = 10        ;We don't have any entity types defined yet, so I'm going to skip some of the tutorial lines
entities:   .res .sizeof(Entity) * MAXENTITIES  ;Here I can reserve space for commonly used game objects
TOTALENTITIES = .sizeof(Entity) * MAXENTITIES   ;He does the sizeof(Entity) * MAXENTITIES to make sure he always has 
                                                ;enough space on zeropage to handle objects on screen quickly
buttonflag: .res 1
counter:    .res 1
tablesize:  .res 1
tiletoggle: .res 1
tilebuffer: .res 32
spritemem:  .res 2
ptr:        .res 2


.segment "CODE"

WAITFORVBLANK:
    BIT $2002
    BPL WAITFORVBLANK
    RTS

RESET:
    SEI
    CLD
    LDX #$40
    STX $4017
    LDX #$FF        ;Set stack starting point to 0xFF
    TXS
    INX             ;X becomes 0
    STX $2000       ;Zero out PPU register PPUCONTROL
    STX $2001       ;PPUMASK
    STX $4010       ;Audio register

    JSR WAITFORVBLANK

    TXA

CLEARMEM:
    STA $0000, x    ;zeroing out the memory ranges. X is indexing through each block. when X incs to 1, address= $0001 etc
    STA $0100, x
    ;               ;$0200 will not be zeroed out. Graphics info here
    STA $0300, x
    STA $0400, x
    STA $0500, x
    STA $0600, x
    STA $0700, x
    LDA #$FF
    STA $0200, x    ;Fills $0200 with $FF, not 0. 
    LDA #$00
    STA controller
    INX
    BNE CLEARMEM

    
;initialize entities + Entity::xpos
    LDA #$80
    STA entities+Entity::xpos
    LDA #$78
    STA entities+Entity::ypos
    LDA #EntityType::PlayerType
    STA entities+Entity::type


;Clear register and set palette address
    LDA $2002
    LDA #$3F
    STA $2006
    LDA #$10
    STA $2006

    LDX #$00
PALETTELOAD:
    LDA PALETTE, x
    STA $2007           ;PPUDATA
    INX
    CPX #$20
    BNE PALETTELOAD

;;;;;;;;;;;;;;;;;;;;;;;;

    LDA $2002           ;PPUSTATUS    Is he reading to clear vblank flag? Neads to be changed to use NMI instead
    LDA #$20
    STA $2006           ;PPUADDR      we are using $2000 for graphics memory
    LDA #$00
    STA $2006           ;PPUADDR

;Background filling routine begins
;This section will get subbed out for metatile implementation
;For simplicity, I'm gonna only use one palette for the BG

;Begin with canvas tile fill
;Ok, can't do that because of the write order, unless maybe I build an array in RAM then store it.
;I need a way to check against the block table for each location.

;Instead of filling the whole nametable at once, I'm going to try writing to a buffer. We'll have a toggle as to whether
;it's writing the top side or bottom side of the row. This may go slow, but let's see if it works first. If it all fits b/t v-blank, then
;great, though if I find performance issues later, optimizing here should be my first move.
;Logical order:
;Loop 15x{
;   Write top line to tile buffer using Y index
;       Check for Block tiles along the way
;           Write buffer to $2007
;               Switch toggle
;   Write bottom line to tile buffer
;       Check for Block tiles along the way
;           Write buffer to $2007
;               Switch toggle
;       }













    LDY #$08            ;outside loop index for 2KB
    LDX #$00            ;inside loop indexes
FillCanvasTileLoop:
    TXA
    AND #$21
    ;CMP #$00
    ;BEQ TopLeft
    CMP #$01
    BEQ TopRight
    CMP #$20
    BEQ BtmLeft
    CMP #$21
    BEQ BtmRight
TopLeft:
    LDA #$02        ;CHR address
    STA $2007
    JMP IncX
TopRight:
    LDA #$03        ;CHR address
    STA $2007
    JMP IncX
BtmLeft:
    LDA #$12        ;CHR address
    STA $2007
    JMP IncX
BtmRight:
    LDA #$13        ;CHR address
    STA $2007
IncX:
    INX
    BNE FillCanvasTileLoop
    DEY
    BNE FillCanvasTileLoop

FillBlockTileLoop:



FILLATTRIBUTE0:
    LDA $2002       ;reset latch
    LDA #$23        ;High byte of $23CO address (attributes)
    STA $2006
    LDA #$C0        ;Low byte
    STA $2006
    LDX #$40        ;Fill with 64 bytes
    LDA #$00        ;Attribute value
FillAttribute0Loop:
    STA $2007
    DEX
    BNE FillAttribute0Loop

    JSR WAITFORVBLANK

    LDA #%10000000
    STA $2000           ;PPUCONTROL
    LDA #%00011110
    STA $2001           ;PPUMASK

    LDA #$00
    STA spritemem
    LDA #$02
    STA spritemem+1

    JMP GAMELOOP




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GAMELOOP:

INITSPRITES:
    LDY #$00
    LDA #$FF

INITSPRITELOOP:
    STA (spritemem), y
    INY
    EOR #$FF
    STA (spritemem), y
    INY
    STA (spritemem), y
    INY
    EOR #$FF
    STA (spritemem), y
    INY
    BEQ startreadcontrollers
    JMP INITSPRITELOOP

startreadcontrollers:
    ;read controls
    LDA #$01
    STA $4016
    LDA #$00
    STA $4016

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

readcontrollerbuttons:

    LDA $4016           ;a   
    ROR A               ;ROR from 0 bit to carry bit
    ROL controller      ;ROL from carry bit into 0 bit. After 8 reads, this will end up in the 8-bit a button bit
    LDA $4016           ;b
    ROR A
    ROL controller
    LDA $4016           ; select
    ROR A
    ROL controller
    LDA $4016           ; start
    ROR A
    ROL controller
    LDA $4016           ; up
    ROR A
    ROL controller
    LDA $4016           ; down
    ROR A
    ROL controller
    LDA $4016           ; left
    ROR A
    ROL controller
    LDA $4016           ; right
    ROR A
    ROL controller

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

checkleft:
    LDA controller          ;I think 1 means not pressed and 0 means pressed (opposite normal)
    AND #$02                ;abssudLr  bit2 is left, AND will return true if left is 1 (not pressed) then jump to checkright
    BEQ checkright          ;
    DEC entities+Entity::xpos   ;decrement x position
    JMP checkup ; don't allow for left and right at the same time (jump past checkright if left was pressed)

checkright:
    LDA controller
    AND #$01
    BEQ checkup
    INC entities+Entity::xpos

checkup:
    LDA controller
    AND #$08
    BEQ checkdown
    DEC entities+Entity::ypos
    JMP donecheckingdirectional ;jump past check down so not getting both simultaneous

checkdown:
    LDA controller
    AND #$04
    BEQ donecheckingdirectional
    INC entities+Entity::ypos

donecheckingdirectional:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
checkbuttons:

checka:
    LDA controller
    AND #$80
    BEQ checkarelease
    LDA buttonflag
    ORA #$01
    STA buttonflag
    JMP finishcontrols
checkarelease:
    LDA buttonflag
    AND #$01
    BEQ finishcontrols
    DEC buttonflag ;only works for a-button bc it's bit 1
    ;JMP addbullet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
finishcontrols:
processcrolling:
    LDA scrolly
    SEC
    SBC #$02
    STA scrolly
    CMP #$00
    BNE donescroll
    LDA #$EE
    STA scrolly
    LDA swap
    EOR #$02
    STA swap
donescroll:
doneprocessentities:
    NOP
    NOP
    NOP
waitfordrawtocomplete:
    LDA drawcomplete
    CMP #$01
    BNE waitfordrawtocomplete
    LDA #$00
    STA drawcomplete

    JMP GAMELOOP

VBLANK:
    PHA ;push registers - A, P, X, Y
    PHP
    TXA
    PHA
    TYA
    PHA

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;begin populating the OAM data in memory
    LDX #$00        ;x will index mem locations of entity data (getter)
    LDA #$00        ;low byt of graphics page $0200
    LDY #$00        ;y will index the OAM memory location (putter)
    STA spritemem
    LDA #$02        ;high byte of graphics page $0200
    STA spritemem+1

DRAWENTITIES:                       ;copied his code in for now. 
    LDA entities+Entity::type, x    ;should start with x=0 - player sprite
    CMP #EntityType::PlayerType
    BEQ PLAYERSPRITE
    JMP CHECKENDSPRITE
    ;see https://www.nesdev.org/wiki/PPU_OAM

PLAYERSPRITE:
    ;top left sprite
    LDA entities+Entity::ypos, x ; y
    STA (spritemem), y
    INY
    LDA #$02 ; tile
    STA (spritemem), y
    INY
    LDA #$00 ; palette etc
    STA (spritemem), y
    INY
    LDA entities+Entity::xpos, x   ; x
    STA (spritemem), y
    INY

    ;bottom left sprite
    LDA entities+Entity::ypos, x ; y
    CLC
    ADC #$08   ;Add 8 pixels to y-pos for second sprite
    STA (spritemem), y
    INY
    LDA #$12   ;tile location 16, first tile of second row
    STA (spritemem), y
    INY
    LDA #$00   ;palette
    STA (spritemem), y
    INY
    LDA entities+Entity::xpos, x ; x position
    STA (spritemem), y
    INY

    ;top right sprite
    LDA entities+Entity::ypos
    STA (spritemem), y
    INY
    LDA #$03     ;same as top left but we will flip it and add 8 to xpos
    STA (spritemem), y
    INY
    LDA #$00     ;palette %01000001   palette 1, flip horizontal
    STA (spritemem), y
    INY
    LDA entities+Entity::xpos, x
    CLC
    ADC #$08
    STA (spritemem), y
    INY

    ;bottom right
    LDA entities+Entity::ypos, x ; y
    CLC
    ADC #$08   ;Add 8 pixels to y-pos for second sprite
    STA (spritemem), y
    INY
    LDA #$13   ;tile location 16, first tile of second row
    STA (spritemem), y
    INY
    LDA #$00   ;palette with h-flip
    STA (spritemem), y
    INY
    LDA entities+Entity::xpos, x
    CLC
    ADC #$08
    STA (spritemem), y
    INY

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CHECKENDSPRITE:
    TXA
    CLC
    ADC #.sizeof(Entity)
    TAX
    CPX #TOTALENTITIES
    BEQ DONESPRITE
    JMP DRAWENTITIES

DONESPRITE:
;DMA copy sprites
    LDA #$00
    STA $2003 ;reset counter
    LDA #$02    ;set memory to 0x200 range
    STA $4014   ;OAMDMA byte - This action shoves everything we wrote to $0200 with the registerss into the PPU via OAMDMA
    NOP         ;pause for sync

    LDA #$00    ;clear register
    STA $2006
    STA $2006   ;$2006 takes a double write PPUDATA

    ;when ready for scrolling background
    ;LDA scrollx
    ;STA $2005
    ;LDA scrolly
    ;STA $2005

    LDA #%10001000
    ORA swap
    LDX $2002   ;clear the register before
    STA $2000

donewithppu:
    LDA #$01
    STA $07FF

    ;pull registers from stack - Y, X, P, A
    PLA ;push registers - A, P, X, Y
    TAY
    PLA
    TAX
    PLP
    PLA
    INC drawcomplete

    RTI

BLOCKTABLE0SIZE:
    .byte $46      ;70 blocks in this table
BLOCKTABLE0: ;These blocks are all in mem location 0 and will never flip, so all they need is position X{0000} Y{0000}
    .byte $00, $10,           $40, $50, $60, $70, $80, $90, $A0, $B0, $C0, $D0, $E0, $F0
    .byte $01,                                                                       $F1
    .byte $02, $12, $22, $32, $42, $52,                                              $F2
    .byte $03,                                                                       $F3
    .byte $04,                     $54, $64, $74, $84, $94,                          $F4
    .byte $05,                                                                       $F5
    .byte $06,                                                                       $F6
    .byte $07,                                                                       $F7
    .byte $08,                                               $A8, $B8,     $D8, $E8, $F8                                   
    .byte $09,                                                                       $F9
    .byte $0A,                                                                       $FA
    .byte $0B,                                                                       $FB
    .byte $0C,                                                                       $FC
    .byte $0D,                                                                       $FD
    .byte $0E, $1E, $2E, $3E, $4E, $5E, $6E, $7E, $8E, $9E, $AE, $BE, $CE, $DE, $EE, $FE

PALETTE:  ;seems like background can only access last 4 palettes?
    ;sprite palettes     
    .byte $0E, $17, $1C, $37 ;palette 3  ;browns and blue - main char
    .byte $0E, $1C, $2B, $39 ;palette 2  ;pastel green, blue-green, blue  map grabs this
    .byte $0A, $05, $26, $30 ;palette 3  ;green, crimson, pink, white
    .byte $0E, $13, $23, $33 ;palette 4   ;purples 

    ;bg palettes
    .byte $0F, $06, $15, $18 ;palette 1  ;browns, golds, reds, bg1
    .byte $0E, $06, $15, $36 ;palette 2   ;crimson, red, pink    bullet/enemy
    .byte $0E, $07, $1C, $37 ;palette 3  ;browns and blue - main char
    .byte $0E, $13, $23, $33 ;palette 4   ;purples


.segment "VECTORS"
    .word VBLANK
    .word RESET
    .word 0

.segment "CHARS"
    .incbin "sprites.chr"