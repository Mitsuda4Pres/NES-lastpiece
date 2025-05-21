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
tilebufferA: .res 32
tilebufferB: .res 32
swap:       .res 1
screentag:  .res 1
MAXENTITIES = 10        ;We don't have any entity types defined yet, so I'm going to skip some of the tutorial lines
entities:   .res .sizeof(Entity) * MAXENTITIES  ;Here I can reserve space for commonly used game objects
TOTALENTITIES = .sizeof(Entity) * MAXENTITIES   ;He does the sizeof(Entity) * MAXENTITIES to make sure he always has 
                                                ;enough space on zeropage to handle objects on screen quickly
buttonflag: .res 1
counter:    .res 1
checkvar:   .res 1
blockrow:   .res 16
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

;       }
    LDA #<BLOCKTABLE0   ;high byte
    STA ptr+0
    LDA #>BLOCKTABLE0   ;low byte
    STA ptr+1
    LDA #$00
    STA counter
    LDX #$00        ;outside counter goes to 15
    LDY #$00     
    TXA             ;$2007 write counter push to stack
    PHA             

;TODO: START HERE!!!!
ClearBlockRow:
    TYA
    PHA
    LDA #$00
    LDY #$00
    LDX #$00
ClearLoop:
    STA blockrow, y
    INY
    CPY #$10
    BNE ClearLoop
    PLA
    TAY
GetBlocksInRow:
    LDA (ptr), y
    CMP #$FF
    BEQ BlocksEndOfRow
    STA blockrow, x      ;Ok, I'm using y as the pointer index, but I can't use it as the blockrow index too, it's offset
    INX
    INY
    CPY #$FF       ;turns out it did happen lmao. Of course it did. If I make a check, it needs to be against $FF, assuming a level could be all blocks
    BEQ BlocksEndOfRow ;this should never happen, but if we are iterating past 16, let's call it quits. Optimization, remove this
    ;INC ptr+1           ;next address in BLOCKTABLE (Maybe needed if BLOCKTABLE is bigger than 256)
    JMP GetBlocksInRow
BlocksEndOfRow:         ;now 'blockrow' is filled with all block data on that row
    INY
    TYA
    STA counter         ;Hang on to Y value for next row.
    LDY #$00
    LDX #$00
FillBuffer: ;y - blockrow index, x - buffer index (essentially x location)
    ;16 iterations of +2. On each one we will write the 2x2 metatile
    TXA
    ASL
    ASL
    ASL

    ASL             ;push the '8' bit into the carry bit
    BCS checkB
checkA:
    LDA blockrow, y ;What is first piece of block data
    LSR
    LSR
    LSR
    LSR             ;put X value in low 4 bits - works
    STA checkvar
    TXA             ;X is current iteration through the buffer, by twos (so that each metatile is placed two positions apart)
    LSR             ;So to check against the "meta" x position, we should shift right.
    CMP checkvar
    BNE BuffCanvasA
    JMP BuffBlockA
checkB:
    LDA blockrow, y ;What is first piece of block data
    LSR
    LSR
    LSR
    LSR             ;put X value in low 4 bits - works
    STA checkvar
    TXA             ;X is current iteration through the buffer, by twos (so that each metatile is placed two positions apart)
    LSR             ;So to check against the "meta" x position, we should shift right.
    CMP checkvar
    BNE BuffCanvasB
    JMP BuffBlockB
BuffBlockA:          ;Put entire metatile into first 32 byte buffer
    LDA #$00         ;top left CHR address
    STA tilebufferA, x
    INX
    LDA #$01         ;top right
    STA tilebufferA, x
    TXA
    CLC
    ADC #$10
    TAX
    LDA #$11         ;bottom right
    STA tilebufferA, x
    DEX
    LDA #$10        ;bottom left
    STA tilebufferA, x
    TXA
    SEC
    SBC #$10        ;reset x to starting value
    TAX
    INX             ;increment x for next loop
    INX
    CPX #$20
    BEQ JumpToWrite
    INY             ;Incrememnt y because we drew a block
    JMP FillBuffer
JumpToWrite:
    JMP WriteBuffer
BuffCanvasA:
    LDA #$02         ;top left CHR address
    STA tilebufferA, x
    INX
    LDA #$03         ;top right
    STA tilebufferA, x
    TXA
    CLC
    ADC #$10
    TAX
    LDA #$13         ;bottom right
    STA tilebufferA, x
    DEX
    LDA #$12        ;bottom left
    STA tilebufferA, x
    TXA
    SEC
    SBC #$10        ;reset x to starting value
    TAX
    INX             ;increment x twice for next loop
    INX
    CPX #$20
    BEQ WriteBuffer
    JMP FillBuffer
BuffBlockB:          ;Put entire metatile into second 32 byte buffer
    TXA             
    PHA
    SEC
    SBC #$10        ;store iterative X then subtract 16 from X to get a new iterator for second buffer. Grab og X from stack when done.
    TAX
    LDA #$00         ;top left CHR address
    STA tilebufferB, x
    INX
    LDA #$01         ;top right
    STA tilebufferB, x
    TXA
    CLC
    ADC #$10
    TAX
    LDA #$11         ;bottom right
    STA tilebufferB, x
    DEX
    LDA #$10        ;bottom left
    STA tilebufferB, x
    PLA
    ;SEC
    ;SBC #$10        ;reset x to starting value
    TAX
    INX             ;increment x for next loop
    INX
    CPX #$20
    BEQ WriteBuffer
    INY             ;Incrememnt y because we drew a block
    JMP FillBuffer
BuffCanvasB:
    TXA             
    PHA
    SEC
    SBC #$10        ;store iterative X then subtract 16 from X to get a new iterator for second buffer. Grab og X from stack when done.
    TAX
    LDA #$02         ;top left CHR address
    STA tilebufferB, x
    INX
    LDA #$03         ;top right
    STA tilebufferB, x
    TXA
    CLC
    ADC #$10
    TAX
    LDA #$13         ;bottom right
    STA tilebufferB, x
    DEX
    LDA #$12        ;bottom left
    STA tilebufferB, x
    ;TXA
    ;SEC
    ;SBC #$10        ;reset x to starting value
    PLA
    TAX
    INX             ;increment x twice for next loop
    INX
    CPX #$20
    BEQ WriteBuffer
    JMP FillBuffer
WriteBuffer:
IteratorCleanup:
    LDY #$00
    LDX #$00
WriteLoopA:
    LDA tilebufferA, y
    STA $2007       ;The money line!
    INY
    CPY #$10
    BNE WriteLoopA
    LDY #$00
WriteLoopB:
    LDA tilebufferB, y
    STA $2007       ;The money line!
    INY
    CPY #$10
    BNE WriteLoopB
WriteLoopC:
    LDA tilebufferA, y ;should start pulling from tilebufferA + #$10 (16) for second row
    STA $2007       ;The money line!
    INY
    CPY #$20
    BNE WriteLoopC
    LDY #$10
WriteLoopD:
    LDA tilebufferB, y
    STA $2007       ;The money line!
    INY
    CPY #$20
    BNE WriteLoopD
    PLA
    TAX
    INX
    CPX #$0F
    BEQ FinishedBGWrite
FinishedBuffer:
    TXA
    PHA
    LDY counter
    JMP ClearBlockRow
FinishedBGWrite:

;TODO: Okay, I've got bones but they're broken bones. Time to get into the debugger

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
    
    ;TODO: potential for compressing again by half:
    ;If I have a row-ender, I don't need a Y value, so II can pack two blocks into one byte: X1{0000}X2{0000}
BLOCKTABLE0: ;These blocks are all in mem location 0 and will never flip, so all they need is position X{0000} Y{0000}
    .byte $00, $10,           $40, $50, $60, $70, $80, $90, $A0, $B0, $C0, $D0, $E0, $F0, $FF
    .byte $01,                                                                       $F1, $FF
    .byte $02, $12, $22, $32, $42, $52,                                              $F2, $FF
    .byte $03,                                                                       $F3, $FF
    .byte $04,                     $54, $64, $74, $84, $94,                          $F4, $FF
    .byte $05,                                                                       $F5, $FF
    .byte $06,                                                                       $F6, $FF
    .byte $07,                                                                       $F7, $FF
    .byte $08,                                               $A8, $B8,     $D8, $E8, $F8, $FF                              
    .byte $09,                                                                       $F9, $FF
    .byte $0A,                                                                       $FA, $FF
    .byte $0B,                                                                       $FB, $FF
    .byte $0C,                                                                       $FC, $FF
    .byte $0D,                                                                       $FD, $FF
    .byte $0E, $1E, $2E, $3E, $4E, $5E, $6E, $7E, $8E, $9E, $AE, $BE, $CE, $DE, $EE, $FE, $FF



.segment "VECTORS"
    .word VBLANK
    .word RESET
    .word 0

.segment "CHARS"
    .incbin "sprites.chr"