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

;;;Custom registers/maps
COLLMAPBANK     = $0500    ;one page is 256 bytes. Is this enough or do I bleed into 0600?
ENTITIES        = $0600
;If I do a 2 bit coll map, each tile can have 4 properties: "not there", "solid", "climbable", "damaging"
.scope EntityType
    NoEntity = 0
    PlayerType = 1
    Treasure = 2
    Enemy = 3
.endscope

.struct Entity
    Type    .byte
    xpos    .byte
    ypos    .byte
    state   .byte
    ;States for Treasure: 0 - untouched, 1 - retrieved
    ;States for Enemy: 0 - dead, 1 - alive, 2 - attacking
    tlspr      .byte
    trspr      .byte
    blspr        .byte
    brspr       .byte
    tlpal       .byte
    trpal       .byte
    blpal       .byte
    brpal       .byte
.endstruct

.struct Player
    xpos .byte
    ypos .byte
    metax .byte
    metay .byte
    state .byte  ;faceright faceleft      fall  climb   hurt  jump  walk standing
                 ;0         0               0     0       0     0    0     0 -
    env   .byte
.endstruct

.struct PlayerSprite    ;directions based on facing right. swap when facing left.
    topleft     .byte
    topright    .byte
    btmleft     .byte
    btmright    .byte
    tlpal       .byte
    trpal       .byte
    blpal       .byte
    brpal       .byte
.endstruct

.struct Metatile
    ypos        .byte       
    spritetl    .byte
    spritetr    .byte      
    spritebl    .byte      
    spritebr    .byte      
    atttl         .byte
    atttr         .byte
    attbl         .byte
    attbr         .byte
    xpos        .byte
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
playerdata: .res .sizeof(Player)
;MAXENTITIES = 5        ;We don't have any entity types defined yet, so I'm going to skip some of the tutorial lines
;entities:   .res .sizeof(Entity) * MAXENTITIES  ;Here I can reserve space for commonly used game objects
;TOTALENTITIES = .sizeof(Entity) * MAXENTITIES   ;He does the sizeof(Entity) * MAXENTITIES to make sure he always has 
                                                ;enough space on zeropage to handle objects on screen quickly
buttonflag: .res 1
counter:    .res 1
checkvar:   .res 1
colltemp1:  .res 1
colltemp2:  .res 1
colltemp3:  .res 1
animaframes: .res 1
totalsprites:   .res 1
playersprite: .res .sizeof(PlayerSprite)
blockrow:   .res 16
spritemem:  .res 2
ptr:        .res 2
metatile:   .res .sizeof(Metatile)

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

    
InitializePlayer:
    LDA #$20
    STA playerdata+Player::xpos
    LDA #$00
    STA playerdata+Player::ypos
    STA playerdata+Player::env
    LDA #%10100000  ;falling, facing right
    STA playerdata+Player::state
    LDA #$00
    STA playersprite+PlayerSprite::topleft
    LDA #$01
    STA playersprite+PlayerSprite::topright
    LDA #$10
    STA playersprite+PlayerSprite::btmleft
    LDA #$11
    STA playersprite+PlayerSprite::btmright
    LDA #$00
    STA playersprite+PlayerSprite::tlpal
    STA playersprite+PlayerSprite::trpal
    STA playersprite+PlayerSprite::blpal
    STA playersprite+PlayerSprite::brpal

    LDY #$00
InitializeEntities:
    ;The only startingg entity is the last medallion piece
    ;In the future, I can have an array of starting entites somewhere to read from
    LDA #EntityType::Treasure
    STA $0400
    LDA #$C0    ;xpos
    STA $0401
    LDA #$E0    ;ypos
    STA $0402
    LDA #$00
    STA $0403
    LDA #$26
    STA $0404
    LDA #$27
    STA $0405
    LDA #$36
    STA $0406
    LDA #$37
    STA $0407
    LDA #$01
    STA $0408
    STA $0409
    STA $040A
    STA $040B

    LDA #$05 ;player plus medallion pieces
    STA totalsprites
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
LoadScreen:
;Background filling routine begins
;This section will get subbed out for metatile implementation
;For simplicity, I'm gonna only use one palette for the BG

;Begin with canvas tile fill
;Ok, can't do that because of the write order, unless maybe I build an array in RAM then store it.
;I need a way to check against the block table for each location.

;Instead of filling the whole nametable at once, I'm going to try writing to a buffer. We'll have a toggle as to whether
;it's writing the top side or bottom side of the row. This may go slow, but let's see if it works first. If it all fits b/t v-blank, then
;great, though if I find performance issues later, optimizing here should be my first move.
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
    BCS CheckVineA
CheckBlockA:
    TXA             ;X is current iteration through the buffer, by twos (so that each metatile is placed two positions apart)
    LSR             ;So to check against the "meta" x position, we should shift right.
    CMP checkvar
    BNE BuffCanvasA
    JMP BuffBlockA
CheckVineA:
    TXA             ;X is current iteration through the buffer, by twos (so that each metatile is placed two positions apart)
    LSR             ;So to check against the "meta" x position, we should shift right.
    CMP checkvar
    BNE BuffCanvasA
    JMP BuffVineA
checkB:
    LDA blockrow, y ;What is first piece of block data
    LSR
    LSR
    LSR
    LSR             ;put X value in low 4 bits - works
    STA checkvar
    BCS CheckVineB
CheckBlockB:
    TXA             ;X is current iteration through the buffer, by twos (so that each metatile is placed two positions apart)
    LSR             ;So to check against the "meta" x position, we should shift right.
    CMP checkvar
    BNE JumpToBuffCanvasB
    JMP BuffBlockB
CheckVineB:
    TXA             ;X is current iteration through the buffer, by twos (so that each metatile is placed two positions apart)
    LSR             ;So to check against the "meta" x position, we should shift right.
    CMP checkvar
    BNE JumpToBuffCanvasB
    JMP BuffVineB
JumpToBuffCanvasB:
    JMP BuffCanvasB
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
BuffCanvasA:    ;TODO: use a metatile struct and preload it before calling the buff routine so that it's only written once
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
    BEQ JumpToWriteB
    JMP FillBuffer
JumpToWriteB:
    JMP WriteBuffer
BuffVineA:
    LDA #$04         ;top left CHR address
    STA tilebufferA, x
    INX
    LDA #$05         ;top right
    STA tilebufferA, x
    TXA
    CLC
    ADC #$10
    TAX
    LDA #$15         ;bottom right
    STA tilebufferA, x
    DEX
    LDA #$14        ;bottom left
    STA tilebufferA, x
    TXA
    SEC
    SBC #$10        ;reset x to starting value
    TAX
    INX             ;increment x twice for next loop
    INX
    CPX #$20
    BEQ WriteBuffer
    INY
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
BuffVineB:
    TXA             
    PHA
    SEC
    SBC #$10        ;store iterative X then subtract 16 from X to get a new iterator for second buffer. Grab og X from stack when done.
    TAX
    LDA #$04         ;top left CHR address
    STA tilebufferB, x
    INX
    LDA #$05         ;top right
    STA tilebufferB, x
    TXA
    CLC
    ADC #$10
    TAX
    LDA #$15         ;bottom right
    STA tilebufferB, x
    DEX
    LDA #$14        ;bottom left
    STA tilebufferB, x
    PLA
    TAX
    INX             ;increment x for next loop
    INX
    CPX #$20
    BEQ WriteBuffer
    INY             ;Incrememnt y because we drew a block
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
LoadCollMap:
    LDA #<COLLMAP0   ;high byte
    STA ptr+0
    LDA #>COLLMAP0   ;low byte
    STA ptr+1
    LDY #$00
    LDX #$00
WriteCMLoop:
    LDA (ptr), y
    STA COLLMAPBANK, x
    INY
    INX
    CPY #$3C    ;60 bytes in COLLMAP0
    BNE WriteCMLoop



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
;;TODO: More robust logic will go on button calls once collision detection is implemented

checkleft:
    LDA controller          ;I think 1 means not pressed and 0 means pressed (opposite normal)
    AND #$02                ;bit2 is left, AND will return true if left is 1 (not pressed) then jump to checkright
    BEQ checkright          ;
    LDA playerdata+Player::state
    AND #%00111100
    ORA #%01000010          ;walking, facing left
    STA playerdata+Player::state
    DEC playerdata+Player::xpos   ;decrement x position
    INC animaframes
    JMP checkup ; don't allow for left and right at the same time (jump past checkright if left was pressed)

checkright:
    LDA controller
    AND #$01
    BEQ checkup
    LDA playerdata+Player::state
    AND #%00111100
    ORA #%10000010          ;walking facing right
    STA playerdata+Player::state
    INC playerdata+Player::xpos
    INC animaframes

checkup:
    LDA controller
    AND #$08
    BEQ checkdown
    LDA playerdata+Player::env
    CMP #$01    ;over a climbable
    BNE donecheckingdirectional
    LDA playerdata+Player::state
    ORA #%00010000      ;set to climbing
    DEC playerdata+Player::ypos
;JumpToDone:
    ;JMP donecheckingdirectional ;jump past check down so not getting both simultaneous

checkdown:
    LDA controller
    AND #$04
    BEQ donecheckingdirectional
    LDA playerdata+Player::env
    CMP #$01    ;over a climbable
    BNE donecheckingdirectional
    LDA playerdata+Player::state
    ORA #%00010000      ;set to climbing
    INC playerdata+Player::ypos
;JumpToDone:
    ;JMP donecheckingdirectional ;jump past check down so not getting both simultaneous

    ;INC playerdata+Player::ypos

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
    ;JMP playerjump
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

processplayer:
FindMetaPosition:
    LDA playerdata+Player::xpos
    LSR
    LSR
    LSR
    LSR
    STA playerdata+Player::metax
    LDA playerdata+Player::ypos
    LSR
    LSR
    LSR
    LSR
    STA playerdata+Player::metay
LoadState:
;check walk state
    LDA playerdata+Player::state
    AND #%00000010
    CMP #$02
    BNE CheckFalling
    LDA animaframes
    LSR
    LSR
    LSR
    LSR
    BCS SetWalkTwo
SetWalkOne:
    LDA #$02
    STA playersprite+PlayerSprite::topleft
    LDA #$03
    STA playersprite+PlayerSprite::topright
    LDA #$12
    STA playersprite+PlayerSprite::btmleft
    LDA #$13
    STA playersprite+PlayerSprite::btmright
    JMP CheckFalling
SetWalkTwo:
    LDA #$04
    STA playersprite+PlayerSprite::topleft
    LDA #$05
    STA playersprite+PlayerSprite::topright
    LDA #$14
    STA playersprite+PlayerSprite::btmleft
    LDA #$15
    STA playersprite+PlayerSprite::btmright

;check fall state
CheckFalling:
    LDA playerdata+Player::env
    CMP #$01    ;is player climbing?
    BEQ EndProcessPlayer
    LDA playerdata+Player::state
    AND #%00100000
    ;bitmask against falling %00100000 if necessary
    CMP #$20
    BEQ PlayerFall
;check stand state
    LDA playerdata+Player::state
    AND #%00000010
    CMP #$00
    BEQ PlayerIdle
    JMP EndProcessPlayer
PlayerFall:
    ;to get it working, fall at a rate of 2 pps. Math later.
    INC playerdata+Player::ypos
    INC playerdata+Player::ypos
    JMP EndProcessPlayer
PlayerIdle:
    LDA #$00
    STA animaframes
    STA playersprite+PlayerSprite::topleft
    LDA #$01
    STA playersprite+PlayerSprite::topright
    LDA #$10
    STA playersprite+PlayerSprite::btmleft
    LDA #$11
    STA playersprite+PlayerSprite::btmright
EndProcessPlayer:
    JSR CheckPlayerCollisionDown    ;check downward collision every frame
    JSR CheckPlayerCollisionLeft
    JSR CheckPlayerCollisionRight
    JSR CheckPlayerCollisionOver
waitfordrawtocomplete:
    LDA drawcomplete
    CMP #$01
    BNE waitfordrawtocomplete
    LDA #$00
    STA drawcomplete

    JMP GAMELOOP

;;;;; ----- Logical subroutines ------ ;;;;;;
CheckPlayerCollisionDown: ;IN <--- direction of check (Y)
;TODO: Only worry about one directional check at a time
;Player's  x/y position need to be translated into the 2-bit collision map then compared
;with what appears directionally in the next tile in that direction. Then resolve.    

;Player X/Y are given based on 256x240 pixel screen. To reduce to 16x15, divide by 16. I think that's 4 shifts.
;Or perhaps object needs to expand to pixel level
    PHA
    TXA
    PHA
    ;LDA
    ;check facing to determine which edge to test
    LDA playerdata+Player::state
    AND #%10000000      ;check against facing right
    BEQ CheckFacingLeft ;if not, go to left
CheckFacingRight:
    LDA playerdata+Player::xpos    ;Use pixel X for finer tune
    STA colltemp3
    LSR
    LSR
    LSR
    LSR
    LSR
    LSR
    STA colltemp2   ;collmap x byte
    JMP ContinueCheck
CheckFacingLeft:
    LDA playerdata+Player::xpos    ;Use pixel X for finer tune
    CLC
    ADC #$10
    STA colltemp3
    LSR
    LSR
    LSR
    LSR
    LSR
    LSR
    STA colltemp2   ;collmap x byte
ContinueCheck:
    LDA playerdata+Player::metay
    ASL
    ASL
    CLC
    ADC colltemp2   ;Add x byte to Y byte to find byte location in collmap
    ;Once "inside" the byte, bit position can be found with metax. But I need the bit that is "below" it in the map,
    ;which is the bit that is 4 bytes away. Which may render the above STA useless since I'm still modifying this value
    CLC
    ADC #$04    ;+4 takes us "down a row" to the meta tile below. This logic will change per subroutine
    STA colltemp1   ;target byte, time to find the bit
    SEC
    SBC #$3C
    BCS ReturnFromCheckDown
    LDA colltemp3       ;get x back
    LSR
    LSR
    LSR
    LSR
    AND #%00000011              ;mask out last two bits. Result determines bit mask for target byte
    CMP #$00
    BEQ MaskOutZero
    CMP #$01
    BEQ MaskOutOne
    CMP #$02
    BEQ MaskOutTwo
    CMP #$03
    BNE ReturnFromCheckDown
    JMP MaskOutThree
ReturnFromCheckDown:
    JMP ReturnFromColl
MaskOutZero:
    LDX colltemp1
    LDA COLLMAPBANK, x ;Load target byte from collision map
    AND #%11000000
    CMP #%01000000
    BNE NoGround  ;if not a 01, no collision
ZeroChangeState:
    LDA playerdata+Player::metay
    ASL
    ASL
    ASL
    ASL
    STA playerdata+Player::ypos ;set player's actual Ypos to the top of the meta tile.
    LDA playerdata+Player::state
    AND #%11000000
    ORA #%00000001
    STA playerdata+Player::state
    JMP ReturnFromColl
MaskOutOne:
    LDX colltemp1
    LDA COLLMAPBANK, x ;Load target byte from collision map
    AND #%00110000
    CMP #%00010000
    BNE NoGround  ;if not a 01, no collision
OneChangeState:
    LDA playerdata+Player::metay
    ASL
    ASL
    ASL
    ASL
    STA playerdata+Player::ypos ;set player's actual Ypos to the top of the meta tile.
    LDA playerdata+Player::state
    AND #%11000000
    ORA #%00000001
    STA playerdata+Player::state
    JMP ReturnFromColl
MaskOutTwo:
;left side check
    LDX colltemp1
    LDA COLLMAPBANK, x ;Load target byte from collision map
    AND #%00001100
    CMP #%00000100
    BNE NoGround  ;if not a 01, no collision
TwoChangeState:
    LDA playerdata+Player::metay
    ASL
    ASL
    ASL
    ASL
    STA playerdata+Player::ypos ;set player's actual Ypos to the top of the meta tile.
    LDA playerdata+Player::state
    AND #%11000000
    ORA #%00000001
    STA playerdata+Player::state
    JMP ReturnFromColl
MaskOutThree:
;left side check
    LDX colltemp1
    LDA COLLMAPBANK, x ;Load target byte from collision map
    AND #%00000011
    CMP #%00000001
    BNE NoGround  ;if not a 01, no collision
ThreeChangeState:
    LDA playerdata+Player::metay
    ASL
    ASL
    ASL
    ASL
    STA playerdata+Player::ypos ;set player's actual Ypos to the top of the meta tile.
    LDA playerdata+Player::state
    AND #%11000000
    ORA #%00000001
    STA playerdata+Player::state
    JMP ReturnFromColl
NoGround:
    LDA playerdata+Player::state
    AND #%00010000    ;is the player climbing?
    CMP #%00010000
    BEQ ReturnFromColl
    LDA playerdata+Player::state
    AND #%11000000
    ORA #%00100000
    STA playerdata+Player::state ;set player state to falling
ReturnFromColl:
    PLA
    TAX
    PLA
    RTS
;----------------------------------------------------------------------------------------------------
CheckPlayerCollisionLeft:
    PHA
    TXA
    PHA
    LDA playerdata+Player::metax
    LSR
    LSR
    STA colltemp1
    LDA playerdata+Player::metay
    ASL
    ASL
    CLC
    ADC colltemp1   ;Add x byte to Y byte to find byte location in collmap
    STA colltemp2   ;Location of player byte in coll map
    ;find bit pair
    LDA playerdata+Player::metax
    AND #%00000011
    CMP #$00
    BEQ MaskOutZeroL
    CMP #$01
    BEQ MaskOutOneL
    CMP #$02
    BEQ MaskOutTwoL
    CMP #$03
    BNE ReturnFromCollL
    JMP MaskOutThreeL
MaskOutZeroL:
    LDX colltemp2
    LDA COLLMAPBANK, x
    AND #%11000000 ;the bit set to the left of three
    CMP #%00000000
    BEQ ReturnFromCollL
    CMP #%10000000
    BEQ ReturnFromCollL
    INC playerdata+Player::xpos
    JMP ReturnFromCollL
MaskOutOneL:
    LDX colltemp2
    LDA COLLMAPBANK, x
    AND #%00110000 ;the bit set to the left of three
    CMP #%00000000
    BEQ ReturnFromCollL
    CMP #%00100000
    BEQ ReturnFromCollL
    INC playerdata+Player::xpos
    JMP ReturnFromCollL
MaskOutTwoL:
    LDX colltemp2
    LDA COLLMAPBANK, x
    AND #%00001100 ;the bit set to the left of three
    CMP #%00000000
    BEQ ReturnFromCollL
    CMP #%00001000
    BEQ ReturnFromCollL
    INC playerdata+Player::xpos
    JMP ReturnFromCollL
MaskOutThreeL:
    LDX colltemp2
    LDA COLLMAPBANK, x
    AND #%00000011 ;the bit set to the left of three
    CMP #%00000000
    BEQ ReturnFromCollL
    CMP #%00000010
    BEQ ReturnFromCollL
    INC playerdata+Player::xpos
ReturnFromCollL:
    PLA
    TAX
    PLA
    RTS
;---------------------------------------------------------------
CheckPlayerCollisionRight:
    PHA
    TXA
    PHA
    LDA playerdata+Player::metax
    CLC
    ADC #$01
    LSR
    LSR
    STA colltemp1
    LDA playerdata+Player::metay
    ASL
    ASL
    CLC
    ADC colltemp1   ;Add x byte to Y byte to find byte location in collmap
    STA colltemp2   ;Location of player byte in coll map
    ;find bit pair
    LDA playerdata+Player::metax
    AND #%00000011
    CMP #$00
    BEQ MaskOutZeroR
    CMP #$01
    BEQ MaskOutOneR
    CMP #$02
    BEQ MaskOutTwoR
    CMP #$03
    BNE ReturnFromCollR
    JMP MaskOutThreeR
MaskOutZeroR:
    LDX colltemp2
    LDA COLLMAPBANK, x
    AND #%00110000 ;the bit set to the left of three
    CMP #%00000000
    BEQ ReturnFromCollR
    CMP #%00100000
    BEQ ReturnFromCollR
    DEC playerdata+Player::xpos
    JMP ReturnFromCollR
MaskOutOneR:
    LDX colltemp2
    LDA COLLMAPBANK, x
    AND #%00001100 ;the bit set to the left of three
    CMP #%00000000
    BEQ ReturnFromCollR
    CMP #%00001000
    BEQ ReturnFromCollR
    DEC playerdata+Player::xpos
    JMP ReturnFromCollR
MaskOutTwoR:
    LDX colltemp2
    LDA COLLMAPBANK, x
    AND #%00000011 ;the bit set to the left of three
    CMP #%00000000
    BEQ ReturnFromCollR
    CMP #%00000010
    BEQ ReturnFromCollR
    DEC playerdata+Player::xpos
    JMP ReturnFromCollR
MaskOutThreeR:
    LDX colltemp2
    LDA COLLMAPBANK, x
    AND #%11000000 ;the bit set to the left of three
    CMP #%00000000
    BEQ ReturnFromCollR
    CMP #%10000000
    BEQ ReturnFromCollR
    DEC playerdata+Player::xpos
ReturnFromCollR:
    PLA
    TAX
    PLA
    RTS


CheckPlayerCollisionOver:
    PHA
    TXA
    PHA
    LDA playerdata+Player::xpos
    CLC
    ADC #$08
    LSR
    LSR
    LSR
    LSR
    LSR
    LSR
    STA colltemp1
    LDA playerdata+Player::metay
    ASL
    ASL
    CLC
    ADC colltemp1   ;Add x byte to Y byte to find byte location in collmap
    STA colltemp2   ;Location of player byte in coll map
    ;find bit pair
    LDA playerdata+Player::xpos
    CLC
    ADC #$08
    LSR
    LSR
    LSR
    LSR
    AND #%00000011
    CMP #$00
    BEQ MaskOutZeroO
    CMP #$01
    BEQ MaskOutOneO
    CMP #$02
    BEQ MaskOutTwoO
    CMP #$03
    BNE ReturnFromCollO
    JMP MaskOutThreeO
MaskOutZeroO:
    LDX colltemp2
    LDA COLLMAPBANK, x
    AND #%11000000 ;the bit set to the left of three
    CMP #%10000000
    BNE NoEnvCondition
    LDA #$01            ;01 will mean over a climbable
    STA playerdata+Player::env
    JMP ReturnFromCollR
MaskOutOneO:
    LDX colltemp2
    LDA COLLMAPBANK, x
    AND #%00110000 ;the bit set to the left of three
    CMP #%00100000
    BNE NoEnvCondition
    LDA #$01            ;01 will mean over a climbable
    STA playerdata+Player::env
    JMP ReturnFromCollR
MaskOutTwoO:
    LDX colltemp2
    LDA COLLMAPBANK, x
    AND #%00001100 ;the bit set to the left of three
    CMP #%00001000
    BNE NoEnvCondition
    LDA #$01            ;01 will mean over a climbable
    STA playerdata+Player::env
    JMP ReturnFromCollR
MaskOutThreeO:
    LDX colltemp2
    LDA COLLMAPBANK, x
    AND #%00000011 ;the bit set to the left of three
    CMP #%00000010
    BNE NoEnvCondition
    LDA #$01            ;01 will mean over a climbable
    STA playerdata+Player::env
    JMP ReturnFromCollO
NoEnvCondition:
    LDA #$00
    STA playerdata+Player::env
    LDA playerdata+Player::state
    AND #%11001111
ReturnFromCollO:
    PLA
    TAX
    PLA
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    NMI / VBLANK    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

DRAWENTITIES:    ;move back to entity manager later. THis is terrible Jam code.
    CPX #$00    ;should start with x=0 - player sprite
    BEQ PLAYERSPRITE
    CPX #$01
    BEQ LoadMedallion1
    CPX #$02
    BEQ GoToLoadMedallion2
    CPX #$03
    BEQ GoToLoadMedallion3
    JMP DONESPRITE
GoToLoadMedallion2:
    JMP LoadMedallion2
GoToLoadMedallion3:
    JMP LoadMedallion3
    ;CMP #EntityType::PlayerType
    ;BEQ PLAYERSPRITE
    ;JMP CHECKENDSPRITE
    ;see https://www.nesdev.org/wiki/PPU_OAM
    ;top left sprite
PLAYERSPRITE:
    LDA playerdata+Player::state
    AND #%11000000
    CMP #$80
    BNE LoadPlayerFacingLeft
LoadPlayerFacingRight:
    LDA playerdata+Player::ypos
    STA metatile+Metatile::ypos
    LDA playerdata+Player::xpos
    STA metatile+Metatile::xpos
    LDA playersprite+PlayerSprite::topleft
    STA metatile+Metatile::spritetl
    LDA playersprite+PlayerSprite::topright
    STA metatile+Metatile::spritetr
    LDA playersprite+PlayerSprite::btmleft
    STA metatile+Metatile::spritebl
    LDA playersprite+PlayerSprite::btmright
    STA metatile+Metatile::spritebr
    LDA #$00
    STA metatile+Metatile::atttl
    STA metatile+Metatile::atttr
    STA metatile+Metatile::attbl
    STA metatile+Metatile::attbr
    JMP DRAWMETATILE
LoadPlayerFacingLeft:
    LDA playerdata+Player::ypos
    STA metatile+Metatile::ypos
    LDA playerdata+Player::xpos
    STA metatile+Metatile::xpos
    LDA playersprite+PlayerSprite::topright
    STA metatile+Metatile::spritetl
    LDA playersprite+PlayerSprite::topleft
    STA metatile+Metatile::spritetr
    LDA playersprite+PlayerSprite::btmright
    STA metatile+Metatile::spritebl
    LDA playersprite+PlayerSprite::btmleft
    STA metatile+Metatile::spritebr
    LDA #$40
    STA metatile+Metatile::atttl
    STA metatile+Metatile::atttr
    STA metatile+Metatile::attbl
    STA metatile+Metatile::attbr
    JMP DRAWMETATILE
LoadMedallion1:
    LDA #$08
    STA metatile+Metatile::ypos
    LDA #$D8
    STA metatile+Metatile::xpos
    LDA #$20
    STA metatile+Metatile::spritetl
    LDA #$21
    STA metatile+Metatile::spritetr
    LDA #$30
    STA metatile+Metatile::spritebl
    LDA #$31
    STA metatile+Metatile::spritebr
    LDA #$01
    STA metatile+Metatile::atttl
    STA metatile+Metatile::atttr
    STA metatile+Metatile::attbl
    STA metatile+Metatile::attbr
    JMP DRAWMETATILE
LoadMedallion2:
    LDA #$08
    STA metatile+Metatile::ypos
    LDA #$E8
    STA metatile+Metatile::xpos
    LDA #$22
    STA metatile+Metatile::spritetl
    LDA #$23
    STA metatile+Metatile::spritetr
    LDA #$32
    STA metatile+Metatile::spritebl
    LDA #$33
    STA metatile+Metatile::spritebr
    LDA #$01
    STA metatile+Metatile::atttl
    STA metatile+Metatile::atttr
    STA metatile+Metatile::attbl
    STA metatile+Metatile::attbr
    JMP DRAWMETATILE
LoadMedallion3:
    LDA #$18
    STA metatile+Metatile::ypos
    LDA #$D8
    STA metatile+Metatile::xpos
    LDA #$24
    STA metatile+Metatile::spritetl
    LDA #$25
    STA metatile+Metatile::spritetr
    LDA #$34
    STA metatile+Metatile::spritebl
    LDA #$35
    STA metatile+Metatile::spritebr
    LDA #$01
    STA metatile+Metatile::atttl
    STA metatile+Metatile::atttr
    STA metatile+Metatile::attbl
    STA metatile+Metatile::attbr
    JMP DRAWMETATILE


;-----------------------------------------------------------------------------
    DRAWMETATILE: ;in--struct of meta tile data
    LDA metatile+Metatile::ypos ; y
    STA (spritemem), y
    INY
    LDA metatile+Metatile::spritetl ; tile
    STA (spritemem), y
    INY
    LDA metatile+Metatile::atttl ; palette etc
    STA (spritemem), y
    INY
    LDA metatile+Metatile::xpos   ; x
    STA (spritemem), y
    INY

    ;bottom left sprite
    LDA metatile+Metatile::ypos ; y
    CLC
    ADC #$08   ;Add 8 pixels to y-pos for second sprite
    STA (spritemem), y
    INY
    LDA metatile+Metatile::spritebl ; tile
    STA (spritemem), y
    INY
    LDA metatile+Metatile::attbl   ;palette
    STA (spritemem), y
    INY
    LDA metatile+Metatile::xpos ; x position
    STA (spritemem), y
    INY

    ;top right sprite
    LDA metatile+Metatile::ypos
    STA (spritemem), y
    INY
    LDA metatile+Metatile::spritetr     ;same as top left but we will flip it and add 8 to xpos
    STA (spritemem), y
    INY
    LDA metatile+Metatile::atttr     ;palette %01000001   palette 1, flip horizontal
    STA (spritemem), y
    INY
    LDA metatile+Metatile::xpos
    CLC
    ADC #$08
    STA (spritemem), y
    INY

    ;bottom right
    LDA metatile+Metatile::ypos ; y
    CLC
    ADC #$08   ;Add 8 pixels to y-pos for second sprite
    STA (spritemem), y
    INY
    LDA metatile+Metatile::spritebr   
    STA (spritemem), y
    INY
    LDA metatile+Metatile::attbr   ;palette with h-flip
    STA (spritemem), y
    INY
    LDA metatile+Metatile::xpos
    CLC
    ADC #$08
    STA (spritemem), y
    INY
    ;JMP DoneDrawingMetatile
ClearMetatile:
    LDA #$00
    STA metatile+Metatile::xpos
    STA metatile+Metatile::ypos
DoneDrawingMetatile:

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CHECKENDSPRITE:
    TXA
    CLC
    ADC #$01
    TAX
    CPX totalsprites
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
    .byte $00,                                                                       $F0, $FF
    .byte $00, $10, $20, $30, $40, $50,                                              $F0, $FF
    .byte $00,                                                                       $F0, $FF
    .byte $00,                     $50, $60, $70, $80, $90,                          $F0, $FF
    .byte $00,                                                                       $F0, $FF
    .byte $00,                                                        $C8,           $F0, $FF
    .byte $00,                                                        $C8,           $F0, $FF
    .byte $00,                                               $A0, $B0,$C8, $D0, $E0, $F0, $FF                              
    .byte $00,                                                        $C8,           $F0, $FF
    .byte $00,                                                        $C8,           $F0, $FF
    .byte $00,                                                        $C8,           $F0, $FF
    .byte $00,                                                        $C8,           $F0, $FF
    .byte $00,                                                        $C8,           $F0, $FF
    .byte $00, $10, $20, $30, $40, $50, $60, $70, $80, $90, $A0, $B0, $C0, $D0, $E0, $F0, $FF

;2-bit collision map: 00 - nothing, 01 - solid, 10 - climbable, 11 - damaging.
COLLMAP0: ;will reduce to 60 bytes
    .byte %01010000, %01010101, %01010101, %01010101
    .byte %01000000, %00000000, %00000000, %00000001
    .byte %01010101, %01010000, %00000000, %00000001
    .byte %01000000, %00000000, %00000000, %00000001
    .byte %01000000, %00010101, %01010000, %00000001
    .byte %01000000, %00000000, %00000000, %00000001
    .byte %01000000, %00000000, %00000000, %10000001
    .byte %01000000, %00000000, %00000000, %10000001
    .byte %01000000, %00000000, %00000101, %10010101
    .byte %01000000, %00000000, %00000000, %10000001
    .byte %01000000, %00000000, %00000000, %10000001
    .byte %01000000, %00000000, %00000000, %10000001
    .byte %01000000, %00000000, %00000000, %10000001
    .byte %01000000, %00000000, %00000000, %10000001
    .byte %01010101, %01010101, %01010101, %01010101

.segment "VECTORS"
    .word VBLANK
    .word RESET
    .word 0

.segment "CHARS"
    .incbin "sprites.chr"