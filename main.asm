;The Last Piece
;An NES game conceived for the Goblin Bunker Game Jam, May 2025
;Mitsuda4Pres
;TODO: -Entity manager for each area to govern items/enemies
;   -Jump
;   -Item collection on walk-over
;   -Title screen
;   -Damage condition/damage-dealing floors/fall damage
;   -Game over state/screen



.segment "HEADER"

    .byte "NES"
    .byte $1a
    .byte $02       ; 4 - 2*16k PRG ROM
    .byte $01       ; 5 - 8k CHR ROM
    .byte %00000000 ; 6 - mapper - horizontal mirroring
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
AREABANK        = $0440     ;192 bytes for 96 areas. Do not exceed or you will bleed into $0500!
SPRITELOOKUP    = $0480     ;96 bytes, each entry 4 bytes, 24 entries
PALETTELOOKUP   = $04E0     ;32 bytes, each entry 1 byte
COLLMAPBANK     = $0500    ;one page is 256 bytes. Is this enough or do I bleed into 0600?
ENTITIES        = $0600

;If I do a 2 bit coll map, each tile can have 4 properties: "not there", "solid", "climbable", "damaging"
.scope EntityType
    NoEntity = 0
    PlayerType = 1
    Treasure = 2
    Enemy = 3
.endscope

.struct Entity          ;15b
    type        .byte   ;will correspond with the in the spritelookup table
    xpos        .byte
    ypos        .byte
    state       .byte
    ;Player States
    ;faceright faceleft      fall  climb   hurt  jump  walk standing
    ;0         0               0     0       0     0    0     0 -
    ;States for Treasure: 0 - untouched, 1 - retrieved
    ;States for Enemy: 0 - dead, 1 - alive, 2 - attacking
    sproffset   .byte
    paloffset   .byte
    tlspr       .byte
    trspr       .byte
    blspr       .byte
    brspr       .byte
    tlpal       .byte ;likely can reduce these to one palette byte, then one byte with bit flags for each quadrant for flips h and v
    trpal       .byte
    blpal       .byte
    brpal       .byte
    env         .byte   ;$00 - nothing, $01 - climbable
    metax       .byte
    metay       .byte
.endstruct

.struct Metatile        ;11b
    ypos        .byte       
    spritetl    .byte
    spritetr    .byte      
    spritebl    .byte      
    spritebr    .byte      
    atttl       .byte
    atttr       .byte
    attbl       .byte
    attbr       .byte
    xpos        .byte
    state       .byte
.endstruct

.struct Area            ;8b
    selfaddr1   .byte
    selfaddr2   .byte
    btaddr1     .byte
    btaddr2     .byte
    cmaddr1     .byte
    cmaddr2     .byte
    entaddr1    .byte
    entaddr2    .byte
.endstruct

.segment "STARTUP"

;I had to open up the zero page in nes.cfg to range from $0002 to $00FF. Idk if that will break something later.
.segment "ZEROPAGE"
gamestate:          .res 1  ;$00 - title, $01 - main game, $02 - paused, $03 - game over, $04 - cutscene, $05 - screen transition, $06 - PPUoff NT fill
controller:         .res 1    ;reserve 1 byte for controller input
drawcomplete:       .res 1
scrollx:            .res 1
scrolly:            .res 1
tilebufferA:        .res 32
tilebufferB:        .res 32
swap:               .res 1
buttonflag:         .res 1
counter:            .res 1
checkvar:           .res 1
colltemp1:          .res 1
colltemp2:          .res 1
colltemp3:          .res 1
animaframes:        .res 1
totalsprites:       .res 1
playerdata:         .res .sizeof(Entity)            ;15b
metatile:           .res .sizeof(Metatile)          ;11b
entitybuffer:       .res .sizeof(Entity)            ;15b
blockrow:           .res 16
spritemem:          .res 2
level:              .res .sizeof(Area)              ;8b
nextarea:           .res .sizeof(Area)              ;8b
nextareaid:         .res 1
btptr:              .res 2
cmptr:              .res 2
entptr:             .res 2
exitdir:            .res 1      ;1- up, 2 - down, 3 - left, 4 - right
;ptr:                .res 2
;total zero page reserved: 161b (max 254)

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

    LDX #$00
    LDY #$00
WriteAreaBank:      ;Write all areas PRGROM addresses into RAM lookup table
                        ;Future optimization: if I keep a self-size byte in the AREA arrays, I can loop this and not hardcode.
    LDA #<AREA0
    STA AREABANK, x
    INX
    LDA #>AREA0
    STA AREABANK, x
    INX
    LDA #<AREA1
    STA AREABANK, x
    INX
    LDA #>AREA1
    STA AREABANK, x

    LDX #$00
    
InitializeFirstScreen:
    LDA AREABANK
    STA level+Area::selfaddr1
    LDA AREABANK+1
    STA level+Area::selfaddr2
    ;block table
    LDA level+Area::selfaddr1 
    CLC
    ADC #$07
    STA level+Area::btaddr1     ;OMG I can't believe on a certain write, this is landing dead on #$00, which is putting the ptr one page off.
    LDA level+Area::selfaddr2
    STA level+Area::btaddr2
    BCS IncrementBTHightByte
    JMP InitializeCollisionMap
IncrementBTHightByte:
    INC level+Area::btaddr2
;collision map
InitializeCollisionMap:
    LDA level+Area::selfaddr1
    CLC
    ADC #$64 ;#$64
    STA level+Area::cmaddr1
    LDA level+Area::selfaddr2
    STA level+Area::cmaddr2
    BCS IncrementCMHighByte
    JMP ContinueToInitPlayer
IncrementCMHighByte:
    INC level+Area::cmaddr2
ContinueToInitPlayer:
    LDX #$00
InitializePlayer:
    LDA #EntityType::PlayerType
    STA playerdata+Entity::type
    STA ENTITIES, x
    INX
    LDA #$20
    STA playerdata+Entity::xpos
    STA ENTITIES, x
    INX   
    LDA #$00
    STA playerdata+Entity::ypos
    STA ENTITIES, x
    INX
    LDA #%10100000  ;falling, facing right
    STA playerdata+Entity::state
    STA ENTITIES, x
    INX
    LDA #$00
    STA playerdata+Entity::tlspr
    STA ENTITIES, x
    INX
    LDA #$01
    STA playerdata+Entity::trspr
    STA ENTITIES, x
    INX
    LDA #$10
    STA playerdata+Entity::blspr
    STA ENTITIES, x
    INX
    LDA #$11
    STA playerdata+Entity::brspr
    STA ENTITIES, x
    INX
    LDA #$00
    STA playerdata+Entity::tlpal
    STA playerdata+Entity::trpal
    STA playerdata+Entity::blpal
    STA playerdata+Entity::brpal
    STA ENTITIES, x
    INX
    STA ENTITIES, x
    INX
    STA ENTITIES, x
    INX
    STA ENTITIES, x
    INX
    LDA #$00        ;no env
    STA playerdata+Entity::env
    STA ENTITIES, x
    INX
    STA ENTITIES, x     ;zero out meta x
    INX
    STA ENTITIES, x     ;zero out meta y
    INX
    LDA #$FF
    STA ENTITIES, x     ;end of array
    ;15 writes to ENTITIES array
    

;InitializeLastPiece:
    ;The only startingg entity is the last medallion piece
    ;LDA #EntityType::Treasure
    ;STA ENTITIES, x
    ;INX
    ;LDA #$E0    ;xpos
    ;STA ENTITIES, x
    ;INX
    ;LDA #$D0    ;ypos
    ;STA ENTITIES, x
    ;INX
    ;LDA #$80    ;state
    ;STA ENTITIES, x
    ;INX
    ;LDA #$26    ;tl sprite
    ;STA ENTITIES, x
    ;INX
    ;LDA #$27    ;tr sprite
    ;STA ENTITIES, x
    ;INX
    ;LDA #$36    ;bl prite
    ;STA ENTITIES, x
    ;INX
    ;LDA #$37    ;br sprite
    ;STA ENTITIES, x
    ;INX
    ;LDA #$01    ;palette
    ;STA ENTITIES, x
    ;INX
    ;STA ENTITIES, x
    ;INX
    ;STA ENTITIES, x
    ;INX
    ;STA ENTITIES, x
    ;INX
    ;STA ENTITIES, x ;env
    ;INX
    ;STA ENTITIES, x ;metax  -deprecate out to a zero page variable for player?
    ;INX
    ;STA ENTITIES, x ;metay
    ;INX
    ;LDA #$FF
    ;STA ENTITIES, x
    ;INX
    ;LDA #$0A ;player plus medallion pieces
    ;STA totalsprites
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

    LDA level+Area::btaddr1
    STA btptr
    LDA level+Area::btaddr2
    STA btptr+1
    LDA level+Area::cmaddr1
    STA cmptr
    LDA level+Area::cmaddr2
    STA cmptr+1
    LDA level+Area::entaddr1
    STA entptr
    LDA level+Area::entaddr2
    STA entptr+1
    JSR LoadNametable
    JSR LoadAttributes
    JSR LoadCollMap
    ;JSR LoadAreaEntities    ;Hold off until I add entities to the AREA0 array

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
CheckForGameState:       ;After player is written, see if game has entered "transition" state to move to next area
    LDA gamestate
    CMP #$05
    BEQ JumpToScreenTransition
    CMP #$06
    BNE DoneCheckForGameState
    JSR LoadNextArea
JumpToScreenTransition:
    JMP SCREENTRANLOOP   ;waitfordrawtocompleteST   ;do I need to do jump to a specific part of the loop?
DoneCheckForGameState:

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
    LDA playerdata+Entity::state
    AND #%00111100                     ;TODO: OPTIMIZE - save cycles by jumping
    ORA #%01000010          ;walking, facing left
    STA playerdata+Entity::state
    DEC playerdata+Entity::xpos   ;decrement x position
    INC animaframes
    JMP checkup ; don't allow for left and right at the same time (jump past checkright if left was pressed)

checkright:
    LDA controller
    AND #$01
    BEQ checkup
    LDA playerdata+Entity::state
    AND #%00111100
    ORA #%10000010          ;walking facing right
    STA playerdata+Entity::state
    INC playerdata+Entity::xpos
    INC animaframes

checkup:
    LDA controller
    AND #$08
    BEQ checkdown
    LDA playerdata+Entity::env
    CMP #$01    ;over a climbable
    BNE donecheckingdirectional
    LDA playerdata+Entity::state
    ORA #%00010000      ;set to climbing
    AND #%11011111      ;turn of falling
    STA playerdata+Entity::state
    DEC playerdata+Entity::ypos
    INC animaframes
;JumpToDone:
    JMP donecheckingdirectional ;jump past check down so not getting both simultaneous

checkdown:
    LDA controller
    AND #$04
    BEQ donecheckingdirectional
    LDA playerdata+Entity::env
    CMP #$01    ;over a climbable
    BNE donecheckingdirectional
    LDA playerdata+Entity::state
    ORA #%00010000      ;set to climbing
    AND #%11011111      ;turn of falling
    STA playerdata+Entity::state
    INC playerdata+Entity::ypos
    INC animaframes
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
    ;LDA scrolly
    ;SEC
    ;SBC #$02
    ;STA scrolly
    ;CMP #$00
    ;BNE donescroll
    ;LDA #$EE
    ;STA scrolly
    ;LDA swap
    ;EOR #$02
    ;STA swap
donescroll:

processplayer:
FindMetaPosition:
    LDA playerdata+Entity::xpos
    LSR
    LSR
    LSR
    LSR
    STA playerdata+Entity::metax
    LDA playerdata+Entity::ypos
FinishSetMetaY:
    LSR
    LSR
    LSR
    LSR
    STA playerdata+Entity::metay
LoadState:
;check climbing state
    LDA playerdata+Entity::state
    TAX
    AND #%00010000    ;check if climbing
    CMP #%00010000
    BNE CheckWalkState  ;if not, jump out
    LDA playerdata+Entity::env
    CMP #$01
    BEQ SetClimbing  ;if still on a climbable, jump out
    TXA
    AND #%11101111      ;turn off climbing
    STA playerdata+Entity::state
SetClimbing:
    LDA #$08
    STA playerdata+Entity::tlspr
    LDA #$09
    STA playerdata+Entity::trspr
    LDA #$18
    STA playerdata+Entity::blspr
    LDA #$19
    STA playerdata+Entity::brspr
    LDA animaframes
    LSR
    LSR
    LSR
    LSR
    BCS SetClimbingTwo
SetClimbingOne:
    LDA playerdata+Entity::state
    ORA #%10000000
    AND #%10111111
    STA playerdata+Entity::state
    JMP CheckFalling
SetClimbingTwo:
    LDA playerdata+Entity::state
    ORA #%01000000
    AND #%01111111
    STA playerdata+Entity::state
    JMP CheckFalling
;check walk state
CheckWalkState:
    LDA playerdata+Entity::state
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
    STA playerdata+Entity::tlspr
    LDA #$03
    STA playerdata+Entity::trspr
    LDA #$12
    STA playerdata+Entity::blspr
    LDA #$13
    STA playerdata+Entity::brspr
    JMP CheckFalling
SetWalkTwo:
    LDA #$04
    STA playerdata+Entity::tlspr
    LDA #$05
    STA playerdata+Entity::trspr
    LDA #$14
    STA playerdata+Entity::blspr
    LDA #$15
    STA playerdata+Entity::brspr

;check fall state
CheckFalling:
    LDA playerdata+Entity::state
    AND #%00010000
    CMP #$10    ;is player climbing?
    BEQ EndProcessPlayer
    LDA playerdata+Entity::state
    AND #%00100000
    ;bitmask against falling %00100000 if necessary
    CMP #$20
    BEQ PlayerFall
;check stand state
    LDA playerdata+Entity::state
    AND #%00000010
    CMP #$00
    BEQ PlayerIdle
    JMP EndProcessPlayer
PlayerFall:
    ;to get it working, fall at a rate of 2 pps. Math later.
    INC playerdata+Entity::ypos
    INC playerdata+Entity::ypos
    JMP EndProcessPlayer
PlayerIdle:
    LDA #$00
    STA animaframes
    STA playerdata+Entity::tlspr
    LDA #$01
    STA playerdata+Entity::trspr
    LDA #$10
    STA playerdata+Entity::blspr
    LDA #$11
    STA playerdata+Entity::brspr
EndProcessPlayer:
    ;update ENTITIES array with current player data
    ;first add playerdata to entitiesbuffer. this is inefficient in cycles, but to have a duplicate subroutine is inefficient in memory.
    ;TODO: find solution to above issue
    LDX #$00
    LDY #$00
CopyPDtoEBLoop:
    LDA playerdata, x
    STA entitybuffer, x
    INX
    INY
    CPY #$0F    ;15 items in entity struct
    BNE CopyPDtoEBLoop
    LDX #$01    ;X = ENTITIES index of player + 1
    LDA #$01
    STA checkvar    ;set checkvar to 1 so subroutine does not mess with stack
    JSR WriteEntityFromBuffer ;Subroutine to write all the changes made to player this frame
CheckCollisions:;check for collisions from new position
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


;When switching screens, get out of game loop entirely and use SCREENTRANLOOP
;Both PPU nametables should be loaded up by using the TransitionScreen BG loading subroutine
SCREENTRANLOOP:
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP

    NOP
    NOP
InitSpritesST:
    LDY #$00
    LDA #$FF
InitSpriteLoopST:
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
    BEQ ContinueTransition
    JMP InitSpriteLoopST
ContinueTransition:
    LDA exitdir
    CMP #$02
    BNE waitfordrawtocompleteST     ;change as more directions added ofc
TransitionDown: ;downward exit means screen scrolls up, scrolly will decrement
    INC scrolly
    INC scrolly ;where the magic happens
    DEC playerdata+Entity::ypos
    DEC playerdata+Entity::ypos
    LDA playerdata+Entity::ypos
    SEC
    SBC #$FD
    BCC DoneTransitionDown
    LDA #$00
    STA playerdata+Entity::ypos
DoneTransitionDown:
    LDX #$00
CopyPDtoEBLoopST:
    LDA playerdata, x
    STA entitybuffer, x
    INX
    INY
    CPY #$0F    ;15 items in entity struct
    BNE CopyPDtoEBLoopST
    LDX #$01    ;X = ENTITIES index of player + 1
    LDA #$01
    STA checkvar    ;set checkvar to 1 so subroutine does not mess with stack
    JSR WriteEntityFromBuffer ;Subroutine to write all the changes made to player this frame
    LDA scrolly
    CMP #$F0
    BNE waitfordrawtocompleteST
ResetScrollY:
    LDA swap
    EOR #$02
    STA swap
    LDA #$00
    STA scrolly
    LDA #$01                    ;set gamestate to main game
    STA gamestate
    JMP waitfordrawtocomplete   ;back to main GAMELOOP
waitfordrawtocompleteST:
    LDA drawcomplete
    CMP #$01
    BNE waitfordrawtocompleteST
    LDA #$00
    STA drawcomplete

    JMP SCREENTRANLOOP



;;;;; ----- Logical subroutines ------ ;;;;;;
CheckPlayerCollisionDown: ;IN <--- direction of check (Y)
    PHA
    TXA
    PHA
    TYA
    PHA
    ;I still want to try this with a check one pixel in from the edge. can I use the stack for my vars?
    LDA playerdata+Entity::xpos
    CLC
    ADC #$02    ;one pixel in from left edge - x1
    TAX         ;store in X reg
    CLC
    ADC #$0C    ;plus 14 more to get one pixel in from right edge - x2
    TAY         ;store on Y
    LDA playerdata+Entity::ypos
    CLC
    ADC #$10    ;y pos of player's bottom edge (feet) - y
    CMP #$FD    ;Bottom of screen?
    BEQ ExitDown
    CMP #$FE
    BEQ ExitDown
    LSR         ;divide by 16
    LSR         ;meta y of player's feet
    LSR
    LSR
    ASL
    ASL
    STA checkvar         ;store in checkvar
    ;Get collmap position of (x1,y) and (x2, y) then check if tile below is solid. If both are NOT solid (== $01), then fall.
    ;1) Divide x by 16
    ;2) That number + y = test cell
    ;3) if collmap, test cell is $01, solid.
TestX1:
    TXA
    LSR
    LSR
    LSR
    LSR     ;meta x
    PHA
    LSR
    LSR     ;collmap x
    STA colltemp2
    LDA #$00
    STA colltemp3
    JMP ContinueTest
TestX2:
    TYA
    LSR
    LSR
    LSR
    LSR
    PHA
    LSR
    LSR
    STA colltemp2
    LDA #$01
    STA colltemp3
ContinueTest:
    LDA colltemp2   ;0, 1, 2 or 3
    CLC
    ADC checkvar    ;x1 (col) + target y (row)  ; multiple of 4
    TAX
    LDA COLLMAPBANK, x ;get target byte
    STA colltemp1
    PLA     ;get back meta x
    AND #%00000011
    CMP #$00
    BEQ MaskOutZero
    CMP #$01
    BEQ MaskOutOne
    CMP #$02
    BEQ MaskOutTwo
    CMP #$03
    BEQ MaskOutThree ;may be to far to branch
    JMP ReturnFromCollisionDown
ExitDown:
    LDA #$02
    STA exitdir
    LDY #$02        ;offset to "down" exit id
    LDA (level+Area::selfaddr1), y  ;exit id is the selfid of the next area, which should also be the offset into the AREABANK array
    STA nextareaid  ;get id of next area 
    JSR SetupNextArea   ;populate "nextarea" struct for draw routine.
    LDA #$06        ;Load next area game state
    STA gamestate
    JMP ReturnFromCollisionDown
MaskOutZero:
    LDA colltemp1
    AND #%11000000
    LSR
    LSR
    LSR
    LSR
    LSR
    LSR
    JMP Resolve
MaskOutOne:
    LDA colltemp1
    AND #%00110000
    LSR
    LSR
    LSR
    LSR
    JMP Resolve
MaskOutTwo:
    LDA colltemp1
    AND #%00001100
    LSR
    LSR
    JMP Resolve
MaskOutThree:
    LDA colltemp1
    AND #%00000011
Resolve:
    CMP #$01
    BEQ ReturnSolid     ;if x1 or x2 is over solid, we can stop here
    ;CMP #$11            ;if x1 or x2 is over damaging, we can resolve?
    ;BEQ SetX1Damaging
    CMP #$10            ;climbable?
    BEQ SetClimbable
ContinueResolve:
    LDA colltemp3       ;holding whether we checked x1 or x2
    CMP #$01            ;if both points checked and no solid below, we must be falling
    BEQ SetFalling
    JMP TestX2
SetClimbable:
    LDA playerdata+Entity::env
    ORA #%00010000
    STA playerdata+Entity::env
    JMP ContinueResolve
SetFalling:
    LDA playerdata+Entity::state
    ORA #%00100000  ;turn on falling
    AND #%11111110  ;turn off standing
    STA playerdata+Entity::state
    JMP ReturnFromCollisionDown
ReturnSolid:            ;resolve collision here. in future, may be wise to separate resolutinon from check
    LDA playerdata+Entity::ypos
    AND #%11110000      ;return to "top" of metatile by zeroing out the low nibble (any pixels "below" the multiple of 16)
    STA playerdata+Entity::ypos
    LDA playerdata+Entity::state
    ORA #%00000001      ;set status to standing
    AND #%11011111      ;if falling, stop falling
    STA playerdata+Entity::state
ReturnFromCollisionDown:
    PLA
    TAY
    PLA
    TAX
    PLA
    RTS


;----------------------------------------------------------------------------------------------------
CheckPlayerCollisionLeft:
    PHA
    TXA
    PHA
    LDA playerdata+Entity::metax
    LSR
    LSR
    STA colltemp1
    LDA playerdata+Entity::metay
    ASL
    ASL
    CLC
    ADC colltemp1   ;Add x byte to Y byte to find byte location in collmap
    STA colltemp2   ;Location of player byte in coll map
    ;find bit pair
    LDA playerdata+Entity::metax
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
    INC playerdata+Entity::xpos
    JMP ReturnFromCollL
MaskOutOneL:
    LDX colltemp2
    LDA COLLMAPBANK, x
    AND #%00110000 ;the bit set to the left of three
    CMP #%00000000
    BEQ ReturnFromCollL
    CMP #%00100000
    BEQ ReturnFromCollL
    INC playerdata+Entity::xpos
    JMP ReturnFromCollL
MaskOutTwoL:
    LDX colltemp2
    LDA COLLMAPBANK, x
    AND #%00001100 ;the bit set to the left of three
    CMP #%00000000
    BEQ ReturnFromCollL
    CMP #%00001000
    BEQ ReturnFromCollL
    INC playerdata+Entity::xpos
    JMP ReturnFromCollL
MaskOutThreeL:
    LDX colltemp2
    LDA COLLMAPBANK, x
    AND #%00000011 ;the bit set to the left of three
    CMP #%00000000
    BEQ ReturnFromCollL
    CMP #%00000010
    BEQ ReturnFromCollL
    INC playerdata+Entity::xpos
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
    LDA playerdata+Entity::metax
    CLC
    ADC #$01
    LSR
    LSR
    STA colltemp1
    LDA playerdata+Entity::metay
    ASL
    ASL
    CLC
    ADC colltemp1   ;Add x byte to Y byte to find byte location in collmap
    STA colltemp2   ;Location of player byte in coll map
    ;find bit pair
    LDA playerdata+Entity::metax
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
    DEC playerdata+Entity::xpos
    JMP ReturnFromCollR
MaskOutOneR:
    LDX colltemp2
    LDA COLLMAPBANK, x
    AND #%00001100 ;the bit set to the left of three
    CMP #%00000000
    BEQ ReturnFromCollR
    CMP #%00001000
    BEQ ReturnFromCollR
    DEC playerdata+Entity::xpos
    JMP ReturnFromCollR
MaskOutTwoR:
    LDX colltemp2
    LDA COLLMAPBANK, x
    AND #%00000011 ;the bit set to the left of three
    CMP #%00000000
    BEQ ReturnFromCollR
    CMP #%00000010
    BEQ ReturnFromCollR
    DEC playerdata+Entity::xpos
    JMP ReturnFromCollR
MaskOutThreeR:
    LDX colltemp2
    LDA COLLMAPBANK, x
    AND #%11000000 ;the bit set to the left of three
    CMP #%00000000
    BEQ ReturnFromCollR
    CMP #%10000000
    BEQ ReturnFromCollR
    DEC playerdata+Entity::xpos
ReturnFromCollR:
    PLA
    TAX
    PLA
    RTS
;------------------------------------------------------------------------------------------------------------
CheckPlayerCollisionOver:
    PHA
    TXA
    PHA
    LDA playerdata+Entity::xpos
    CLC
    ADC #$08
    LSR
    LSR
    LSR
    LSR
    LSR
    LSR
    STA colltemp1
    LDA playerdata+Entity::ypos
    ;Trying to fix climb screen transition bug here
    LSR
    LSR
    LSR
    LSR
    ASL
    ASL
    CLC
    ADC colltemp1   ;Add x byte to Y byte to find byte location in collmap
    STA colltemp2   ;Location of player byte in coll map
    ;find bit pair
    LDA playerdata+Entity::xpos
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
    STA playerdata+Entity::env
    JMP ReturnFromCollO
MaskOutOneO:
    LDX colltemp2
    LDA COLLMAPBANK, x
    AND #%00110000 ;the bit set to the left of three
    CMP #%00100000
    BNE NoEnvCondition
    LDA #$01            ;01 will mean over a climbable
    STA playerdata+Entity::env
    JMP ReturnFromCollO
MaskOutTwoO:
    LDX colltemp2
    LDA COLLMAPBANK, x
    AND #%00001100 ;the bit set to the left of three
    CMP #%00001000
    BNE NoEnvCondition
    LDA #$01            ;01 will mean over a climbable
    STA playerdata+Entity::env
    JMP ReturnFromCollO
MaskOutThreeO:
    LDX colltemp2
    LDA COLLMAPBANK, x
    AND #%00000011 ;the bit set to the left of three
    CMP #%00000010
    BNE NoEnvCondition
    LDA #$01            ;01 will mean over a climbable
    STA playerdata+Entity::env
    JMP ReturnFromCollO
NoEnvCondition:
    LDA #$00
    STA playerdata+Entity::env
ReturnFromCollO:
    PLA
    TAX
    PLA
    RTS


;------------------Entity Change Direction Subroutine------------------------------
ChangePlayerFacing: ;push A and Y, doesn't use
    PHA
    TYA
    PHA
    LDA playerdata+Entity::tlspr
    TAY
    LDA playerdata+Entity::trspr
    STA playerdata+Entity::tlspr
    TYA
    STA playerdata+Entity::trspr
    LDA playerdata+Entity::blspr
    TAY
    LDA playerdata+Entity::brspr
    STA playerdata+Entity::blspr
    TYA
    STA playerdata+Entity::brspr
    LDA playerdata+Entity::tlpal
    EOR #$04
    STA playerdata+Entity::tlpal
    LDA playerdata+Entity::trpal
    EOR #$04
    STA playerdata+Entity::trpal
    LDA playerdata+Entity::blpal
    EOR #$04
    STA playerdata+Entity::blpal
    LDA playerdata+Entity::brpal
    EOR #$04
    STA playerdata+Entity::brpal
    PLA
    TAY
    PLA
    RTS


;--*--*--*--*--*--*--*--*--* STORE ENTITIES SUBROUTINE   *--*--*--*--*--*--*--*--*--*--*--
StoreEntity: ;full subroutine, checkvar = #$00
    PHA
    TXA
    PHA
    TYA
    PHA
    LDX #$00
    LDY #$00
FindEndOfArray:
    LDA ENTITIES, x
    CMP #$FF
    BNE FindEndOfArray
    INX
WriteEntityFromBuffer: ;can this loop with y?   Call this is a subroutine to update. Set checkvar to #$01 (or anything but 00)
;IN <--- X = index of ENTITIES array + 1
    DEX     ;overwrite $FF
    LDA entitybuffer+Entity::type
    STA ENTITIES, x
    INX
    LDA entitybuffer+Entity::xpos
    STA ENTITIES, x
    INX
    LDA entitybuffer+Entity::ypos
    STA ENTITIES, x
    INX
    LDA entitybuffer+Entity::state
    STA ENTITIES, x
    INX
    LDA entitybuffer+Entity::tlspr
    STA ENTITIES, x
    INX
    LDA entitybuffer+Entity::trspr
    STA ENTITIES, x
    INX
    LDA entitybuffer+Entity::blspr
    STA ENTITIES, x
    INX
    LDA entitybuffer+Entity::brspr
    STA ENTITIES, x
    INX
    LDA entitybuffer+Entity::tlpal
    STA ENTITIES, x
    INX
    LDA entitybuffer+Entity::trpal
    STA ENTITIES, x
    INX
    LDA entitybuffer+Entity::blpal
    STA ENTITIES, x
    INX
    LDA entitybuffer+Entity::brpal
    STA ENTITIES, x
    INX
    LDA entitybuffer+Entity::env
    STA ENTITIES, x
    INX
    LDA entitybuffer+Entity::metax
    STA ENTITIES, x
    INX
    LDA entitybuffer+Entity::metay
    STA ENTITIES, x
    LDA checkvar
    CMP #$00
    BNE ReturnFromStoreEntity
    INX
    LDA #$FF
    STA ENTITIES, x
    PLA
    TAY
    PLA
    TAX
    PLA
ReturnFromStoreEntity:
    RTS



;-------------------Load Screen Subroutines----------------------------------------------------------
SetupNextArea:
    PHA
    TXA
    PHA
    TYA
    PHA
    LDX nextareaid
    LDA AREABANK, x
    STA nextarea+Area::selfaddr1
    INX
    LDA AREABANK, x
    STA nextarea+Area::selfaddr2
    LDY #$05                            ;First byte of "contents" section in AREA array. Contains offset of the block table
    LDA (nextarea+Area::selfaddr1), y   ;get block table offset (probably $07)
    STA nextarea+Area::btaddr1          ;use as variable for a moment
    LDA nextarea+Area::selfaddr1
    CLC
    ADC nextarea+Area::btaddr1
    STA nextarea+Area::btaddr1
    ;TAY                                 ;put that value into X
    ;LDA (nextarea+Area::selfaddr1), y     ;get the value of X added to lowbyte of the array address. Should be first byte in block table.
    ;STA nextarea+Area::btaddr1
    LDA nextarea+Area::selfaddr2        ;may eventually need to robustify for if btaddr crosses to next memory page, selfaddr2 will need to INC
    STA nextarea+Area::btaddr2
    BCS NAIncrementHighByteBT
    JMP NAContinueToLoadCM
NAIncrementHighByteBT:
    INC nextarea+Area::btaddr2
NAContinueToLoadCM:
    LDY #$06
    LDA (nextarea+Area::selfaddr1), y   ;get collmap offset, repeat process
    STA nextarea+Area::cmaddr1          ;use as variable for a moment
    LDA nextarea+Area::selfaddr1
    CLC
    ADC nextarea+Area::cmaddr1
    STA nextarea+Area::cmaddr1
    LDA nextarea+Area::selfaddr2        ;may eventually need to robustify for if btaddr crosses to next memory page, selfaddr2 will need to INC
    STA nextarea+Area::cmaddr2
    BCS NAIncrementHighByteCM
    JMP NAContinueToLoadEntities
NAIncrementHighByteCM:
    INC nextarea+Area::cmaddr2
NAContinueToLoadEntities:
    LDY #$07
    LDA (nextarea+Area::selfaddr1), y   ;get collmap offset, repeat process
    STA nextarea+Area::entaddr1          ;use as variable for a moment
    LDA nextarea+Area::selfaddr1
    CLC
    ADC nextarea+Area::entaddr1
    STA nextarea+Area::entaddr1
    LDA nextarea+Area::selfaddr2        ;may eventually need to robustify for if btaddr crosses to next memory page, selfaddr2 will need to INC
    STA nextarea+Area::entaddr2
    BCS NAIncrementHighByteEntities
    JMP NAReturn
NAIncrementHighByteEntities:
    INC nextarea+Area::entaddr2
NAReturn:
    ;Done. "nextarea" struct is populated.
    PLA
    TAY
    PLA
    TAX
    PLA
    RTS

;Background Draw methods
;Upon game state changing to transition, we need to fill both nametables in $2007 then scroll between them. We'll receive the BLOCKTABLE
;COLLMAP for the new screen and hold them in a pointer somewhere, or a reference variable for a pointer. Then LoadTransition can fill both
;nametables in PPU memory and increment the correct scroll, h or v (First I'm only messing around with v). The idea is to get a Zelda-like
;screen transition.
;LoadNametable loads one name table (not both). Need to make a way to call for "level" then "nextarea" when transitioning screens

LoadNametable:
    PHA
    TXA
    PHA
    TYA
    PHA
;Background filling routine begins
;This section will get subbed out for metatile implementation
;For simplicity, I'm gonna only use one palette for the BG

;Begin with canvas tile fill
;Ok, can't do that because of the write order, unless maybe I build an array in RAM then store it.
;I need a way to check against the block table for each location.

;Instead of filling the whole nametable at once, I'm going to try writing to a buffer. We'll have a toggle as to whether
;it's writing the top side or bottom side of the row. This may go slow, but let's see if it works first. If it all fits b/t v-blank, then
;great, though if I find performance issues later, optimizing here should be my first move.
    ;LDA #<BLOCKTABLE0   ;high byte
    ;STA ptr+0
    ;LDA #>BLOCKTABLE0   ;low byte
    ;STA ptr+1
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
    LDA (btptr), y
    CMP #$FF
    BEQ BlocksEndOfRow
    STA blockrow, x      
    INX
    INY
    ;LDA btptr, y
    ;CMP #$FF
    ;BNE ContinueGetBlocks
    ;INC btptr+1
ContinueGetBlocks:
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
    CPX #$0F       ;15 metarows
    BEQ FinishedBGWrite
FinishedBuffer:
    TXA
    PHA
    LDY counter
    JMP ClearBlockRow
FinishedBGWrite:
    PLA
    TAY
    PLA
    TAX
    PLA
    RTS
;----------------------------------------------------------------------------------------
LoadAttributes:
    PHA
    TXA
    PHA
    TYA
    PHA

    LDA $2002       ;reset latch
    LDA #$23        ;High byte of $23CO address (attributes)
    STA $2006
    LDA #$C0        ;Low byte
    STA $2006
    LDX #$40        ;Fill with 64b
    LDA #$00        ;Attribute value
LoadAttributesLoop:
    STA $2007
    DEX
    BNE LoadAttributesLoop
ReturnFromLoadAttributes:
    PLA
    TAY
    PLA
    TAX
    PLA
    RTS

;--*--*--*--*--*--*--*Load COllision Map subroutine*--*--*--*--*--*--*--*--*--*
LoadCollMap:
    PHA
    TXA
    PHA
    TYA
    PHA
    LDY #$00
    LDX #$00
WriteCMLoop:
    LDA (cmptr), y
    STA COLLMAPBANK, x
    INY
    INX
    CPY #$3C    ;60 bytes in COLLMAP0
    BNE WriteCMLoop
ReturnFromLoadCollMap:
    PLA
    TAY
    PLA
    TAX
    PLA
    RTS

;--*--*--*--*--*--*--*Load Entities subroutine*--*--*--*--*--*--*--*--*--*
;Should I use the StoreEntities subroutine or bypass it? It should save a handful of cycles to do
;everything here, but a bit redundant. Start with using subroutine, if need to optimize out later, fine.
;This way, all entities are stored in the same fashion, less likely to have bugs.
;On second thought, this needs to clear all entities except the player, so the loop will be written into the subroutine, starting from byte $10
LoadAreaEntities:
    PHA
    TXA
    PHA
    TYA
    PHA
    LDX #$10    ;first address after player entity data
    LDY #$00    
WriteEntitiesLoop:  ;type, x, y, state, tlspr, trspr, blspr, brspr, tlpal, trpal, blpal, brpal, env, metax, metay
    LDA (entptr), y
    STA ENTITIES, x
    INX
    INY
    LDA (entptr), y
    AND #%11110000  ;xpos will by the high byte alone. Always a multiple of 16(meta position)
    STA ENTITIES, x
    INX
    LDA (entptr), y
    ASL
    ASL
    ASL
    ASL             ;move low byte into high byte for y pos
    STA ENTITIES, x
    LDA #$00        ;state
    STA ENTITIES, x
    ;START HERE
ReturnFromLoadEntities:
    PLA
    TAY
    PLA
    TAX
    PLA
    RTS

;------------------------------Load NextArea to 2007-----------------------------------------
LoadNextArea:
    PHA
    TXA
    PHA
    TYA
    PHA
    
    LDA $2000
    AND #%01111111
    STA $2000
    LDA #$00
    STA $2001
    LDA $2002           ;PPUSTATUS    Is he reading to clear vblank flag? Neads to be changed to use NMI instead
    LDA #$20
    STA $2006           ;PPUADDR      $2000 for nametable1
    LDA #$00
    STA $2006           ;PPUADDR
FillNametables:
    LDA level+Area::btaddr1
    STA btptr
    LDA level+Area::btaddr2
    STA btptr+1
    JSR LoadNametable
    LDA #$28
    STA $2006           ;PPUADDR      $2800 for nametable 2
    LDA #$00
    STA $2006           ;PPUADDR

    LDA nextarea+Area::btaddr1
    STA btptr
    LDA nextarea+Area::btaddr2
    STA btptr+1
    JSR LoadNametable

    LDA #$23
    STA $2006           ;PPUADDR      nametable1 attribute layer
    LDA #$C0
    STA $2006           ;PPUADDR
    JSR LoadAttributes  
    JSR LoadAttributes ;currently not changing anything about the attributes
FillCollMap:
    ;LDA level+Area::cmaddr1
    ;STA cmptr
    ;LDA level+Area::cmaddr2
    ;STA cmptr+1
    ;JSR LoadCollMap
    LDA nextarea+Area::cmaddr1
    STA cmptr
    LDA nextarea+Area::cmaddr2
    STA cmptr+1
    JSR LoadCollMap
    LDA #$1E
    STA $2001
    LDA $2000
    ORA #%10000000
    STA $2000
ReturnFromLoadNextArea:    
    LDA #$05
    STA gamestate    ;Go to screen transition
    PLA
    TAY
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
    LDA #$00        ;low byte of graphics page $0200
    LDY #$00        ;y will index the OAM memory location (putter)
    STA spritemem
    LDA #$02        ;high byte of graphics page $0200
    STA spritemem+1

DRAWENTITIES:   
BeginLoadEntityLoop:
    LDX #$00
LoadEntityLoop:
    INX ;go past type
    LDA ENTITIES, x     ;xpos
    STA metatile+Metatile::xpos
    INX
    LDA ENTITIES, x     ;ypos
    STA metatile+Metatile::ypos
    INX
    LDA ENTITIES, x
    STA metatile+Metatile::state
    INX 
    LDA ENTITIES, x     ;tlspr
    STA metatile+Metatile::spritetl
    INX
    LDA ENTITIES, x     ;trspr
    STA metatile+Metatile::spritetr
    INX
    LDA ENTITIES, x     ;blspr
    STA metatile+Metatile::spritebl
    INX
    LDA ENTITIES, x     ;brspr
    STA metatile+Metatile::spritebr
    INX
    LDA ENTITIES, x     ;tlpal
    STA metatile+Metatile::atttl
    INX
    LDA ENTITIES, x     ;trpal
    STA metatile+Metatile::atttr
    INX
    LDA ENTITIES, x     ;blpal
    STA metatile+Metatile::attbl
    INX
    LDA ENTITIES, x     ;brpal
    STA metatile+Metatile::attbr
    INX                 ;env
    INX                 ;metax
    INX                 ;metay
    INX                 ;next entity::type OR stop byte
CheckMetatileFacing:
    LDA metatile+Metatile::state
    AND #%11000000
    CMP #%10000000
    BNE DRAWMETATILEHRZMIRROR
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
    JMP ClearMetatile
DRAWMETATILEHRZMIRROR: ;in--struct of meta tile data
;swap tiles and set horizontal flip
    LDA metatile+Metatile::ypos ; y
    STA (spritemem), y
    INY
    LDA metatile+Metatile::spritetr ; tile
    STA (spritemem), y
    INY
    LDA metatile+Metatile::atttl ; palette etc
    ORA #$40
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
    LDA metatile+Metatile::spritebr ; tile
    STA (spritemem), y
    INY
    LDA metatile+Metatile::attbl   ;palette
    ORA #$40
    STA (spritemem), y
    INY
    LDA metatile+Metatile::xpos ; x position
    STA (spritemem), y
    INY

    ;top right sprite
    LDA metatile+Metatile::ypos
    STA (spritemem), y
    INY
    LDA metatile+Metatile::spritetl     ;same as top left but we will flip it and add 8 to xpos
    STA (spritemem), y
    INY
    LDA metatile+Metatile::atttr     ;palette %01000001   palette 1, flip horizontal
    ORA #$40
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
    LDA metatile+Metatile::spritebl   
    STA (spritemem), y
    INY
    LDA metatile+Metatile::attbr   ;palette with h-flip
    ORA #$40
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
    LDA ENTITIES, x
    CMP #$FF
    BEQ DONESPRITE
    JMP LoadEntityLoop

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

    ;when ready for scrolling background, only if screen is transitioning
    LDA gamestate
    CMP #$05        ;conditional may be unneccesary if scrollx/scrolly don't change
    BNE SwapScreen
    LDA scrollx
    STA $2005
    LDA scrolly
    STA $2005

SwapScreen:
    LDA #%10001000
    ORA swap
    LDX $2002   ;clear the register before
    STA $2000

donewithppu:
    LDA #$01
    STA $07FF

    ;pull registers from stack - Y, X, P, A
    PLA 
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
    .byte $0E, $1C, $2B, $39 ;palette 2  ;pastel green, blue-green, blue
    .byte $0A, $05, $26, $30 ;palette 3  ;green, crimson, pink, white
    .byte $0E, $13, $23, $33 ;palette 4   ;purples 

    ;bg palettes
    .byte $0F, $06, $15, $18 ;palette 1  ;browns, golds, reds, bg1
    .byte $0E, $06, $15, $36 ;palette 2   ;crimson, red, pink    bullet/enemy
    .byte $0E, $07, $1C, $37 ;palette 3  ;browns and blue - main char
    .byte $0E, $13, $23, $33 ;palette 4   ;purples
    
    ;TODO: potential for compressing again by half:
    ;If I have a row-ender, I don't need a Y value, so II can pack two blocks into one byte: X1{0000}X2{0000}

;--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*
;--*--*--*--*--*--*--*--*--Game areas ROM--*--*--*--*--*--*--*--*--*--*--*
;--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*

AREA0:  ;How to traverse these memory chunks. Start with a little "table of contents"? A few bytes to load as an adder to each segment?
SELFID0:
    .byte $00
EXITS0:          ;$FF is no exit
    .byte $FF   ;up
    .byte $02   ;down   Exits have to be a multiple of 2
    .byte $FF   ;left
    .byte $FF   ;right
CONTENTS0:
    .byte $07   ;offset to blocktable
    .byte $64   ;offset to collmap
BLOCKTABLE0: ;These blocks are all in mem location 0 and will never flip, so all they need is position X{0000}  --Y{0000}-- Actually they don't need Y
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
    .byte $00, $10, $20, $30, $40, $50, $60, $70, $80, $90, $A0, $B0, $C8, $D0, $E0, $F0, $FF
;100 offset (100?)
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
    .byte %01010101, %01010101, %01010101, %10010101

AREA1:
SELFID1:
    .byte $02   ;RAM lookup table place number. By 2s for storing two-byte PRGROM addresses
EXITS1:
    .byte $00   ;up
    .byte $FF   ;down
    .byte $FF   ;left
    .byte $FF   ;right
CONTENTS1:
    .byte $08   ;offset to blocktable (maybe unecessary if nothing is variable length before the block table)
    .byte $5D   ;offset to collmap
    .byte $99   ;offset to entity list
BLOCKTABLE1:
    .byte $00, $10, $20, $30, $40, $50, $60, $70, $80, $90, $A0, $B0, $C8, $D0, $E0, $F0, $FF
    .byte $00,                                                        $C8,           $F0, $FF
    .byte $00,                                                        $C8,           $F0, $FF
    .byte $00,                                                        $C8,           $F0, $FF
    .byte $00,                                                        $C8,           $F0, $FF
    .byte $00,                                                                       $F0, $FF
    .byte $00,                                                                       $F0, $FF
    .byte $00,                                                   $B0, $C0, $D0,      $F0, $FF
    .byte $00,                                                                       $F0, $FF                              
    .byte $00,                                                                       $F0, $FF
    .byte $00,                                                                       $F0, $FF
    .byte $00,                               $70, $80, $90, $A0, $B0,                $F0, $FF
    .byte $00,                                                                       $F0, $FF
    .byte $00,                                                                       $F0, $FF
    .byte $00, $10, $20, $30, $40, $50, $60, $70, $80, $90, $A0, $B0, $C0, $D0, $E0, $F0, $FF
;+84 to collmap
;2-bit collision map: 00 - nothing, 01 - solid, 10 - climbable, 11 - damaging.
COLLMAP1:
    .byte %01010000, %01010101, %01010101, %10010101 ;60
    .byte %01000000, %00000000, %00000000, %10000001    ;64
    .byte %01000000, %00000000, %00000000, %10000001    ;68
    .byte %01000000, %00000000, %00000000, %10000001    ;6C
    .byte %01000000, %00000000, %00000000, %10000001    ;70
    .byte %01000000, %00000000, %00000000, %00000001
    .byte %01000000, %00000000, %00000000, %00000001
    .byte %01000000, %00000000, %00000001, %01010001
    .byte %01000000, %00000000, %00000000, %00000001    ;80
    .byte %01000000, %00000000, %00000000, %00000001
    .byte %01000000, %00000000, %00000000, %00000001
    .byte %01000000, %00000001, %01010101, %00000001
    .byte %01000000, %00000000, %00000000, %00000001    ;90
    .byte %01000000, %00000000, %00000000, %00000001
    .byte %01010101, %01010101, %01010101, %01010101    ;98
ENTLIST1:
    .byte $02, $ED      ;type (see EntityType struct), $XY meta location
    .byte $FF           ;End of list


;--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*
;--*--*--*--*--*--*--*--*--Sprite Look-up Table ROM*--*--*--*--*--*--*--*
;--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*
SPRITELOOKUP:   ;first square (tl) of each metatile
    .byte $00, $02, $04, $06, $08   ;player
    .byte $20                      ;medallion top-left (5)
    .byte $22                      ;medallion top-right (6)
    .byte $24                      ;medallion bottom-left (7)
    .byte $26                      ;medallion bottom-right (8)
    .byte $0E                      ;hat     
;START HERE!!!
PALETTELOOKUP:
    .byte $00, $FF
    .byte $01, $FF
    .byte $01, $FF
    .byte $01, $FF
    .byte $01, $FF


.segment "VECTORS"
    .word VBLANK
    .word RESET
    .word 0

.segment "CHARS"
    .incbin "sprites.chr"