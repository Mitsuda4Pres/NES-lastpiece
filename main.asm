;TODO: 8/7 - Fix climb down bug on screen 3 that pushes player left
;   8/7 - Auto jump bug when landing on vine
;   8/7 - Collision bug when trying to jump sideways through a one tile opening.
;   8/7 - When climbing to top of vine, character "vibrates". Make it so he just chills until jumping off
;   8/7 - "Vibrates" when vertical in a one tile wide space


.segment "HEADER"

    .byte "NES" 
    .byte $1a
    .byte $02       ; 4 - 2*16k PRG ROM (NROM 256)
    .byte $01       ; 5 - 8k CHR ROM
    .byte %00000000 ; 6 - mapper - horizontal mirroring
    .byte $00       ; 7
    .byte $00       ; 8 - 
    .byte $00       ; 9 - NTSC
    .byte $00
    ; filler
    .byte $00,$00,$00,$00,$00

;Can define game constants here, such as a global structure or variables
;Game Constants (make sure to use #CONSTANT when calling for value, not address)
GRAVITY         = $03
JUMPFORCE       = $50
TERMINALVEL     = $04
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
AREABANK        = $0440     ;128 bytes for 64 areas.
UI              = $04C0     ;64 bytes
COLLMAPBANK     = $0500     ;one page is 256 bytes. Is this enough or do I bleed into 0600? A colliion map will always be 60 bytes rn.
TEXTBANK        = $0580
ENTITIES        = $0600

;If I do a 2 bit coll map, each tile can have 4 properties: "not there", "solid", "climbable", "damaging"
.scope EntityType
    NoEntity = 0
    Player = 1
    Treasure = 2
    Snake = 3       ;will likely have a type number for each enemy type
.endscope

.struct Entity          ;12b
    type        .byte
    xpos        .byte
    ypos        .byte
    state       .byte
    ;Player States
    ;faceright faceleft      fall  climb   invinc  jump  walk standing
    ;0         0               0     0       0     0    0     0 -
    ;States for Treasure: 0 - untouched, 1 - retrieved
    ;States for Enemy: go by 2s and alternate for animation
    ;faceright faceleft                    dead   hurt  attack walk
    ;0         0               0     0       0     0    0     0 

    ;0 - alive/default action(walk, etc.), 2 - attack/secondary action, 4 - dead
    pal         .byte ;likely can reduce these to one palette byte, then one byte with bit flags for each quadrant for flips h and v
    tlspr       .byte
    trspr       .byte
    blspr       .byte
    brspr       .byte
    env         .byte   ;$00 - nothing, $01 - climbable, $02 - damage, $03 - other entity  %tltrblbr 00-00-00-00
    metax       .byte
    metay       .byte
    ;any variables below this comment are not stored in ENTITIES RAM
    ;use for player variables since playerdata is maintained in zeropage
    velocity    .byte
    health      .byte
.endstruct

.struct Metatile        ;8b
    ypos        .byte       
    spritetl    .byte
    spritetr    .byte      
    spritebl    .byte      
    spritebr    .byte      
    pal         .byte
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
    paladdr1    .byte
    paladdr2    .byte
    entaddr1    .byte
    entaddr2    .byte
.endstruct

.segment "STARTUP"

;I had to open up the zero page in nes.cfg to range from $0002 to $00FF. Idk if that will break something later.
.segment "ZEROPAGE"
gamestate:          .res 1  ;$00 - title, $01 - main game, $02 - paused, $03 - game over, $04 - win/cutscene, $05 - screen transition, $06 - PPUoff NT fill, $07 - transition to main game, #$08 - main game but with lava
controller:         .res 1    ;reserve 1 byte for controller input
drawcomplete:       .res 1      ;1 bit flag
scrollx:            .res 1
scrolly:            .res 1
tilebufferA:        .res 32
tilebufferB:        .res 32
swap:               .res 1      ;1 bit flag
buttonflag:         .res 1      ;1 bit flag
timer:              .res 1
counter:            .res 1
checkvar:           .res 1
colltemp1:          .res 1
colltemp2:          .res 1
colltemp3:          .res 1
collreturnval:      .res 1
animaframes:        .res 1
totalsprites:       .res 1
playerdata:         .res .sizeof(Entity)            ;14b
lastpiece:          .res 1
entcollisionret:    .res 3                          ;array to return the "ids" of collided entities
playeroverbox:      .res 1
lavacounter:        .res 1
lavatimer:          .res 1
lavaaddress:        .res 2
cutsceneid:         .res 1                          ;00 - none, 01 - lava shake, 02 - victory
metatile:           .res .sizeof(Metatile)          ;8b
entitybuffer:       .res .sizeof(Entity)            ;14b
blockrow:           .res 16
spritemem:          .res 2
level:              .res .sizeof(Area)              ;10b
nextarea:           .res .sizeof(Area)              ;10b
nextareaid:         .res 1
ptr:                .res 2
ptr2:               .res 2
bgpalette:          .res 1
btptr:              .res 2
cmptr:              .res 2
palptr:             .res 2
entptr:             .res 2
exitdir:            .res 1      ;1 - up, 2 - down, 3 - left, 4 - right

;total zero page reserved: 168b (max 254)

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

    ;TXA
    LDA #$00
    STA gamestate

    LDX #$00 ;before calling from wild
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

INITIALIZETITLESCREEN:
    LDA #$00
    STA timer
    LDA #$FF
    STA ENTITIES
    LDA #<TITLESCREEN
    ;LDA #<VICTORYSCREEN
    STA ptr           ;utilize block table pointer for text load
    LDA #>TITLESCREEN
    ;LDA #>VICTORYSCREEN
    STA ptr+1
    JSR LoadTextFromROM  ;Load Title Screen text into TEXTBANK ($0580) [THIS WORKS]
    
    LDA $2002           ;PPUSTATUS    Is he reading to clear vblank flag? Neads to be changed to use NMI instead
    LDA #$20
    STA $2006           ;PPUADDR      we are using $2000 for graphics memory
    LDA #$00
    STA $2006           ;PPUADDR

    JSR LoadBlankNametable 
    
    LDA $2002           ;PPUSTATUS    Is he reading to clear vblank flag? Neads to be changed to use NMI instead
    LDA #$28
    STA $2006           ;PPUADDR      we are using $2000 for graphics memory
    LDA #$00
    STA $2006           ;PPUADDR
    JSR LoadBlankNametable
    LDA #$23
    STA $2006           ;PPUADDR      nametable1 attribute layer
    LDA #$C0
    STA $2006           ;PPUADDR
    LDA #%01010101
    STA bgpalette
    JSR LoadSingleAttributes
    JSR DrawText

;Clear register and set palette address
INITPALETTE:
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


    ;LDA #$0A ;player plus medallion pieces
    ;STA totalsprites
;Placegholder hardcode to get to title screen

;;;;;;;;;;;;;;;;;;;;;;;;

    LDA $2002           ;PPUSTATUS    Is he reading to clear vblank flag? Neads to be changed to use NMI instead
    LDA #$20
    STA $2006           ;PPUADDR      we are using $2000 for graphics memory
    LDA #$00
    STA $2006           ;PPUADDR
    LDA gamestate
    CMP #$00
    BEQ FINISHINIT 

FINISHINIT:
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

























;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------
;-----------------MAIN GAME LOOP-------------------------------------------------------
;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------

GAMELOOP:
UpdateTimer:
    LDA timer
    CMP #$00
    BEQ CheckForGameState
    DEC timer
CheckForGameState:       ;After player is written, see if game has entered "transition" state to move to next area
    LDA gamestate
    CMP #$03            ;game over
    BEQ HandleGameOver
    CMP #$04            ;win state, cutscene
    BEQ HandleCutscene
    CMP #$05
    BEQ JumpToScreenTransition
    CMP #$08
    BEQ HandleLava
    CMP #$06            ;leave as last in list because it just calls a JSR
    BNE DoneCheckForGameState
    JSR LoadNextArea
    JMP waitfordrawtocomplete
JumpToScreenTransition:
    JMP SCREENTRANLOOP   ;waitfordrawtocompleteST   ;do I need to do jump to a specific part of the loop?

HandleGameOver:
        LDA timer
        CMP #$00
        BEQ JumpToInitGameOver
        JMP waitfordrawtocomplete
    JumpToInitGameOver:
        LDA #<GAMEOVERSCREEN
        STA ptr           ;utilize block table pointer for text load
        LDA #>GAMEOVERSCREEN
        STA ptr+1

        JSR InitializeAltScreen
        JMP waitfordrawtocomplete
HandleCutscene:          ;make sure relevant variables, esp custceneid, for a scene are set at time the state is changed.
        LDA cutsceneid
        CMP #$00
        BEQ DoneCheckForGameState  ;00 - bug state, gamestate shows cutscene but no id given
        CMP #$01
        BEQ RunCutscene1
        CMP #$02
        BEQ RunCutscene2
        JMP DoneCheckForGameState   ;no branch bug state, given id doesn't have associated cutscene
    RunCutscene1:
        LDA timer
        CMP #$00
        BEQ ExitCutscene1
        JSR PaletteShake
        JMP waitfordrawtocomplete
    ExitCutscene1:
        LDA #$08        ;enter lava state
        STA gamestate
        LDA #$00        ;restore original palette
        STA bgpalette    
        LDA $2002
        LDA #$23
        STA $2006           ;PPUADDR      nametable1 attribute layer
        LDA #$C0
        STA $2006           ;PPUADDR
        JSR LoadSingleAttributes
        LDA #$00
        STA cutsceneid
        JMP waitfordrawtocomplete
    RunCutscene2:
        JSR VictoryCutscene
        JMP waitfordrawtocomplete

HandleLava:             ;anything else we needto do here? Once the top row animation is in, may need some added logic
        LDA lavatimer
        CMP #$00
        BEQ ResetLavaTimer
        DEC lavatimer
        JMP FinishHandleLava
    ResetLavaTimer:
        INC lavacounter
        LDA #$80        ;time between lava flow advances
        STA lavatimer
    FinishHandleLava:
        JSR AddLavaToTileBuffer
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
    AND #%11011000      ;turn of falling, jumping, standing and walking
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
checkstart:
    LDA controller
    AND #$10
    BEQ checka
    LDA gamestate
    CMP #$00
    BEQ loadmaingame
    CMP #$03
    BEQ loadmaingame
    JMP checka  ;eventually put PAUSE function here
loadmaingame:
    LDA #$07
    STA gamestate   ;if on title screen, change state to main game.
    JMP waitfordrawtocomplete
checka:
    LDA controller
    AND #$80
    BEQ checkarelease
    
    LDA buttonflag
    AND #%00000001
    CMP #$01
    BEQ checkarelease

    LDA playerdata+Entity::state
    AND #%00000100      ;is jumping?
    CMP #$04
    BEQ checkarelease   ;if already jumping, move on to next check
    LDA playerdata+Entity::state
    AND #%00100000      ;is falling?
    CMP #$20
    BEQ checkarelease   ;if already jumping, move on to next check

    LDA playerdata+Entity::state
    AND #%11101101      ;turn off walking and climbing
    ORA #%00000100      ;turn on jumping
    STA playerdata+Entity::state
    LDA #JUMPFORCE
    STA playerdata+Entity::velocity

    INC buttonflag
    ;ORA #$01
    ;STA buttonflag

    JMP finishcontrols
checkarelease:
    LDA controller
    AND #$80
    BNE finishcontrols
    LDA buttonflag
    AND #$01
    BEQ finishcontrols
    DEC buttonflag ;only works for a-button bc it's bit 1
    ;JMP playerjump
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
finishcontrols: ;if on title screen, skip game logic
    LDA gamestate
    CMP #$00
    BNE processplayer
    JMP waitfordrawtocomplete

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
CheckHealth:
    LDA playerdata+Entity::health
    CMP #$01
    BPL LoadState
    LDA #$02        ;palette change
    STA playerdata+Entity::pal
    LDA #$00
    STA playerdata+Entity::health
LoadState:
;check win conditions
CheckWinConditions:
    LDA lastpiece       ;has the player collected the last piece?
    CMP #$01
    BNE CheckInvincibleState
    LDY #$00
    LDA (level+Area::selfaddr1), y  ;do I need to move this into ptr first? If not, TODO: go save cycles/zero page in LoadNextArea
    CMP #$04            ;self ID of "outside" top screen
    BNE CheckInvincibleState
    LDA #$00
    STA colltemp1
    STA colltemp2
    STA colltemp3
    STA timer
    LDA #$02        ;victory cutscene id in cutscene library
    STA cutsceneid
    LDA #$04        ;laughably, the self ID of the final area and the "win" gamestate ID are the same, so I could skip this line.
    STA gamestate   ;but I would feel uncomfortable doing so bc if I reogranize something, it would break and I'd forget why.
    JMP waitfordrawtocomplete
;check invincible state
CheckInvincibleState:
    LDA playerdata+Entity::state
    AND #%00001000
    CMP #%00001000
    BNE CheckJumpState
    LDA timer
    CMP #$00
    BEQ RemoveInvincibleState
    LDA timer
    LSR
    LSR
    LSR
    BCS CheckJumpState
    LDA playerdata+Entity::pal
    EOR #$02
    STA playerdata+Entity::pal
    JMP CheckJumpState
RemoveInvincibleState:
    LDA playerdata+Entity::state
    AND #%11110111
    STA playerdata+Entity::state
    LDA #$00
    STA playerdata+Entity::pal
;check jump state
CheckJumpState:
    LDA playerdata+Entity::state
    TAX
    AND #%00000100      ;check if jumping
    CMP #%00000100
    BNE CheckClimbingState
JumpResolve:
    ;if jumping, then not falling. Now that we have terminal velocity we can separate these for state management sake
    ;LDA playerdata+Entity::state
    ;AND #%11011111
    ;STA playerdata+Entity::state
    LDA playerdata+Entity::velocity
    BEQ PlayerDescend       ;branch on less than or equal to zero
    BMI PlayerDescend
PlayerAscend:
    TAX
    CLC
    LSR
    LSR
    LSR
    LSR
    STA playerdata+Entity::velocity
    LDA playerdata+Entity::ypos
    SEC
    SBC playerdata+Entity::velocity
    STA playerdata+Entity::ypos
    TXA
    ;STA playerdata+Entity::velocity
    SEC
    SBC #GRAVITY
    STA playerdata+Entity::velocity
    JMP CheckCollisions
PlayerDescend:
    TAX
    EOR #%11111111  ;invert bits
    CLC
    ADC #$01        ;add 1
    CLC
    LSR             ;divide by 4
    LSR
    LSR
    CMP #TERMINALVEL
    BPL SetTerminalVelocity
    STA playerdata+Entity::velocity
    JMP ChangeYPos
SetTerminalVelocity:
    LDA #TERMINALVEL
    STA playerdata+Entity::velocity
ChangeYPos:
    LDA playerdata+Entity::ypos
    CLC
    ADC playerdata+Entity::velocity
    STA playerdata+Entity::ypos
    TXA             ;get starting velocity back
    SEC
    SBC #GRAVITY
    STA playerdata+Entity::velocity
    JMP CheckCollisions
CheckClimbingState:
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
    LDA #$04
    STA playerdata+Entity::tlspr
    LDA #$05
    STA playerdata+Entity::trspr
    LDA #$14
    STA playerdata+Entity::blspr
    LDA #$15
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
;TODO: Move animaton procedures to use the ENTITIES values like the rest of game objects
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
    LDA #$00
    STA playerdata+Entity::tlspr
    LDA #$01
    STA playerdata+Entity::trspr
    LDA #$12
    STA playerdata+Entity::blspr
    LDA #$02
    STA playerdata+Entity::brspr
    JMP CheckFalling
SetWalkTwo:
    LDA #$00
    STA playerdata+Entity::tlspr
    LDA #$01
    STA playerdata+Entity::trspr
    LDA #$13
    STA playerdata+Entity::blspr
    LDA #$03
    STA playerdata+Entity::brspr

;check fall state
CheckFalling:
    LDA playerdata+Entity::state
    AND #%00010000
    CMP #%00010000    ;is player climbing?
    BEQ CheckCollisions
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
    JMP CheckCollisions
PlayerFall:
    ;to get it working, fall at a rate of 2 pps. Math later.
    INC playerdata+Entity::ypos
    INC playerdata+Entity::ypos

    JMP CheckCollisions
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

CheckCollisions:;check for collisions from new position
ProcessCollisions:                  ;get return values from all collisions and process
    ;LDA collreturnval   ;00 nothing, 01 solid, 10 climbable, 11 damage!
CheckAgainstEntities:
    LDA #$00                ;clear return array
    STA entcollisionret
    STA entcollisionret+1
    STA entcollisionret+2
    JSR CheckPlayerCollisionAgainstEntities
    LDA entcollisionret     ;TODO: only checking first entity right now, ROBUSTIFY
    CMP #$00
    BEQ JumpToCheckOver
    CMP #EntityType::Snake
    BEQ HandleSnakeHit
    CMP #EntityType::Treasure
    BEQ HandleGetTreasure
JumpToCheckOver:
    JMP CheckOver
HandleGetTreasure: ;How to handle multiple treasures on one stage? Not needed for this demo. two byte return value with location in ENTITIES?
    LDA #$80
    STA timer
    LDA #$01
    STA cutsceneid
    LDA #$04
    STA gamestate
    
    LDX #$0C    ;end of player
    FindTreasureEntity:
        LDA ENTITIES, x
        CMP #EntityType::Treasure
        BEQ ResolveHandleTreasure
        CMP #$FF    ;end of entities list
        BEQ JumpToCheckOver   ;This should be impossible
        CMP #$00    ;or type nothing (dead space)
        BEQ JumpToCheckOver
        TXA
        CLC
        ADC #$0C
        TAX
        JMP FindTreasureEntity
    ResolveHandleTreasure:
        LDA #$00
        STA ENTITIES, x     ;turn off draw
        LDA #$01
        STA lastpiece       ;hooray, you now have the last piece   
                            ;Replace this code with an inventory system if game gets bigger
    HardCodeUIChangeForDemo:    ;TODO: placeholder code!!
        LDX #$0C
        LDA #$22        ;y
        STA UI, x
        INX
        LDA #$39        ;spr
        STA UI, x
        INX
        LDA #$01
        STA UI, x
        INX
        LDA #$DC        ;x
        STA UI, x
        INX
        LDA #$FF
        STA UI, x

        JMP CheckOver

HandleSnakeHit:
    LDA playerdata+Entity::state
    AND #%00001000
    CMP #%00001000
    BEQ CheckOver
    LDA playerdata+Entity::state
    AND #%11000000
    CMP #%10000000
    BEQ SnakeHitFacingRight
SnakeHitFacingLeft:
    INC playerdata+Entity::xpos
    INC playerdata+Entity::xpos
    INC playerdata+Entity::xpos
    INC playerdata+Entity::xpos
    INC playerdata+Entity::xpos
    INC playerdata+Entity::xpos
    INC playerdata+Entity::xpos
    INC playerdata+Entity::xpos
    DEC playerdata+Entity::health
    LDA playerdata+Entity::health
    CMP #$00
    BEQ Dead
    LDA playerdata+Entity::state
    ORA #%00001000          ;turn on invincible
    STA playerdata+Entity::state
    LDA #$8F
    STA timer
    JMP CheckOver
SnakeHitFacingRight:
    DEC playerdata+Entity::xpos
    DEC playerdata+Entity::xpos
    DEC playerdata+Entity::xpos
    DEC playerdata+Entity::xpos
    DEC playerdata+Entity::xpos
    DEC playerdata+Entity::xpos
    DEC playerdata+Entity::xpos
    DEC playerdata+Entity::xpos
    DEC playerdata+Entity::health
    LDA playerdata+Entity::health
    CMP #$00
    BEQ Dead
    LDA playerdata+Entity::state
    ORA #%00001000          ;turn on invincible
    LDA #$8F
    STA timer
    STA playerdata+Entity::state
    JMP CheckOver
Dead:
    LDA #$03
    STA gamestate
    LDA #$08
    STA playerdata+Entity::tlspr
    LDA #$09
    STA playerdata+Entity::trspr
    LDA #$18
    STA playerdata+Entity::blspr
    LDA #$19
    STA playerdata+Entity::brspr
    LDA #$8F
    STA timer
    JMP EndProcessPlayer

CheckOver:
    LDA #$00
    STA collreturnval

    JSR CheckPlayerCollisionOver
ProcessLeft:
    JSR CheckPlayerCollisionLeft
    LDA collreturnval
    AND #%00001100
    CMP #%00001000      ;Climbable
    BEQ HandleClimbableLeft
    CMP #%00000100      ;solid
    BEQ HandleSolidLeft
    JMP ProcessRight
HandleClimbableLeft:
    ;no left exits yet
    LDA #$01
    STA playerdata+Entity::env
    JMP ProcessRight
HandleSolidLeft:
    ;LDA playerdata+Entity::state
    ;AND #%11111101  ;turn of walk
    ;ORA #%00000001  ;turn on stand
    ;STA playerdata+Entity::state
    INC playerdata+Entity::xpos
ProcessRight:
    JSR CheckPlayerCollisionRight  
    LDA collreturnval
    AND #%00000011
    CMP #%00000010      ;Climbable
    BEQ HandleClimbableRight
    CMP #%00000001      ;solid
    BEQ HandleSolidRight
    JMP ProcessUp
HandleClimbableRight:
    ;No right exits yet
    LDA #$01
    STA playerdata+Entity::env

    JMP ProcessUp
HandleSolidRight:
    ;LDA playerdata+Entity::state
    ;AND #%11111101  ;turn of walk
    ;ORA #%00000001  ;turn on stand
    ;STA playerdata+Entity::state
    DEC playerdata+Entity::xpos

ProcessUp:
    JSR CheckPlayerCollisionUp    ;check upward collision every frame
    LDA collreturnval
    AND #%11000000
    CMP #%10000000      ;climbable
    BEQ HandleClimbableUp
    CMP #%01000000      ;solid
    BEQ HandleSolidUp
    CMP #%11000000
    BEQ HandleDamagingUp
    CMP #%00000000
    BEQ HandleNothingUp
    JMP ProcessDown
HandleClimbableUp:
    LDA #$01
    STA playerdata+Entity::env
    JMP ProcessDown
HandleSolidUp:
    LDA #$00
    STA playerdata+Entity::velocity
    ;LDA playerdata+Entity::state
    ;AND #%11111011  ;turn off jump
    ;ORA #%00100000  ;turn on fall
    ;STA playerdata+Entity::state
    LDA playerdata+Entity::ypos ;push back down
    AND #%11110000
    CLC
    ADC #$10
    STA playerdata+Entity::ypos
    JMP EndProcessPlayer
HandleDamagingUp:
    LDA playerdata+Entity::state
    AND #%00001000
    CMP #%00001000 ;invincible?
    BEQ HandleSolidUp
    DEC playerdata+Entity::health
    LDA playerdata+Entity::health
    CMP #$00
    BEQ DeadU
    LDA playerdata+Entity::state
    ORA #%00001000          ;turn on invincible
    LDA #$8F
    STA timer
    STA playerdata+Entity::state
    JMP EndProcessPlayer
DeadU:
    JMP Dead
HandleNothingUp:
    ;LDA playerdata+Entity::state
    ;AND #%00010000      ;is climbing?
    ;CMP #%00010000
    ;BNE ProcessDown
    ;INC playerdata+Entity::ypos
ProcessDown:
    JSR CheckPlayerCollisionDown    ;check downward collision every frame
    LDA collreturnval
    AND #%00110000
    CMP #%00100000      ;Climbable
    BEQ HandleClimbableDown
    CMP #%00010000      ;solid
    BEQ HandleSolidDown
    JMP HandleNothingDown
HandleClimbableDown:
    ;LDA #$01
    ;STA playerdata+Entity::env
    LDA playerdata+Entity::state
    AND #%00010000
    CMP #%00010000
    BEQ KeepClimbingDown
    JMP HandleNothingDown
KeepClimbingDown:
    LDA playerdata+Entity::state
    AND #%11011000      ;turn off fall, jump, walk, stand
    ORA #%00010000
    STA playerdata+Entity::state
    JMP EndProcessPlayer
HandleSolidDown:
    LDA playerdata+Entity::state
    AND #%11001011  ;turn off fall, climb, and jump
    ORA #%00000001  ;turn on stand
    STA playerdata+Entity::state
    LDA playerdata+Entity::ypos
    AND #%11110000
    STA playerdata+Entity::ypos
    JMP EndProcessPlayer
HandleNothingDown:
    LDA playerdata+Entity::state
    TAX
    AND #%00000100
    BNE EndProcessPlayer
    TXA
    AND #%00010000
    BNE EndProcessPlayer
    TXA
    AND #%11101100      ;turn off climb, jump, stand
    ORA #%00100000      ;turn on falling
    STA playerdata+Entity::state
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
    CPY #$0B    ;12 items in entity struct
    BNE CopyPDtoEBLoop
    LDX #$00    ;X = ENTITIES index of player + 1
    LDA #$01
    STA checkvar    ;set checkvar to 1 so subroutine does not mess with stack
    JSR WriteEntityFromBuffer ;Subroutine to write all the changes made to player this frame

    ;JMP waitfordrawtocomplete   ;DEBUG line, comment out when ready to test enemy processes.
    ;START HERE 7/12: Fix entity update system

    LDX #$0C    ;start after player entry
ProcessEnemiesLoop: ;state is byte 4 of the ENTITIES entry. Process directly to RAM location.
    LDA ENTITIES, x
    CMP #$FF
    BNE BufferEntity
    ;CMP #$00
    ;BNE BufferEntity
    JMP EndProcessEnemiesLoop
;TODO: in order to robustify enemy processes, write actions into their ROM profile.
;      At first let's just process through enemy types with a switch-case style flow
;      Identifying the type then applying its action accordingly, iterating through all possible types.
;       If the game starts to acquire too many enemy types, this will take retooling
;   Use entitybuffer toperform subs like collision detection while processing an entity, then write it at the end.

BufferEntity:
    LDY #$00
BufferEntityLoop:
    LDA ENTITIES, x
    STA entitybuffer, y
    INX
    INY
    CPY #$0C    ;12 items in entity struct
    BNE BufferEntityLoop
    TXA
    SEC
    SBC #$0C
    TAX
CheckSnake:
    LDA entitybuffer+Entity::type
    CMP #EntityType::Snake
    BNE JumpToNextEnemyType
    LDA entitybuffer+Entity::state
    LSR
    BCS SnakeWalkProcess    ;If low bit is set, Process walk
        ;---Reload state to perform other logic---
    JMP FinishSnake
JumpToNextEnemyType:
    JMP NextEnemyType
SnakeWalkProcess:
;increment selftimer (+B)
    LDA entitybuffer+Entity::env    ;enemy repurposes env as self-timer
    CMP #$90
    BMI SnakeMove
    INC entitybuffer+Entity::env
    JMP FinishSnake
SnakeMove:
    LDA #$00
    STA collreturnval
    STA entitybuffer+Entity::env    ;reset timer
    LDA entitybuffer+Entity::state ;reload state
    ASL
    BCS SnakeMoveRight
SnakeMoveLeft:
    JSR CheckEntityCollisionLeft
    LDA collreturnval
    CMP #$02
    BEQ SnakeToggleFacing
    DEC entitybuffer+Entity::xpos
    DEC entitybuffer+Entity::xpos
    JMP SnakeMoveChangeFrame
SnakeMoveRight:
    JSR CheckEntityCollisionRight
    LDA collreturnval
    CMP #$02
    BEQ SnakeToggleFacing
    INC entitybuffer+Entity::xpos
    INC entitybuffer+Entity::xpos
SnakeMoveChangeFrame:
    LDA #<SNAKE
    STA entptr
    LDA #>SNAKE
    STA entptr+1
    LDA entitybuffer+Entity::metax      ;repurpose metax as frame counter. SnakeWalk is 2 frames.
    CMP #$01
    BEQ SnakeMoveChangeFrame2
SnakeMoveChangeFrame1:
    INC entitybuffer+Entity::metax
    LDY #$07    ;offset to second sprite
    LDA (entptr), y
    STA entitybuffer+Entity::tlspr
    INY
    LDA (entptr), y
    STA entitybuffer+Entity::trspr
    INY
    LDA (entptr), y
    STA entitybuffer+Entity::blspr
    INY
    LDA (entptr), y
    STA entitybuffer+Entity::brspr
    INY
    JMP FinishSnake
SnakeMoveChangeFrame2: 
    DEC entitybuffer+Entity::metax
    LDY #$03    ;offset to first sprite
    LDA (entptr), y
    STA entitybuffer+Entity::tlspr
    INY
    LDA (entptr), y
    STA entitybuffer+Entity::trspr
    INY
    LDA (entptr), y
    STA entitybuffer+Entity::blspr
    INY
    LDA (entptr), y
    STA entitybuffer+Entity::brspr
    INY
    JMP FinishSnake
SnakeToggleFacing:
    LDA #$00
    STA collreturnval
    LDA entitybuffer+Entity::state
    EOR #%11000000
    STA entitybuffer+Entity::state
FinishSnake:
    JSR WriteEntityFromBuffer   ;uses x index, make sure it's in right location
NextEnemyType:
    TXA
    CLC
    ADC #$0C
    TAX         ;move to next entity position
    JMP ProcessEnemiesLoop

EndProcessEnemiesLoop:
    LDA #$FF
    STA ENTITIES, x ;set endpoint on entities array
waitfordrawtocomplete:
    LDA drawcomplete
    CMP #$01
    BNE waitfordrawtocomplete
    LDA #$00
    STA drawcomplete
    LDA gamestate ;-check if transitioning from title screen
    CMP #$07
    BNE GoToGAMELOOP
    LDA #$01
    STA gamestate
    JSR InitializeMainGame
GoToGAMELOOP:
    JMP GAMELOOP


;---------------------------------------------------------------------------------------------

;---------------------------------------------------------------------------------------------


;---------------------------------------------------------------------------------------------


;---------------------------------------------------------------------------------------------



;---------------------------------------------------------------------------------------------

;---------------------------------------------------------------------------------------------


;---------------------------------------------------------------------------------------------


;---------------------------------------------------------------------------------------------
;----------------------Alternative Game Loops-----------------------------



;---------------SCREEN TRANSITION LOOP-----------------------------------------------------;
;When switching screens, get out of game loop entirely and use SCREENTRANLOOP
;Both PPU nametables should be loaded up by using the TransitionScreen BG loading subroutine
SCREENTRANLOOP:
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
        CMP #$01
        BEQ TransitionUp
        CMP #$02
        BEQ TransitionDown
        JMP waitfordrawtocompleteST     ;change as more directions added ofc
    TransitionUp:
        DEC scrolly
        DEC scrolly
        INC playerdata+Entity::ypos
        INC playerdata+Entity::ypos
        LDA scrolly
        CMP #$FD
        BNE DoneTransitionUp
        ;LDA #$80
        ;STA playerdata+Entity::ypos
    DoneTransitionUp:
        LDX #$00
        JMP CopyPDtoEBLoopST
    TransitionDown: ;downward exit means screen scrolls up, scrolly will decrement
        INC scrolly
        INC scrolly ;where the magic happens
        DEC playerdata+Entity::ypos
        DEC playerdata+Entity::ypos
        LDA playerdata+Entity::ypos
        SEC
        SBC #$FD
        BCC DoneTransitionDown
        LDA #$01
        STA playerdata+Entity::ypos
    DoneTransitionDown:
        LDX #$00
    CopyPDtoEBLoopST:
        LDA playerdata, x
        STA entitybuffer, x
        INX
        INY
        CPY #$0C    ;12 items in entity struct
        BNE CopyPDtoEBLoopST
        LDX #$00    ;X = ENTITIES index of player + 1
        LDA #$01
        STA checkvar    ;set checkvar to 1 so subroutine does not mess with stack
        JSR WriteEntityFromBuffer ;Subroutine to write all the changes made to player this frame
    CheckScrollDirection:
        LDA exitdir
        CMP #$01
        BEQ CheckEndScrollUp
        CMP #$02
        BEQ CheckEndScrollDown
    CheckEndScrollUp:
        LDA scrolly
        CMP #$00
        BNE JumpTowaitfordrawtocompleteST
        LDA #$DF
        STA playerdata+Entity::ypos
        JMP ResetScrollY
    JumpTowaitfordrawtocompleteST:
        JMP waitfordrawtocompleteST
    CheckEndScrollDown:
        LDA scrolly
        CMP #$F0
        BNE JumpTowaitfordrawtocompleteST
    ;Execute at the end of screen transition
    ResetScrollY:   ;Rename to reset nametable. Time to put the new level in NT1 and clear NT2.   
        ;LDA swap    ;should I swap and stay in second nametable or rewrite new map to first nametable and only use second on transitions?
        ;EOR #$02
        ;STA swap
        LDA #$00
        STA scrolly
    MoveNAtoLevelStruct:
        LDA nextarea+Area::selfaddr1
        STA level+Area::selfaddr1
        LDA nextarea+Area::selfaddr2
        STA level+Area::selfaddr2
        LDA nextarea+Area::btaddr1
        STA level+Area::btaddr1
        LDA nextarea+Area::btaddr2
        STA level+Area::btaddr2
        LDA nextarea+Area::cmaddr1
        STA level+Area::cmaddr1
        LDA nextarea+Area::cmaddr2
        STA level+Area::cmaddr2
        LDA nextarea+Area::paladdr1
        STA level+Area::paladdr1
        LDA nextarea+Area::paladdr2
        STA level+Area::paladdr2
        LDA nextarea+Area::entaddr1
        STA level+Area::entaddr1
        LDA nextarea+Area::entaddr2
        STA level+Area::entaddr2

        LDA $2000
        AND #%01111111
        STA $2000
        LDA #$00
        STA $2001
        LDA $2002
        LDA #$20
        STA $2006           ;PPUADDR      $2000 for nametable1
        LDA #$00
        STA $2006           ;PPUADDR
    ResetNametables:
        LDA level+Area::btaddr1
        STA btptr
        LDA level+Area::btaddr2
        STA btptr+1
        JSR LoadNametable
        LDA #$23
        STA $2006           ;PPUADDR      nametable1 attribute layer
        LDA #$C0
        STA $2006           ;PPUADDR
        LDA level+Area::paladdr1
        STA palptr
        LDA level+Area::paladdr2
        STA palptr+1
        JSR LoadAttributes  
        LDA #$1E
        STA $2001
        LDA $2000
        ORA #%10000000
        STA $2000

        LDA #$01                    ;set gamestate to main game
        STA gamestate
        ;Finally add the new area entities
    FillEntities:
        LDA nextarea+Area::entaddr1
        STA entptr
        LDA nextarea+Area::entaddr2
        STA entptr+1
        JSR LoadEntitiesFromROM
    ClearNextAreaStruct:
        LDA #$00
        STA nextarea+Area::selfaddr1
        STA nextarea+Area::selfaddr2
        STA nextarea+Area::btaddr1
        STA nextarea+Area::btaddr2
        STA nextarea+Area::cmaddr1
        STA nextarea+Area::cmaddr2
        STA nextarea+Area::entaddr1
        STA nextarea+Area::entaddr2

        JMP waitfordrawtocomplete   ;back to main GAMELOOP
    waitfordrawtocompleteST:
        LDA drawcomplete
        CMP #$01
        BNE waitfordrawtocompleteST
        LDA #$00
        STA drawcomplete

        JMP SCREENTRANLOOP



;
;
;
;
;

;-------SSSSS---------UU-----------UU-------BBBBBBBB----------SSSSS------------------------;
;-----SS-----SS-------UU-----------UU-------BB------BB------SS-----SS----------------------;
;---SS--------SS------UU-----------UU-------BB------BB----SS--------SS---------------------;
;---SS----------------UU-----------UU-------BB------BB----SS-------------------------------;
;---SS----------------UU-----------UU-------BB----BB------SS-------------------------------;
;-----SS--------------UU-----------UU-------BBBBBBB---------SS-----------------------------;
;-------SSSSSS--------UU-----------UU-------BB-----BB---------SSSSSSS----------------------;
;------------SS-------UU-----------UU-------BB------BB--------------SS---------------------;
;-------------SS------UU-----------UU-------BB-------BB--------------SS--------------------;
;----SS-------SS------UU-----------Uu-------BB-------BB----SS--------SS--------------------;
;-----SS-----SS---------UU-------UU---------BB------BB------SS------SS---------------------;
;-------SSSSS-------------UUUUUUU-----------BBBBBBBB-----------SSSSS-----------------------;




;;;--------------------------------------------------------------------------------------------;;;
;;;=-------------------------------------------------------------------------------------------;;;
;;;;; ----- Logical subroutines ------ ;;;;;;
;;;--------------------------------------------------------------------------------------------;;;
;;;--------------------------------------------------------------------------------------------;;;


;;;----------------------------------Collision Detection subroutines--------------------------;;;
PlayerCollisionDetection:
    CheckPlayerCollisionUp:
        CheckPlayerCollisionUpStart:
            NOP
            NOP
            NOP

            PHA
            TXA
            PHA
            TYA
            PHA

            NOP
            NOP
            NOP

            LDA playerdata+Entity::xpos
            CLC
            ADC #$01    ;one pixel in from left edge - x1
            TAX         ;store in X reg (X1)
            CLC
            ADC #$0C    ;plus 14 more to get one pixel in from right edge - x2
            TAY         ;store on Y     (X2)
            LDA playerdata+Entity::ypos
            CMP #$00    ;Top of screen?
            BEQ ExitUp
            ;CMP #$FF
            ;BEQ ExitUp
            LSR         ;divide by 16
            LSR         ;meta y of player's head
            LSR
            LSR
            ASL
            ASL         ;collision map y-offset
            STA checkvar         ;store in checkvar
            ;Get collmap position of (x1,y) and (x2, y) then check if tile below is solid. If both are NOT solid (== $01), then fall.
            ;1) Divide x by 16
            ;2) That number + y = test cell
            ;3) if collmap, test cell is $01, solid.
        TestX1U:
            TXA
            LSR
            LSR
            LSR
            LSR     ;meta x
            PHA
            LSR
            LSR     ;collmap x
            STA colltemp2       ;collision map x1-offset
            LDA #$00
            STA colltemp3
            JMP ContinueTestU
        TestX2U:
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
        ContinueTestU:
            LDA checkvar    ;check row above (Y-offset)
            CLC
            ADC colltemp2   ;0, 1, 2 or 3 (X-offset)
            ;CLC
            ;ADC checkvar    ;x1 (col) + target y (row)  ; multiple of 4
            TAX
            LDA COLLMAPBANK, x ;get target byte
            STA colltemp1       ;target byte
            PLA     ;get back meta x
            AND #%00000011
            CMP #$00
            BEQ MaskOutZeroU
            CMP #$01
            BEQ MaskOutOneU
            CMP #$02
            BEQ MaskOutTwoU
            CMP #$03
            BEQ MaskOutThreeU ;may be to far to branch
            JMP ReturnFromCollisionUp
        ExitUp:
            LDA #$01
            STA exitdir
            LDY #$01        ;offset to "up" exit id
            LDA (level+Area::selfaddr1), y  ;exit id is the selfid of the next area, which should also be the offset into the AREABANK array
            STA nextareaid  ;get id of next area 
            JSR SetupNextArea   ;populate "nextarea" struct for draw routine.
            ;LDA #$DF
            ;STA playerdata+Entity::ypos
            LDA #$F0        ;set scrolly back 16px to account for only 15 horizontal rows
            STA scrolly
            LDA #$06        ;Load next area game state
            STA gamestate
            ;LDA collreturnval
            ;ORA #%10000000      ;Exit value in UP bits
            ;AND #%10111111      ;force 10xxxxxx
            ;STA collreturnval
            JMP ReturnFromCollisionUp
        MaskOutZeroU:
            LDA colltemp1
            AND #%11000000
            LSR
            LSR
            LSR
            LSR
            LSR
            LSR
            JMP ResolveU
        MaskOutOneU:
            LDA colltemp1
            AND #%00110000
            LSR
            LSR
            LSR
            LSR
            JMP ResolveU
        MaskOutTwoU:
            LDA colltemp1
            AND #%00001100
            LSR
            LSR
            JMP ResolveU
        MaskOutThreeU:
            LDA colltemp1
            AND #%00000011
        ResolveU:
            CMP #%00000011            ;if x1 or x2 is over damaging, we can resolve?
            BEQ ReturnDamagingU
            CMP #%00000001            ;is solid
            BEQ ReturnSolidU     ;if x1 or x2 is over solid, we can stop here
        ContinueResolveU:       ;TODO: check accuracy of this test for UP
            LDX colltemp3       ;holding whether we checked x1 or x2
            CPX #$01            ;if both points checked and no solid below, we must be falling
            BNE TestX2U
            CMP #%00000010            ;climbable?
            BEQ ReturnClimbableU
            JMP ReturnFromCollisionUp
        ReturnClimbableU:
            LDA collreturnval
            ORA #%10000000
            AND #%10111111
            STA collreturnval
            JMP ReturnFromCollisionUp
        ReturnSolidU:            ;resolve collision here. in future, may be wise to separate resolutinon from check
            ;INC playerdata+Entity::ypos     ;push back from entering block
            ;LDA #$00
            ;STA playerdata+Entity::velocity ;set velocity to zero
            LDA collreturnval
            ORA #%01000000        ;solid in up position
            AND #%01111111        ;for 01 into top 2 bits 01xxxxxx
            STA collreturnval
            JMP ReturnFromCollisionUp
        ReturnDamagingU:
            LDA collreturnval
            ORA #%11000000
            STA collreturnval
        ReturnFromCollisionUp:
            LDA #$00
            STA colltemp1
            STA colltemp2
            STA colltemp3
            STA checkvar

            PLA
            TAY
            PLA
            TAX
            PLA
            RTS

    CheckPlayerCollisionDown:
        CheckPlayerCollisionDownStart: 
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
            ADC #$0D    ;plus 14 more to get one pixel in from right edge - x2
            TAY         ;store on Y
            LDA playerdata+Entity::ypos
            CLC
            ADC #$10    ;y pos of player's bottom edge (feet) - y
            ;CMP #$FD    ;Bottom of tile of screen?
            ;BEQ ExitDown
            ;CMP #$FE
            ;BEQ ExitDown
            ;CMP #$FF
            ;BEQ ExitDown
            LSR         ;divide by 16
            LSR         ;meta y of player's feet
            LSR
            LSR
            CMP #$0F
            BEQ ExitDown
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
            ;LDA collreturnval
            ;ORA #%00100000
            ;AND #%11101111      ;force mask xx10xxxx
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
            CMP #%00000001
            BEQ ReturnSolid     ;if x1 or x2 is over solid, we can stop here
            CMP #%00000011            ;if x1 or x2 is over damaging, we can resolve?
            BEQ ReturnSolid
            CMP #%00000010            ;climbable?
            BEQ ReturnClimbable
        ContinueResolve:
            LDA colltemp3       ;holding whether we checked x1 or x2
            CMP #$01            ;if both points checked and no solid below, we must be falling
            BNE TestX2
            JMP ReturnNoGround
        ReturnClimbable:
            ;LDA #$01
            ;STA playerdata+Entity::env
            LDA collreturnval
            AND #%11101111
            ORA #%00100000
            STA collreturnval
            LDA colltemp3
            CMP #$01
            BEQ ReturnFromCollisionDown
            JMP TestX2
        ReturnNoGround:
            ;if player is jumping, do not reset to falling
            LDA collreturnval
            AND #%11001111      ;mask xx00xxxx, nothing in down bit means set state to falling
            STA collreturnval
            JMP ReturnFromCollisionDown
        ReturnSolid:            ;resolve collision here. in future, may be wise to separate resolutinon from check
            LDA collreturnval
            ORA #%00010000
            AND #%11011111      ;Mask xx01xxxx
            STA collreturnval
        ReturnFromCollisionDown:
            LDA #$00
            STA colltemp1
            STA colltemp2
            STA colltemp3
            STA checkvar
            PLA
            TAY
            PLA
            TAX
            PLA
            RTS


    ;----------------------------------------------------------------------------------------------------
    CheckPlayerCollisionLeft:
        CheckPlayerCollisionLeftStart:  ;add edge screen detect
            PHA
            TXA
            PHA
            LDA playerdata+Entity::xpos
            CMP #$00
            BNE ContinueGettingXL
            JMP ReturnSolidL
        ContinueGettingXL:
            LSR
            LSR
            LSR
            LSR
            LSR
            LSR
            STA colltemp1
            LDA playerdata+Entity::ypos
            LSR
            LSR
            LSR
            LSR     ;meta y
        CheckY2L:   ;check this first. yes i know it's backwards
            ASL
            ASL
            CLC
            ADC colltemp1   ;Add x byte to Y byte to find byte location in collmap
            STA colltemp2   ;Location of player byte in coll map
            LDA #$00
            STA checkvar
            JMP ContinueCheckL
        CheckY1L:
            LDA playerdata+Entity::ypos
            CLC
            ADC #$0F
            LSR
            LSR
            LSR
            LSR
            ASL
            ASL
            CLC
            ADC colltemp1
            STA colltemp2
            INC checkvar
            ;find bit pair
        ContinueCheckL:
            LDA playerdata+Entity::metax
            AND #%00000011
            CMP #$00
            BEQ MaskOutZeroL
            CMP #$01
            BEQ MaskOutOneL
            CMP #$02
            BEQ MaskOutTwoL
            CMP #$03
            BNE ReturnNothingL
            JMP MaskOutThreeL
        MaskOutZeroL:
            LDX colltemp2
            LDA COLLMAPBANK, x
            AND #%11000000 ;the bit set to the left of three
            LSR
            LSR
            LSR
            LSR
            LSR
            LSR
            ;CMP #%01000000
            ;BNE ReturnNothingL
            ;INC playerdata+Entity::xpos
            JMP ResolveL
        MaskOutOneL:
            LDX colltemp2
            LDA COLLMAPBANK, x
            AND #%00110000 
            LSR
            LSR
            LSR
            LSR
            ;CMP #%00010000
            ;BNE ReturnNothingL
            ;INC playerdata+Entity::xpos
            JMP ResolveL
        MaskOutTwoL:
            LDX colltemp2
            LDA COLLMAPBANK, x
            AND #%00001100 ;the bit set to the left of three
            LSR
            LSR
            ;CMP #%00000100
            ;BNE ReturnNothingL
            ;INC playerdata+Entity::xpos
            JMP ResolveL
        MaskOutThreeL:
            LDX colltemp2
            LDA COLLMAPBANK, x
            AND #%00000011 ;the bit set to the left of three
        ResolveL:
            CMP #%00000001  ;solid?
            BEQ ReturnSolidL
            CMP #%00000011
            BEQ ReturnSolidL
        ContinueResolveL:
            LDA checkvar
            CMP #$01
            BNE CheckY1L
            JMP ReturnNothingL
        ReturnSolidL:
            LDA collreturnval
            ORA #%00000100
            AND #%11110111      ;mask xxxx01xx
            STA collreturnval
            JMP ReturnFromCollL
        ReturnNothingL:
            LDA collreturnval
            AND #%11110011
            STA collreturnval
        ReturnFromCollL:
            LDA #$00
            STA colltemp1
            STA colltemp2
            STA colltemp3
            STA checkvar

            PLA
            TAX
            PLA
            RTS
    ;---------------------------------------------------------------
    CheckPlayerCollisionRight:    
        CheckPlayerCollisionRightStart:  ;add edge screen detect
            PHA
            TXA
            PHA
            LDA playerdata+Entity::xpos
            CLC
            ADC #$10
            CMP #$FF
            BNE ContinueGettingXR
            JMP ReturnSolidR
        ContinueGettingXR:
            LSR
            LSR
            LSR
            LSR
            LSR
            LSR
            STA colltemp1
            LDA playerdata+Entity::ypos
            LSR
            LSR
            LSR
            LSR     ;meta y
        CheckY2R:   ;check this first. yes i know it's backwards
            ASL
            ASL
            CLC
            ADC colltemp1   ;Add x byte to Y byte to find byte location in collmap
            STA colltemp2   ;Location of player byte in coll map
            LDA #$00
            STA checkvar
            JMP ContinueCheckR
        CheckY1R:
            LDA playerdata+Entity::ypos
            CLC
            ADC #$0F
            LSR
            LSR
            LSR
            LSR
            ASL
            ASL
            CLC
            ADC colltemp1
            STA colltemp2
            INC checkvar
            ;find bit pair
        ContinueCheckR:
            LDA playerdata+Entity::metax
            CLC
            ADC #$01
            AND #%00000011
            CMP #$00
            BEQ MaskOutZeroR
            CMP #$01
            BEQ MaskOutOneR
            CMP #$02
            BEQ MaskOutTwoR
            CMP #$03
            BNE ReturnNothingR
            JMP MaskOutThreeR
        MaskOutZeroR:
            LDX colltemp2
            LDA COLLMAPBANK, x
            AND #%11000000 ;the bit set to the left of three
            LSR
            LSR
            LSR
            LSR
            LSR
            LSR
            ;CMP #%01000000
            ;BNE ReturnNothingL
            ;INC playerdata+Entity::xpos
            JMP ResolveR
        MaskOutOneR:
            LDX colltemp2
            LDA COLLMAPBANK, x
            AND #%00110000 
            LSR
            LSR
            LSR
            LSR
            ;CMP #%00010000
            ;BNE ReturnNothingL
            ;INC playerdata+Entity::xpos
            JMP ResolveR
        MaskOutTwoR:
            LDX colltemp2
            LDA COLLMAPBANK, x
            AND #%00001100 ;the bit set to the left of three
            LSR
            LSR
            ;CMP #%00000100
            ;BNE ReturnNothingL
            ;INC playerdata+Entity::xpos
            JMP ResolveR
        MaskOutThreeR:
            LDX colltemp2
            LDA COLLMAPBANK, x
            AND #%00000011 ;the bit set to the left of three
        ResolveR:
            CMP #%00000001  ;solid?
            BEQ ReturnSolidR
            CMP #%00000011  ;stalactites only damage on UP collision?
            BEQ ReturnSolidR
        ContinueResolveR:
            LDA checkvar
            CMP #$01
            BNE CheckY1R
            JMP ReturnNothingR
        ReturnSolidR:
            LDA collreturnval
            ORA #%00000001
            AND #%11111101      ;mask xxxxxx01
            STA collreturnval
            JMP ReturnFromCollR
        ReturnNothingR:
            LDA collreturnval
            AND #%11111100
            STA collreturnval
        ReturnFromCollR:
            LDA #$00
            STA colltemp1
            STA colltemp2
            STA colltemp3
            STA checkvar

            PLA
            TAX
            PLA
            RTS
;------------------------------------------------------------------------------------------------------------
    CheckPlayerCollisionOver:
        CheckPlayerCollisionOverStart:
            PHA
            TXA
            PHA
            TYA
            PHA
            LDA #$00
            STA checkvar
            STA playeroverbox
            STA collreturnval
        COSetupLoopZero: ;tl corner
            LDA playerdata+Entity::xpos
            CLC
            ADC #$03
            LSR
            LSR
            LSR
            LSR
            STA colltemp3   ;collision map x position
            LSR
            LSR
            STA colltemp1
            LDA playerdata+Entity::ypos
            CLC
            ADC #$03
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
            JMP CheckOverLoop
        CheckOverLoop:
            LDA colltemp3
            AND #%00000011
            CMP #$00
            BEQ MaskOutZeroO
            CMP #$01
            BEQ MaskOutOneO
            CMP #$02
            BEQ MaskOutTwoO
            CMP #$03
            BNE MaskOutFailO
            JMP MaskOutThreeO
        MaskOutFailO:
            JMP ReturnFromCollO
        MaskOutZeroO:
            LDX colltemp2
            LDA COLLMAPBANK, x
            AND #%11000000 ;the bit set to the left of three
            CMP #%10000000
            BNE EndLoopO
            LDA #$01            ;01 will mean over a climbable
            STA playerdata+Entity::env
            JMP ReturnFromCollO
        MaskOutOneO:
            LDX colltemp2
            LDA COLLMAPBANK, x
            AND #%00110000 ;the bit set to the left of three
            CMP #%00100000
            BNE EndLoopO
            LDA #$01            ;01 will mean over a climbable
            STA playerdata+Entity::env
            JMP ReturnFromCollO
        MaskOutTwoO:
            LDX colltemp2
            LDA COLLMAPBANK, x
            AND #%00001100 ;the bit set to the left of three
            CMP #%00001000
            BNE EndLoopO
            LDA #$01            ;01 will mean over a climbable
            STA playerdata+Entity::env
            JMP ReturnFromCollO
        MaskOutThreeO:
            LDX colltemp2
            LDA COLLMAPBANK, x
            AND #%00000011 ;the bit set to the left of three
            CMP #%00000010
            BNE EndLoopO
            LDA #$01            ;01 will mean over a climbable
            STA playerdata+Entity::env
            JMP ReturnFromCollO
        EndLoopO:
            INC checkvar
            LDA checkvar
            CMP #$01
            BEQ COSetupLoopOne
            CMP #$02
            BEQ COSetupLoopTwo
            CMP #$03
            BEQ COSetupLoopThree
            ;after loop three (fourth iteration) no env is set and subroutine returns
            JMP ReturnFromCollO
        COSetupLoopOne: ;tr corner
            LDA playerdata+Entity::xpos
            CLC
            ADC #$0D    ;add 13
            LSR
            LSR
            LSR
            LSR
            STA colltemp3   ;collision map x position
            LSR
            LSR
            STA colltemp1
            LDA playerdata+Entity::ypos
            CLC
            ADC #$03
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
            JMP CheckOverLoop
        COSetupLoopTwo:     ;bl corner
            LDA playerdata+Entity::xpos
            CLC
            ADC #$03
            LSR
            LSR
            LSR
            LSR
            STA colltemp3   ;collision map x position
            LSR
            LSR
            STA colltemp1
            LDA playerdata+Entity::ypos
            CLC
            ADC #$0D    ;+13
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
            JMP CheckOverLoop
        COSetupLoopThree:   ;br corner
            LDA playerdata+Entity::xpos
            CLC
            ADC #$0D
            LSR
            LSR
            LSR
            LSR
            STA colltemp3   ;collision map x position
            LSR
            LSR
            STA colltemp1
            LDA playerdata+Entity::ypos
            CLC
            ADC #$0D
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
        NoEnvCondition: 
            LDA #$00
            STA playerdata+Entity::env
        ReturnFromCollO:
            LDA #$00
            STA colltemp1
            STA colltemp2
            STA colltemp3
            STA checkvar

            PLA
            TAY
            PLA
            TAX
            PLA
            RTS

    CheckPlayerCollisionAgainstEntities:
            PHA
            TXA
            PHA
            TYA
            PHA
            LDX #$00
            LDY #$00
            LDA #$00
        CheckAgainstEntitiesLoop:
            TXA
            CLC
            ADC #$0C
            TAX
            LDA ENTITIES, x
            CMP #$FF        ;$FF is end of list
            BEQ EndCheckAgainstEntities
        FirstXCheck:        ;is right side of player greater than left side of entity? If not, return.
            INX             ;index entity's xpos
            LDA ENTITIES, x
            CLC
            ADC #$02        ;enemy hitbox
            STA colltemp1
            LDA playerdata+Entity::xpos
            CLC
            ADC #$0D        ;right side of character. Decrease to change right side of hurtbox
            CMP colltemp1     ;change enemy hitbox
            BPL SecondXCheck    ;if plus, continue
            DEX                 ;otherwise, move to next entity
            JMP CheckAgainstEntitiesLoop
        SecondXCheck:
            LDA ENTITIES, x
            CLC
            ADC #$0E
            STA colltemp1       ;get right side position of entity
            LDA playerdata+Entity::xpos
            CLC
            ADC #$03            ;hurtbox 3px in
            CMP colltemp1
            BMI FirstYCheck
            DEX
            JMP CheckAgainstEntitiesLoop
        FirstYCheck:
            INX
            LDA ENTITIES, x
            CLC
            ADC #$06        ;top side hitbox, snakes are low to ground. Will need a hitbox value for different enemiess
            STA colltemp1
            LDA playerdata+Entity::ypos
            CLC
            ADC #$0D            ;hurtbox val, 3px in
            CMP colltemp1
            BPL SecondYCheck
            DEX
            DEX
            JMP CheckAgainstEntitiesLoop
        SecondYCheck:
            LDA ENTITIES, x
            CLC
            ADC #$0E
            STA colltemp1
            LDA playerdata+Entity::ypos
            CLC
            ADC #$03        ;hurtbox 3px in
            CMP colltemp1
            BMI EntityCollisionTrue
            DEX
            DEX
            JMP CheckAgainstEntitiesLoop
        EntityCollisionTrue:
            DEX
            DEX
            LDA ENTITIES, x
            STA entcollisionret, y          ;With this system, only the first 3 entities in the ENTITIES bank will get returned. 
            INY                             ;It is very unlikely there will be three simultaneous sprite collisions in this game.
            CPY #$03
            BEQ ReturnFromCheckAgainstEntities
            JMP CheckAgainstEntitiesLoop  
        EndCheckAgainstEntities:            ;If end of list is reached before 3 hits occur (usual case)
            LDA #$FF
            STA entcollisionret, y
        ReturnFromCheckAgainstEntities:
            PLA
            TAY
            PLA
            TAX
            PLA
            RTS

EntityCollisionDetection:
    CheckEntityCollisionDown:
        CheckEntityCollisionDownStart: 
            PHA
            TXA
            PHA
            TYA
            PHA
            
            LDA entitybuffer+Entity::xpos
            CLC
            ADC #$00    ;pixel offset if needed
            TAX         ;store in X reg
            CLC
            ADC #$10    ;plus 14 more to get one pixel in from right edge - x2
            TAY         ;store on Y
            LDA entitybuffer+Entity::ypos
            CLC
            ADC #$10    ;y pos of entity's bottom edge (feet) - y
            CMP #$FD    ;Bottom of screen?
            ;BEQ OffScreenDestroy
            CMP #$FE
            ;BEQ OffSCreenDestroy
            LSR         ;divide by 16
            LSR         ;meta y of entity's feet
            LSR
            LSR
            ASL
            ASL
            STA checkvar         ;store in checkvar
            ;Get collmap position of (x1,y) and (x2, y) then check if tile below is solid. If both are NOT solid (== $01), then fall.
            ;1) Divide x by 16
            ;2) That number + y = test cell
            ;3) if collmap, test cell is $01, solid.
        ETestX1:
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
        ETestX2:
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
        EContinueTest:
            LDA colltemp2   ;0, 1, 2 or 3
            CLC
            ADC checkvar    ;x1 (col) + target y (row)  ; multiple of 4
            TAX
            LDA COLLMAPBANK, x ;get target byte
            STA colltemp1
            PLA     ;get back meta x
            AND #%00000011
            CMP #$00
            BEQ EMaskOutZero
            CMP #$01
            BEQ EMaskOutOne
            CMP #$02
            BEQ EMaskOutTwo
            CMP #$03
            BEQ EMaskOutThree ;may be to far to branch
            JMP EReturnFromCollisionDown
        ;ExitDown:
        ;    LDA #$02
        ;    STA exitdir
        ;    LDY #$02        ;offset to "down" exit id
        ;    LDA (level+Area::selfaddr1), y  ;exit id is the selfid of the next area, which should also be the offset into the AREABANK array
        ;    STA nextareaid  ;get id of next area 
        ;    JSR SetupNextArea   ;populate "nextarea" struct for draw routine.
        ;    LDA #$06        ;Load next area game state
        ;    STA gamestate
        ;    JMP ReturnFromCollisionDown
        EMaskOutZero:
            LDA colltemp1
            AND #%11000000
            LSR
            LSR
            LSR
            LSR
            LSR
            LSR
            JMP EResolve
        EMaskOutOne:
            LDA colltemp1
            AND #%00110000
            LSR
            LSR
            LSR
            LSR
            JMP EResolve
        EMaskOutTwo:
            LDA colltemp1
            AND #%00001100
            LSR
            LSR
            JMP EResolve
        EMaskOutThree:
            LDA colltemp1
            AND #%00000011
        EResolve:
            CMP #$01
            BEQ EReturnSolid     ;if x1 or x2 is over solid, we can stop here
            CMP #$03            ;if x1 or x2 is over damaging, we can resolve?
            BEQ EReturnSolid
        EContinueResolve:
            LDA colltemp3       ;holding whether we checked x1 or x2
            CMP #$01            ;if both points checked and no solid below, we must be falling
            BEQ ESetFalling
            JMP TestX2
        ESetFalling:
            LDA entitybuffer+Entity::state
            ORA #%00100000  ;turn on falling
            AND #%11111110  ;turn off standing
            STA entitybuffer+Entity::state
            JMP EReturnFromCollisionDown
        EReturnSolid:            ;resolve collision here. in future, may be wise to separate resolutinon from check
            LDA entitybuffer+Entity::ypos
            AND #%11110000      ;return to "top" of metatile by zeroing out the low nibble (any pixels "below" the multiple of 16)
            STA entitybuffer+Entity::ypos
            LDA entitybuffer+Entity::state
            ORA #%00000001      ;set status to standing
            AND #%11011111      ;if falling, stop falling
            STA entitybuffer+Entity::state
        EReturnFromCollisionDown:
            PLA
            TAY
            PLA
            TAX
            PLA
            RTS
    CheckEntityCollisionLeft:
        CheckEntityCollisionLeftStart:
            PHA
            TXA
            PHA
            LDA entitybuffer+Entity::xpos
            LSR
            LSR
            LSR
            LSR     ;meta x
            TAX
            LSR
            LSR
            STA colltemp1
            LDA entitybuffer+Entity::ypos
            LSR
            LSR
            LSR
            LSR     ;meta y
            ASL
            ASL
            CLC
            ADC colltemp1   ;Add x byte to Y byte to find byte location in collmap
            STA colltemp2   ;Location of player byte in coll map
            CLC
            ADC #$04
            STA colltemp3   ;lower level for ledge detection
            TXA             ;get meta x back
            AND #%00000011
            CMP #$00
            BEQ EMaskOutZeroL
            CMP #$01
            BEQ EMaskOutOneL
            CMP #$02
            BEQ EMaskOutTwoL
            CMP #$03
            BNE EJumpToReturnL
            JMP EMaskOutThreeL
        EJumpToReturnL:
            JMP EReturnFromCollL
        EMaskOutZeroL:
            LDX colltemp2
            LDA COLLMAPBANK, x
            AND #%11000000 
            CMP #%01000000      ;solid
            BNE EZeroCheckLedgeL
            INC entitybuffer+Entity::xpos
            LDA #$02
            STA collreturnval
            JMP EReturnFromCollL
        EZeroCheckLedgeL:
            LDX colltemp3
            LDA COLLMAPBANK, x
            AND #%11000000 ;the bit set to the left of three
            CMP #%01000000      ;solid
            BEQ EZeroNoCollisionL
            CMP #%11000000      ;solid
            BEQ EZeroNoCollisionL
            INC entitybuffer+Entity::xpos
            LDA #$02
            STA collreturnval
            JMP EReturnFromCollL
        EZeroNoCollisionL:
            JMP EReturnFromCollL
        EMaskOutOneL:
            LDX colltemp2
            LDA COLLMAPBANK, x
            AND #%00110000 ;the bit set to the left of three
            CMP #%00010000      ;solid
            BNE EOneCheckLedgeL
            INC entitybuffer+Entity::xpos
            LDA #$02
            STA collreturnval
            JMP EReturnFromCollL
        EOneCheckLedgeL:
            LDX colltemp3
            LDA COLLMAPBANK, x
            AND #%00110000 ;the bit set to the left of three
            CMP #%00010000      
            BEQ EOneNoCollisionL
            CMP #%00110000      
            BEQ EOneNoCollisionL
            INC entitybuffer+Entity::xpos
            LDA #$02
            STA collreturnval
            JMP EReturnFromCollL
        EOneNoCollisionL:
            JMP EReturnFromCollL
        EMaskOutTwoL:
            LDX colltemp2
            LDA COLLMAPBANK, x
            AND #%00001100 ;the bit set to the left of three
            CMP #%00000100         ;solid
            BNE ETwoCheckLedgeL
            INC entitybuffer+Entity::xpos
            LDA #$02
            STA collreturnval
            JMP EReturnFromCollL
        ETwoCheckLedgeL:
            LDX colltemp3
            LDA COLLMAPBANK, x
            AND #%00001100 ;the bit set to the left of three
            CMP #%00000100      
            BEQ ETwoNoCollisionL
            CMP #%00001100      
            BEQ ETwoNoCollisionL
            INC entitybuffer+Entity::xpos
            LDA #$02
            STA collreturnval
            JMP EReturnFromCollL
        ETwoNoCollisionL:
            JMP EReturnFromCollL
        EMaskOutThreeL:
            LDX colltemp2
            LDA COLLMAPBANK, x
            AND #%00000011 ;the bit set to the left of three
            CMP #%00000001      ;solid
            BNE EThreeCheckLedgeL
            INC entitybuffer+Entity::xpos
            LDA #$02
            STA collreturnval
            JMP EReturnFromCollL
        EThreeCheckLedgeL:
            LDX colltemp3
            LDA COLLMAPBANK, x
            AND #%00000011 ;the bit set to the left of three
            CMP #%00000001      
            BEQ EReturnFromCollL
            CMP #%00000011      
            BEQ EReturnFromCollL
            INC entitybuffer+Entity::xpos
            LDA #$02
            STA collreturnval
            JMP EReturnFromCollL
        EReturnFromCollL:
            PLA
            TAX
            PLA
            RTS
    CheckEntityCollisionRight:
        CheckEntityCollisionRightStart:
            PHA
            TXA
            PHA
            LDA entitybuffer+Entity::xpos
            CLC
            ADC #$10
            LSR
            LSR
            LSR
            LSR ;meta x
            TAX
            LSR
            LSR
            STA colltemp1
            LDA entitybuffer+Entity::ypos
            LSR
            LSR
            LSR
            LSR ;meta y
            ASL
            ASL
            CLC
            ADC colltemp1   ;Add x byte to Y byte to find byte location in collmap
            STA colltemp2   ;Location of entity byte in coll map
            CLC
            ADC #$04
            STA colltemp3
            ;find bit pair
            ;LDA playerdata+Entity::metax
            TXA             ;get meta x back
            AND #%00000011
            CMP #$00
            BEQ EMaskOutZeroR
            CMP #$01
            BEQ EMaskOutOneR
            CMP #$02
            BEQ EMaskOutTwoR
            CMP #$03
            BNE EJumpToReturnR
            JMP EMaskOutThreeR
        EJumpToReturnR:
            JMP EReturnFromCollR
        EMaskOutZeroR:
            LDX colltemp2
            LDA COLLMAPBANK, x
            AND #%11000000 
            CMP #%01000000          ;wall
            BNE EZeroCheckLedgeR    
            DEC entitybuffer+Entity::xpos
            LDA #$02
            STA collreturnval       ;return wall hit
            JMP EReturnFromCollR
        EZeroCheckLedgeR:
            LDX colltemp3           ;down a row
            LDA COLLMAPBANK, x
            AND #%11000000
            CMP #%01000000          ;solid (i.e. not a ledge)
            BEQ EZeroNoCollisionR
            CMP #%11000000          ;solid (i.e. not a ledge)
            BEQ EZeroNoCollisionR
            DEC entitybuffer+Entity::xpos
            LDA #$02
            STA collreturnval       ;return wall hit (for now, change value to be a ledge hit)
        EZeroNoCollisionR:
            JMP EReturnFromCollR
        EMaskOutOneR:
            LDX colltemp2
            LDA COLLMAPBANK, x
            AND #%00110000 
            CMP #%00010000          ;solid
            BNE EOneCheckLedgeR
            DEC entitybuffer+Entity::xpos
            LDA #$02
            STA collreturnval       ;return wall hit
            JMP EReturnFromCollR
        EOneCheckLedgeR:
            LDX colltemp3
            LDA COLLMAPBANK, x
            AND #%00110000
            CMP #%00010000         ;solid
            BEQ EOneNoCollisionR
            CMP #%00110000         ;solid
            BEQ EOneNoCollisionR
            DEC entitybuffer+Entity::xpos
            LDA #$02
            STA collreturnval       ;return wall hit (for now, change value to be a ledge hit)
        EOneNoCollisionR:
            JMP EReturnFromCollR
        EMaskOutTwoR:
            LDX colltemp2
            LDA COLLMAPBANK, x
            AND #%00001100 ;the bit set to the left of three
            CMP #%00000100          ;solid
            BNE ETwoCheckLedgeR
            DEC entitybuffer+Entity::xpos
            LDA #$02
            STA collreturnval       ;return wall hit
            JMP EReturnFromCollR
        ETwoCheckLedgeR:
            LDX colltemp3
            LDA COLLMAPBANK, x
            AND #%00001100 ;the bit set to the left of three
            CMP #%00000100          ;solid
            BEQ ETwoNoCollisionR
            CMP #%00001100          ;solid
            BEQ ETwoNoCollisionR
            DEC entitybuffer+Entity::xpos
            LDA #$02
            STA collreturnval       ;return wall hit (for now, change value to be a ledge hit)
        ETwoNoCollisionR:
            JMP EReturnFromCollR
        EMaskOutThreeR:
            LDX colltemp2
            LDA COLLMAPBANK, x
            AND #%00000011 
            CMP #%00000001          ;solid
            BNE EThreeCheckLedgeR
            DEC entitybuffer+Entity::xpos
            LDA #$02
            STA collreturnval       ;return wall hit
            JMP EReturnFromCollR
        EThreeCheckLedgeR:
            LDX colltemp3
            LDA COLLMAPBANK, x
            AND #%00000011 
            CMP #%00000001          ;solid
            BEQ EReturnFromCollR    ;BEQ here because the other possibilities (right now) are nothing or climbable, both of which mean
            CMP #%00000011          ;solid
            BEQ EReturnFromCollR    ;BEQ here because the other possibilities (right now) are nothing or climbable, both of which mean

            DEC entitybuffer+Entity::xpos
            LDA #$02
            STA collreturnval       ;return wall hit
        EReturnFromCollR:
            PLA
            TAX
            PLA
            RTS



;------------------Entity Change Direction Subroutine------------------------------
ChangePlayerFacing: ;push A and Y, doesn't use x
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
    LDA playerdata+Entity::pal
    EOR #$04
    STA playerdata+Entity::pal
    PLA
    TAY
    PLA
    RTS

ChangeEntityFacing: ;push A and Y, doesn't use x
    PHA
    TYA
    PHA
    LDA entitybuffer+Entity::tlspr
    TAY
    LDA entitybuffer+Entity::trspr
    STA entitybuffer+Entity::tlspr
    TYA
    STA entitybuffer+Entity::trspr
    LDA entitybuffer+Entity::blspr
    TAY
    LDA entitybuffer+Entity::brspr
    STA entitybuffer+Entity::blspr
    TYA
    STA entitybuffer+Entity::brspr
    LDA entitybuffer+Entity::pal
    EOR #$04
    STA entitybuffer+Entity::pal
    PLA
    TAY
    PLA
    RTS

;;;Cutscene subroutines
;;;--------------------------------------------------------------------------------------;;;
;;;--*--*--*--*--*--*--*--*--*--*--* CUTSCENE SUBROUTINE   *--*--*--*--*--*--*--*--*--*--*--
;;;--------------------------------------------------------------------------------------;;;
PaletteShake: ;Set timer for duration before calling. Standard duration = $4D
    PHA
    TXA
    PHA
    TYA
    PHA
    LDX #$00
    LDY #$00
    
    LDA timer
    CMP #$00
    BEQ ReturnFromPaletteShake
    LDA timer
    AND #%00000100
    CMP #%00000100
    BNE ReturnFromPaletteShake
    INC colltemp2
    LDA colltemp2
    CMP #$01
    BEQ PSPal1
    CMP #$02
    BEQ PSPal2
    CMP #$03
    BEQ PSPal3
    CMP #$00
    BEQ PSPal4
PSPal1:
    LDA #$00
    STA bgpalette
    JMP FinishPaletteShake
PSPal2:
    LDA #$55
    STA bgpalette
    JMP FinishPaletteShake
PSPal3:
    LDA #$00
    STA colltemp2
    LDA #$AA
    STA bgpalette
    JMP FinishPaletteShake
PSPal4:
    LDA #$FF
    STA bgpalette
FinishPaletteShake:
    LDA $2002
    LDA #$23
    STA $2006           ;PPUADDR      nametable1 attribute layer
    LDA #$C0
    STA $2006           ;PPUADDR
    JSR LoadSingleAttributes
ReturnFromPaletteShake:
    PLA
    TAY
    PLA
    TAX
    PLA
    RTS




VictoryCutscene:    ;timer will run still
                    ;use colltemp1, etc. as counters for cutscene animation stages
    PHA
    TXA
    PHA
    TYA
    PHA
    LDX #$00
    LDY #$00
CheckAnimationState:
    LDA colltemp1
    CMP #00
    BEQ VState0
    CMP #$01
    BEQ VState1
    CMP #$02
    BEQ VState2
    CMP #$03
    BEQ VState3
    CMP #$04
    BEQ JumpToVState4
    JMP ReturnFromVictoryCutscene
JumpToVState4:
    JMP VState4
VState0:    ;finish climbing out
    LDA playerdata+Entity::ypos
    CMP #$D0
    BEQ AdvanceToVState1
    DEC playerdata+Entity::ypos
    INC animaframes
    JMP SetClimbingC
AdvanceToVState1:
    LDA playerdata+Entity::state
    ORA #%10000000  ;force face right
    AND #%10111111
    STA playerdata+Entity::state
    LDA #$01
    STA colltemp1
    JMP CopyPDtoEBLoopC
VState1:    ;walk to right side of screen
    LDA playerdata+Entity::xpos
    CMP #$B0
    BEQ AdvanceToVState2
    INC playerdata+Entity::xpos
    INC animaframes
    JMP SetWalkFrameC
AdvanceToVState2:
    LDA playerdata+Entity::state
    ORA #%01000000  ;switch facing
    AND #%01111111
    STA playerdata+Entity::state
    LDA #$02
    STA colltemp1
    LDA #$4D
    STA timer
    LDA #$00
    STA bgpalette
    JMP CopyPDtoEBLoopC
VState2:
    LDA timer
    CMP #$00
    BNE JumpToReturnVC
    LDA #$03            ;Advance to state3
    STA colltemp1
    LDA #$00
    STA colltemp2
    LDA #$AB
    STA timer
    JMP CopyPDtoEBLoopC
JumpToReturnVC:
    JMP ReturnFromVictoryCutscene
VState3:
    LDA timer
    CMP #$00
    BEQ AdvanceToVState4
    LDA timer
    AND #%00000100
    CMP #%00000100
    BNE JumpToReturnVC
    INC colltemp2
    LDA colltemp2
    CMP #$01
    BEQ VCPal1
    CMP #$02
    BEQ VCPal2
    CMP #$03
    BEQ VCPal3
    CMP #$00
    BEQ VCPal4
VCPal1:
    LDA #$00
    STA bgpalette
    JMP FinishVState3
VCPal2:
    LDA #$55
    STA bgpalette
    JMP FinishVState3
VCPal3:
    LDA #$00
    STA colltemp2
    LDA #$AA
    STA bgpalette
    JMP FinishVState3
VCPal4:
    LDA #$FF
    STA bgpalette
FinishVState3:
    LDA $2002
    LDA #$23
    STA $2006           ;PPUADDR      nametable1 attribute layer
    LDA #$C0
    STA $2006           ;PPUADDR
    JSR LoadSingleAttributes
    JMP CopyPDtoEBLoopC
AdvanceToVState4:
    LDA #$00
    STA bgpalette
    LDA $2002
    LDA #$23
    STA $2006           ;PPUADDR      nametable1 attribute layer
    LDA #$C0
    STA $2006           ;PPUADDR
    JSR LoadSingleAttributes
    LDA #$04
    STA colltemp1
    LDA playerdata+Entity::state
    ORA #%10000000      ;face right again
    AND #%10111111
    STA playerdata+Entity::state
    JMP CopyPDtoEBLoopC
VState4:
    LDA playerdata+Entity::xpos
    CMP #$F8
    BEQ FinishCutscene
    INC playerdata+Entity::xpos
    INC animaframes
    JMP SetWalkFrameC
FinishCutscene:
    LDA #$00
    STA gamestate
    LDA #$FF
    STA ENTITIES
    LDA #<VICTORYSCREEN
    STA ptr           ;utilize block table pointer for text load
    LDA #>VICTORYSCREEN
    STA ptr+1

    JSR InitializeAltScreen
    JMP ReturnFromVictoryCutscene

SetClimbingC:
    LDA #$04
    STA playerdata+Entity::tlspr
    LDA #$05
    STA playerdata+Entity::trspr
    LDA #$14
    STA playerdata+Entity::blspr
    LDA #$15
    STA playerdata+Entity::brspr
    LDA animaframes
    LSR
    LSR
    LSR
    LSR
    BCS SetClimbingTwoC
SetClimbingOneC:
    LDA playerdata+Entity::state
    ORA #%10000000
    AND #%10111111
    STA playerdata+Entity::state
    JMP CopyPDtoEBLoopC
SetClimbingTwoC:
    LDA playerdata+Entity::state
    ORA #%01000000
    AND #%01111111
    STA playerdata+Entity::state
    JMP CopyPDtoEBLoopC

SetWalkFrameC:
    LDA animaframes
    LSR
    LSR
    LSR
    LSR
    BCS SetWalkTwoC
SetWalkOneC:
    LDA #$00
    STA playerdata+Entity::tlspr
    LDA #$01
    STA playerdata+Entity::trspr
    LDA #$12
    STA playerdata+Entity::blspr
    LDA #$02
    STA playerdata+Entity::brspr
    JMP CopyPDtoEBLoopC
SetWalkTwoC:
    LDA #$00
    STA playerdata+Entity::tlspr
    LDA #$01
    STA playerdata+Entity::trspr
    LDA #$13
    STA playerdata+Entity::blspr
    LDA #$03
    STA playerdata+Entity::brspr

CopyPDtoEBLoopC:
    LDA playerdata, x
    STA entitybuffer, x
    INX
    INY
    CPY #$0B    ;12 items in entity struct
    BNE CopyPDtoEBLoopC
    LDX #$00    ;X = ENTITIES index of player + 1
    LDA #$01
    STA checkvar    ;set checkvar to 1 so subroutine does not mess with stack
    JSR WriteEntityFromBuffer ;Subroutine to write all the changes made to player this frame

ReturnFromVictoryCutscene:
    PLA
    TAY
    PLA
    TAX
    PLA
    RTS




;;;RAM loading subroutines
;;;--------------------------------------------------------------------------------------;;;
;--*--*--*--*--*--*--*--*--* STORE ENTITIES SUBROUTINE   *--*--*--*--*--*--*--*--*--*--*--
;;;--------------------------------------------------------------------------------------;;;
;WriteEntity used mainly to update entities after begin processed.
;May never use the first part tbh.
WriteEntityFromBuffer: ;can this loop with y?   Call this is a subroutine to update. Set checkvar to #$01 (or anything but 00)
;IN <--- X = index of ENTITIES array + 1
    PHA
    TXA
    PHA
    TYA
    PHA


    LDA entitybuffer+Entity::type
    STA ENTITIES, x     ;overwrite $FF
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
    LDA entitybuffer+Entity::pal
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
    LDA entitybuffer+Entity::env
    STA ENTITIES, x
    INX
    LDA entitybuffer+Entity::metax
    STA ENTITIES, x
    INX
    LDA entitybuffer+Entity::metay
    STA ENTITIES, x
    ;INX
    ;LDA #$FF
    ;STA ENTITIES, x
    PLA
    TAY
    PLA
    TAX
    PLA
ReturnFromWriteEntity:
    RTS

;;;--------------------Load Text From ROM--------------------------------;;;
;;;---Use to load prewritten text like title screen or any story text----;;;
;;;---Destination: TEXTBANK = $0580--------------------------------------;;;
;;;---IN: ptr variable with address of desired text block----------------;;;
LoadTextFromROM:
    PHA
    TXA
    PHA
    TYA
    PHA
    LDX #$00
    LDY #$00
FindEndOfBankLoop:
    LDA TEXTBANK, x
    CMP #$00
    BEQ LoadTextLoop
    INX
    JMP FindEndOfBankLoop
LoadTextLoop:
    LDA (ptr), y
    CMP #$FF        ;$FF is end of file. We want to copy in all these markers but stop at end of file.
    BEQ ReturnFromLoadTextFromROM
    STA TEXTBANK, x
    INY
    INX
    JMP LoadTextLoop
ReturnFromLoadTextFromROM:
    STA TEXTBANK, x
    PLA
    TAY
    PLA
    TAX
    PLA    
    RTS


;;;----------------------------------------------------------------------------------------------;;;
;-------------------Load Screen Subroutines----------------------------------------------------------
;;;----------------------------------------------------------------------------------------------;;;

InitializeMainGame:
    PHA
    TXA
    PHA
    TYA
    PHA
    LDX #$00
    LDY #$00
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
WriteAreaBankLoop:      ;Write all areas PRGROM addresses into RAM lookup table
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
    INX
    LDA #<AREA2
    STA AREABANK, x
    INX
    LDA #>AREA2
    STA AREABANK, x
    INX
    LDA #<AREA3
    STA AREABANK, x
    INX
    LDA #>AREA3
    STA AREABANK, x
    INX
    LDA #<AREA4
    STA AREABANK, x
    INX
    LDA #>AREA4
    STA AREABANK, x
    INX
    LDA #<AREA5
    STA AREABANK, x
    INX
    LDA #>AREA5
    STA AREABANK, x
    LDX #$00

    JSR LoadUI
InitializeFirstScreen:
    LDA AREABANK                ;set AREA0 as current level
    STA level+Area::selfaddr1
    LDA AREABANK+1
    STA level+Area::selfaddr2
    ;block table
    LDA level+Area::selfaddr1
    CLC
    ADC #$8C
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
    ADC #$10
    STA level+Area::cmaddr1
    LDA level+Area::selfaddr2
    STA level+Area::cmaddr2
    BCS IncrementCMHighByte
    JMP InitializePaletteMap
IncrementCMHighByte:
    INC level+Area::cmaddr2
InitializePaletteMap:
    LDA level+Area::selfaddr1
    CLC
    ADC #$4C
    STA level+Area::paladdr1
    LDA level+Area::selfaddr2
    STA level+Area::paladdr2
    BCS IncrementPMHighByte
    JMP InitializeEntities
IncrementPMHighByte:
    INC level+Area::paladdr2
InitializeEntities:
    LDA level+Area::selfaddr1
    CLC
    ADC #$0B
    STA level+Area::entaddr1
    LDA level+Area::selfaddr2
    STA level+Area::entaddr2
    BCS IncrementEHighByte
    JMP ContinueToInitPlayer
IncrementEHighByte:
    INC level+Area::entaddr2

ContinueToInitPlayer:
    LDX #$00
InitializePlayer:
    LDA #EntityType::Player
    STA playerdata+Entity::type
    STA ENTITIES, x ;type
    INX
    LDA #$20
    STA playerdata+Entity::xpos
    STA ENTITIES, x ;xpos
    INX   
    LDA #$10
    STA playerdata+Entity::ypos
    STA ENTITIES, x ;ypos
    INX
    LDA #%10100000  ;falling, facing right
    STA playerdata+Entity::state
    STA ENTITIES, x ;state
    INX
    LDA #$00
    STA playerdata+Entity::pal
    STA ENTITIES, x ;palette
    INX
    LDA #$00
    STA playerdata+Entity::tlspr
    STA ENTITIES, x ;sprite 1
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
    ;LDA #$00
    ;STA playerdata+Entity::tlspr
    ;STA ENTITIES, x ;sprite 2, head doesn't change
    ;INX
    ;LDA #$01
    ;STA playerdata+Entity::trspr
    ;STA ENTITIES, x
    ;INX
    ;LDA #$12
    ;STA playerdata+Entity::blspr
    ;STA ENTITIES, x
    ;INX
    ;LDA #$13
    ;STA playerdata+Entity::brspr
    ;STA ENTITIES, x
    ;INX
    LDA #$00        ;no env
    STA playerdata+Entity::env
    STA ENTITIES, x
    INX
    STA ENTITIES, x     ;zero out meta x
    INX
    STA ENTITIES, x     ;zero out meta y
    INX
    ;16 writes to ENTITIES array
    ;End of Array
    LDA #$FF
    STA ENTITIES, x
    INX
    LDA #$03            ;i don't anticipate this changing
    STA playerdata+Entity::health
;;----->>>>
    LDA level+Area::btaddr1
    STA btptr
    LDA level+Area::btaddr2
    STA btptr+1
    LDA level+Area::cmaddr1
    STA cmptr
    LDA level+Area::cmaddr2
    STA cmptr+1
    LDA level+Area::paladdr1
    STA palptr
    LDA level+Area::paladdr2
    STA palptr+1
    LDA level+Area::entaddr1
    STA entptr
    LDA level+Area::entaddr2
    STA entptr+1
    JSR LoadEntitiesFromROM
    LDA #$00
    STA colltemp1
    JSR LoadNametable
    LDA #$23
    STA $2006           ;PPUADDR      nametable1 attribute layer
    LDA #$C0
    STA $2006           ;PPUADDR
    ;LDA #$00
    ;STA bgpalette
    JSR LoadAttributes
    JSR LoadCollMap

    LDA #$1E
    STA $2001
    LDA $2000
    ORA #%10000000
    STA $2000

    LDA #$00
    STA spritemem
    LDA #$02
    STA spritemem+1

    PLA
    TAY
    PLA
    TAX
    PLA    
    RTS
;----------------------------------------------------------------
InitializeAltScreen: ;define ptr/ptr+1 with address of text table before coming in
    NOP
    NOP
    NOP
    NOP
    NOP

    PHA
    TXA
    PHA
    TYA
    PHA
    LDX #$00
    LDY #$00
    LDA $2000
    AND #%01111111
    STA $2000
    LDA #$00
    STA $2001
    ;Clear game RAM
    ;0440-06FF
    LDA #$00
    LDX #$00
Clear440Loop:
    STA AREABANK, x
    INX
    CPX #$00
    BNE Clear440Loop
Clear500Loop:
    STA COLLMAPBANK, x
    INX
    CPX #$00
    BNE Clear500Loop
Clear600Loop:
    STA ENTITIES, x
    INX
    CPX #$00
    BNE Clear600Loop
    LDA #$FF
    STA ENTITIES
Clear200Loop:
    STA $0200, x    ;Fills $0200 with $FF, not 0. 
    INX
    CPX #$00
    BNE Clear200Loop

    LDA #$00
    STA timer
    ;LDA #<GAMEOVERSCREEN
    ;STA ptr           ;utilize block table pointer for text load
    ;LDA #>GAMEOVERSCREEN
    ;STA ptr+1
    JSR LoadTextFromROM  ;Load Title Screen text into TEXTBANK ($0580) [THIS WORKS]
    
    LDA $2002           ;PPUSTATUS    Is he reading to clear vblank flag? Neads to be changed to use NMI instead
    LDA #$20
    STA $2006           ;PPUADDR      we are using $2000 for graphics memory
    LDA #$00
    STA $2006           ;PPUADDR

    JSR LoadBlankNametable 
    
    LDA $2002           ;PPUSTATUS    Is he reading to clear vblank flag? Neads to be changed to use NMI instead
    LDA #$28
    STA $2006           ;PPUADDR      we are using $2000 for graphics memory
    LDA #$00
    STA $2006           ;PPUADDR
    JSR LoadBlankNametable
    LDA #$23
    STA $2006           ;PPUADDR      nametable1 attribute layer
    LDA #$C0
    STA $2006           ;PPUADDR
    LDA #%01010101
    STA bgpalette
    JSR LoadSingleAttributes
    JSR DrawText


    LDA #$1E
    STA $2001
    LDA $2000
    ORA #%10000000
    STA $2000

    LDA #$00
    STA spritemem
    LDA #$02
    STA spritemem+1

    LDA #$00
    STA gamestate   ;game over screen behaves exactly the same as title screen

    PLA
    TAY
    PLA
    TAX
    PLA    
    RTS

;add hearts to UI later?
;Bank UI elements in ROM later and load with pointer and loop
LoadUI: ;UI format - y, spr, pal, x (follow OAM format)
    PHA
    TXA
    PHA
    TYA
    PHA

    LDX #$00        ;UI bank index
    
    LDA #$1A        ;y
    STA UI, x
    INX
    LDA #$28        
    STA UI, x
    INX
    LDA #$01
    STA UI, x
    INX
    LDA #$D4        ;same x as heart
    STA UI, x
    INX

    LDA #$1A        ;y
    STA UI, x
    INX
    LDA #$29        ;tr spr
    STA UI, x
    INX
    LDA #$01
    STA UI, x
    INX
    LDA #$DC        ;x
    STA UI, x
    INX

    LDA #$22        ;y
    STA UI, x
    INX
    LDA #$38        ;bl spr
    STA UI, x
    INX
    LDA #$01
    STA UI, x
    INX
    LDA #$D4        ;x
    STA UI, x
    INX

EndLoadUI:
    LDA #$FF
    STA UI, x

    PLA     
    TAY
    PLA
    TAX
    PLA    
    RTS


;Call as part of LoadNextArea
LoadEntitiesFromROM:    
    PHA
    TXA
    PHA
    TYA
    PHA

;For right now, start from just after player, which is +12.
;The only entity to persist between screens will be the player.
    LDX #$0C
    LDY #$00
    TYA
    PHA         ;keep original iterator on stack
LoadNextAreaEntitiesLoop:
    LDA (entptr), y ;x/y metavalue
    CMP #$FF        ;or end of list
    BNE StartLoadEntities 
    JMP EndLoadEntitiesFromROM
StartLoadEntities:
    PHA             ;push the x/y value of the entity
    INY
    LDA (entptr), y ;this value is an offset into the ENTPOINTERS list 
    TAY
    LDA #<ENTPOINTERS
    STA ptr
    LDA #>ENTPOINTERS
    STA ptr+1
    LDA (ptr), y    ;Offset into list and get the address of the entity that will be loaded
    STA ptr2
    INY
    LDA (ptr), y
    STA ptr2+1      ;store target entity address into a new pointer
    LDY #$00
    LDA (ptr2), y   ;type/env *good to here
    LSR
    LSR
    LSR
    LSR
    STA ENTITIES, x ;store type 1
    INX
    PLA             ;grab x/y val off stack
    PHA             ;put it back to use again for y calc
    LSR
    LSR
    LSR
    LSR             ;get high byte (x val)
    ASL
    ASL
    ASL
    ASL             ;times 16 (basically clear the low byte to get the x-val)
    STA ENTITIES, x ;store x 2
    INX
    PLA             ;grab x/y val off stack again
    ASL
    ASL
    ASL
    ASL             ;move y-val into high byte, shifts out x-val
    STA ENTITIES, x ;store y 3
    INX
    LDA #%10000001
    STA ENTITIES, x ;store state 4
    INX
    INY
    LDA (ptr2), y   ;palette
    STA ENTITIES, x ;store pal 5
    INX
    INY             ;go past number of frames
    INY             ;grab first frame
    LDA (ptr2), y   ;tlspr 6
    STA ENTITIES, x
    INX
    INY
    LDA (ptr2), y   ;trspr 7
    STA ENTITIES, x
    INX
    INY
    LDA (ptr2), y   ;blspr 8
    STA ENTITIES, x
    INX
    INY
    LDA (ptr2), y   ;brspr 9
    STA ENTITIES, x
    INX
    INY
    ;LDA (ptr2), y   ;number of animation frames that are stored. The ENTITIES bank is only setup to hold 2 frames at a time.
                    ;when loading the entity for the first time, we will always grab the first two frames
    ;CMP #$01
    ;BEQ LoadSingleSprite
;LoadTwoSprites:
;    LDA #$08
;    STA counter
;    INY             ;INC to first sprite ID
;TwoSpriteLoop:
;    LDA (ptr2), y   ;get all 8 sprite tiles
;    STA ENTITIES, x ;(8)
;    INX
;    INY
;    DEC counter
;    LDA counter
;    CMP #$00
;    BNE TwoSpriteLoop
;    JMP EndSpriteLoop
;LoadSingleSprite:
;    LDA #$01        ;if ENTITIES space for sprites expands (unlikely), simply increase this number
;    PHA             ;Need to run two loops of four, so keep another iterator on the stack
;    LDA #$04
;    STA counter
;    INY             ;INC to first sprite ID
;SingleSpriteLoop:
;    LDA (ptr2), y
;    STA ENTITIES, x ;(4 + 4)
;    INX
;    INY
;    DEC counter
;    LDA counter
;    CMP #$00
;    BNE SingleSpriteLoop
;ReloadSingleSpriteLoop:
;    PLA
;    CMP #$00
;    BEQ EndSpriteLoop
;    SEC
;    SBC #$01
;    PHA
;    LDA #$04
;    STA counter
;    DEY
;    DEY
;    DEY
;    DEY             ;run Y back to first sprite tile
;    JMP SingleSpriteLoop
;EndSpriteLoop:
SetFrameCounter:
    LDA #$00
    STA ENTITIES, x ;10
    INX
SetSelfTimer:
    STA ENTITIES, x ;11
    INX
SetVariable:
    STA ENTITIES, x   ;set to zero for now 12
    INX
    PLA
    TAY
    INY
    INY     ;Entity entries are 2 bytes (location/pointer) so INY twice
    TYA
    PHA
    JMP LoadNextAreaEntitiesLoop
EndLoadEntitiesFromROM:
    LDA #$FF
    STA ENTITIES, x
    PLA     ;grab iterator off stack first
    PLA     ;this is old Y
    TAY
    PLA
    TAX
    PLA    
    RTS


;----------------------------------------------------------------
SetupNextArea:  ;gets pointer data from ROM (AREA0, AREA1, ...) and stores it into and Area struct called nextarea
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
    LDY #$07                            ;First byte of "contents" section in AREA array. Contains offset of the entities table
    LDA (nextarea+Area::selfaddr1), y   ;get block table offset (probably $07)
    STA nextarea+Area::entaddr1          ;use as variable for a moment
    LDA nextarea+Area::selfaddr1
    CLC
    ADC nextarea+Area::entaddr1
    STA nextarea+Area::entaddr1
    ;TAY                                 ;put that value into X
    ;LDA (nextarea+Area::selfaddr1), y     ;get the value of X added to lowbyte of the array address. Should be first byte in block table.
    ;STA nextarea+Area::btaddr1
    LDA nextarea+Area::selfaddr2        ;may eventually need to robustify for if btaddr crosses to next memory page, selfaddr2 will need to INC
    STA nextarea+Area::entaddr2
    BCS NAIncrementHighByteENT          ;if carry was set from adding the low byte, that means it crossed a page
    JMP NAContinueToLoadCM
NAIncrementHighByteENT:
    INC nextarea+Area::entaddr2
NAContinueToLoadCM:
    LDY #$08
    LDA (nextarea+Area::selfaddr1), y   ;get collmap offset, repeat process
    STA nextarea+Area::cmaddr1          ;use as variable for a moment
    LDA nextarea+Area::selfaddr1
    CLC
    ADC nextarea+Area::cmaddr1
    STA nextarea+Area::cmaddr1
    LDA nextarea+Area::selfaddr2        ;may eventually need to robustify for if btaddr crosses to next memory page, selfaddr2 will need to INC
    STA nextarea+Area::cmaddr2
    BCS NAIncrementHighByteCM
    JMP NAContinueToLoadPM
NAIncrementHighByteCM:
    INC nextarea+Area::cmaddr2
NAContinueToLoadPM:
    LDY #$09
    LDA (nextarea+Area::selfaddr1), y   ;get collmap offset, repeat process
    STA nextarea+Area::paladdr1          ;use as variable for a moment
    LDA nextarea+Area::selfaddr1
    CLC
    ADC nextarea+Area::paladdr1
    STA nextarea+Area::paladdr1
    LDA nextarea+Area::selfaddr2        ;may eventually need to robustify for if btaddr crosses to next memory page, selfaddr2 will need to INC
    STA nextarea+Area::paladdr2
    BCS NAIncrementHighBytePM
    JMP NAContinueToLoadBT
NAIncrementHighBytePM:
    INC nextarea+Area::paladdr2
NAContinueToLoadBT:
    LDY #$0A
    LDA (nextarea+Area::selfaddr1), y   ;get entity table offset, repeat process
    STA nextarea+Area::btaddr1          ;use as variable for a moment
    LDA nextarea+Area::selfaddr1
    CLC
    ADC nextarea+Area::btaddr1
    STA nextarea+Area::btaddr1
    LDA nextarea+Area::selfaddr2        
    STA nextarea+Area::btaddr2
    BCS NAIncrementHighByteBT
    JMP NAReturn
NAIncrementHighByteBT:
    INC nextarea+Area::btaddr2
NAReturn:
    ;Done. "nextarea" struct is populated.
    PLA
    TAY
    PLA
    TAX
    PLA
    RTS



;-------------------------------------------------------------------------------------------------------------------------
;Background Draw methods
;Upon game state changing to transition, we need to fill both nametables in $2007 then scroll between them. We'll receive the BLOCKTABLE
;COLLMAP for the new screen and hold them in a pointer somewhere, or a reference variable for a pointer. Then LoadTransition can fill both
;nametables in PPU memory and increment the correct scroll, h or v (First I'm only messing around with v). The idea is to get a Zelda-like
;screen transition.
;LoadNametable loads one name table (not both). Need to make a way to call for "level" then "nextarea" when transitioning screens

LoadBlankNametable:
    PHA
    TXA
    PHA
    TYA
    PHA

    LDA #$D0
    LDX #$04
    LDY #$00
BlankNTLoop:
    STA $2007
    INY
    BNE BlankNTLoop
    DEX
    BNE BlankNTLoop
FinishedBlankWrite:
    PLA
    TAY
    PLA
    TAX
    PLA
    RTS








LoadNametable:  ;copied to scratch
                ;need an IN argument to hold whether Loading for "level" or "nextarea"
        PHA
        TXA
        PHA
        TYA
        PHA

        LDA colltemp1   ;00 for "level", 01 for "nextarea"
        CMP #$01
        BEQ UseNATileset
    UseLevelTileset:
        ;load tileset pointer
        LDA level+Area::selfaddr1
        ;LDA #<BGTILES0   ;high byte
        STA ptr+0
        LDA level+Area::selfaddr2
        ;LDA #>BGTILES0   ;low byte
        STA ptr+1
        LDY #$05
        LDA (ptr), y
        PHA
        INY
        LDA (ptr), y
        STA ptr+1
        PLA
        STA ptr
        JMP ContinueLoadNametable
    UseNATileset:
        ;load tileset pointer
        LDA nextarea+Area::selfaddr1
        ;LDA #<BGTILES0   ;high byte
        STA ptr+0
        LDA nextarea+Area::selfaddr2
        ;LDA #>BGTILES0   ;low byte
        STA ptr+1
        LDY #$05
        LDA (ptr), y
        PHA
        INY
        LDA (ptr), y
        STA ptr+1
        PLA
        STA ptr

    ContinueLoadNametable:
        LDA #$00
        STA counter
        STA colltemp1   ;reuse as a temp variable
        LDX #$00        ;outside counter goes to 15
        LDY #$00     
        TXA             ;$2007 write counter push to stack
        PHA             
    ClearBlockRow:
        TYA
        PHA
        LDA #$FF
        LDY #$00
        LDX #$00
        STA blockrow, y ;First entry in blockrow is FF so if a row is empty it will not display a blokc in first tile.
        INY
        LDA #$00
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
        CMP #$FF
        BEQ BuffCanvasA
        LSR
        LSR
        LSR
        LSR             ;put X value in low 4 bits - works
        STA checkvar
    CheckTileA:
        TXA             ;X is current iteration through the buffer, by twos (so that each metatile is placed two positions apart)
        LSR             ;So to check against the "meta" x position, we should shift right.
        CMP checkvar
        BNE BuffCanvasA
        JMP BuffTileA
    checkB:
        LDA blockrow, y ;What is first piece of block data
        CMP #$FF
        BEQ JumpToBuffCanvasB
        LSR
        LSR
        LSR
        LSR             ;put X value in low 4 bits - works
        STA checkvar
    CheckBlockB:
        TXA             ;X is current iteration through the buffer, by twos (so that each metatile is placed two positions apart)
        LSR             ;So to check against the "meta" x position, we should shift right.
        CMP checkvar
        BNE JumpToBuffCanvasB
        JMP BuffTileB
    JumpToBuffCanvasB:
        JMP BuffCanvasB

    BuffTileA:          ;Put entire metatile into first 32 byte buffer
        LDA blockrow, y ;re-get current tile from blocktable
        AND #%00001111  ;mask out high nibble
        STA colltemp1   ;borrow this variable to hold our tile pointer
        TYA
        PHA             ;push Y (block row iterator) to the stack
        LDA colltemp1   ;get our tile pointer back into Y
        ASL
        ASL
        TAY
        LDA (ptr), y    ;get tl spr from BGTILES0 library (tl, tr bl, br)
        STA tilebufferA, x
        INX
        INY
        LDA (ptr), y         ;top right
        STA tilebufferA, x
        TXA
        CLC
        ADC #$10
        TAX
        INY
        INY
        LDA (ptr), y         ;bottom right
        STA tilebufferA, x
        DEX
        DEY
        LDA (ptr), y        ;bottom left
        STA tilebufferA, x
        TXA
        SEC
        SBC #$10        ;reset x to starting value
        TAX
        PLA             ;grab old Y iterator for blockrow
        TAY
        INX             ;increment x for next loop
        INX
        CPX #$20
        BEQ JumpToWrite
        INY             ;Incrememnt y because we drew a block
        JMP FillBuffer
    JumpToWrite:
        JMP WriteBuffer
    BuffCanvasA:    ;TODO: use a metatile struct and preload it before calling the buff routine so that it's only written once
        TYA
        PHA
        LDY #$40
        LDA (ptr), y         ;top left CHR address
        STA tilebufferA, x
        INX
        INY
        LDA (ptr), y         ;top right
        STA tilebufferA, x
        TXA
        CLC
        ADC #$10
        TAX
        INY
        LDA (ptr), y         ;bottom right
        STA tilebufferA, x
        DEX
        INY
        LDA (ptr), y        ;bottom left
        STA tilebufferA, x
        PLA
        TAY             ;retrieve previous Y index
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
    BuffTileB:          ;Put entire metatile into second 32 byte buffer
        TXA             ;make sure to handle push/pull in correct order
        PHA
        SEC
        SBC #$10        ;store iterative X first then subtract 16 from X to get a new iterator for second buffer. Grab og X from stack when done.
        TAX
        LDA blockrow, y
        AND #%00001111  ;mask out the high nibble, leaving tile pointer half
        STA colltemp1
        TYA
        PHA             ;push old y blockrow iterator
        LDA colltemp1
        ASL
        ASL
        TAY
        LDA (ptr), y         ;top left CHR address
        STA tilebufferB, x
        INX
        INY
        LDA (ptr), y         ;top right
        STA tilebufferB, x
        TXA
        CLC
        ADC #$10
        TAX
        INY
        INY
        LDA (ptr), y         ;bottom right
        STA tilebufferB, x
        DEX
        DEY
        LDA (ptr), y        ;bottom left
        STA tilebufferB, x
        PLA
        TAY
        PLA
        TAX
        INX             ;increment x for next loop
        INX
        CPX #$20
        BEQ WriteBuffer
        INY             ;Incrememnt y because we drew a block
        JMP FillBuffer
    BuffCanvasB:
        TYA
        PHA

        TXA             
        PHA
        SEC
        SBC #$10        ;store iterative X then subtract 16 from X to get a new iterator for second buffer. Grab og X from stack when done.
        TAX
        LDY #$40
        LDA (ptr), y         ;top left CHR address
        STA tilebufferB, x
        INX
        INY
        LDA (ptr), y         ;top right
        STA tilebufferB, x
        TXA
        CLC
        ADC #$10
        TAX
        INY
        LDA (ptr), y         ;bottom right
        STA tilebufferB, x
        DEX
        INY
        LDA (ptr), y        ;bottom left
        STA tilebufferB, x
        PLA
        TAX
        PLA
        TAY

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
        CPX #$0F       ;15 metarows
        BEQ FinishedBGWrite
    FinishedBuffer:
        TXA
        PHA
        LDY counter
        JMP ClearBlockRow
    FinishedBGWrite:
        LDA #$00
        STA checkvar
        STA colltemp1
        STA ptr
        STA ptr+1
        PLA
        TAY
        PLA
        TAX
        PLA
        RTS



;This should work but it needs to run during vblank
AddLavaToTileBuffer: ;adds one row of lava tiles to nametable for each value in lavacounter. Alternates sprite referenec per row.
    PHA
    TXA
    PHA
    TYA
    PHA
    
    LDA lavacounter
    ASL
    ASL
    ASL
    ASL
    ASL ;x32
    STA colltemp2
    LDA #$C0
    SEC
    SBC colltemp2
    STA colltemp2   ;jump back however many rows lavacounter requires

;set $2007 to last entry in nametable. Write to both? Or would that mess up the transitions?
    ;LDA $2002
    LDA #$23
    STA lavaaddress
    LDA colltemp2       ;first tile of last row
    STA lavaaddress+1
    LDA #$30    ;tile id for the row
    STA colltemp1
    LDX #$00    ;32 tiles in a row     
    LDY #$00    ;tile id row counter, loop 4 times
AddLavaLoop:
    LDA colltemp1
    STA tilebufferA, x
    INY
    INC colltemp1
    TYA
    CMP #$04
    BNE IncreaseLavaX
    LDY #$00
    DEC colltemp1
    DEC colltemp1
    DEC colltemp1
    DEC colltemp1
IncreaseLavaX:
    INX
    TXA
    CMP #$20
    BNE AddLavaLoop
    ;LDX #$20
    ;PLA         ;grab lavacounter (how high is the lava?)
    ;SEC
    ;SBC #$01
    ;CMP #$00
    ;BEQ ReturnFromAddLavaToTileBuffer
    ;PHA
    ;LDA colltemp1       ;base tile id
    ;EOR #%00000111      ;00000011 bceomes 00000100, and vice versa (toggle 3 and 4 for the sprite id row)
ReturnFromAddLavaToTileBuffer:
    PLA
    TAY
    PLA
    TAX
    PLA
    RTS

;----------------------------------------------------------------------------------------
LoadSingleAttributes:
;To run using the palette map in the AREA array, we need two x counters and a y iterator. The y iterates though the palette map
;and the "inside" counter runs to 
    PHA
    TXA
    PHA
    TYA
    PHA
    ;set address before calling because there are two, one for each nametable
    ;LDA $2002       ;reset latch
    ;LDA #$23        ;High byte of $23CO address (attributes)
    ;STA $2006
    ;LDA #$C0        ;Low byte
    ;STA $2006
    LDX #$40        ;loop counter 64b
    LDY #$00
    ;LDA #$00        ;Attribute value
    LDA bgpalette   ;single palette chooser
LoadSingleAttributesLoop:
    STA $2007
    DEX
    BNE LoadSingleAttributesLoop
ReturnFromLoadSingleAttributes:
    PLA
    TAY
    PLA
    TAX
    PLA
    RTS




;Set attribute register to $2006 BEFORE calling!!!
LoadAttributes:
;To run using the palette map in the AREA array, we need two x counters and a y iterator. The y iterates though the palette map
;and the "inside" counter runs to 
    PHA
    TXA
    PHA
    TYA
    PHA
    ;set address before calling because there are two, one for each COLLMAPnametable
    ;LDA $2002       ;reset latch
    ;LDA #$23        ;High byte of $23CO address (attributes)
    ;STA $2006
    ;LDA #$C0        ;Low byte
    ;STA $2006
    LDX #$40        ;loop counter
    LDY #$00
    ;LDA #$00        ;Attribute value
    ;LDA bgpalette   ;single palette chooser
LoadAttributesLoop:
    LDA (palptr), y ;get palette info from palettemap
    STA $2007
    INY
    DEX
    BNE LoadAttributesLoop
ReturnFromLoadAttributes:
    PLA
    TAY
    PLA
    TAX
    PLA
    RTS



;--*--*--*--*--*--*--*Load Collision Map subroutine*--*--*--*--*--*--*--*--*--*
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

;------------------------------Load NextArea to 2007-----------------------------------------
LoadNextArea:   ;TODO: setup palptr for LoadAttributes
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
    LDA exitdir
    CMP #$01
    BEQ FillNametablesUp
    ;CMP #$02
    ;BEQ FillNametablesDown
    JMP FillNametablesDown
FillNametablesUp:
    LDA $2002           ;PPUSTATUS    Is he reading to clear vblank flag? Neads to be changed to use NMI instead
    LDA #$28
    STA $2006           ;PPUADDR      $2000 for nametable1
    LDA #$00
    STA $2006           ;PPUADDR

    LDA level+Area::btaddr1
    STA btptr
    LDA level+Area::btaddr2
    STA btptr+1
    LDA #$00
    STA colltemp1       ;use as a flag to toggle whether to pull tileset from level struct (00) or next area (01)

    JSR LoadNametable
    LDA #$20
    STA $2006           ;PPUADDR      $2800 for nametable 2
    LDA #$00
    STA $2006           ;PPUADDR

    LDA nextarea+Area::btaddr1
    STA btptr
    LDA nextarea+Area::btaddr2
    STA btptr+1
    LDA #$01
    STA colltemp1

    JSR LoadNametable

    LDA #$23
    STA $2006           ;PPUADDR      nametable1 attribute layer
    LDA #$C0
    STA $2006           ;PPUADDR
    LDA nextarea+Area::paladdr1
    STA palptr
    LDA nextarea+Area::paladdr2
    STA palptr+1
    ;LDA #$00
    ;STA bgpalette
    JSR LoadAttributes  
    LDA #$2B
    STA $2006           ;PPUADDR      nametable2 attribute layer
    LDA #$C0
    STA $2006           ;PPUADDR
    ;LDA #$00
    ;STA bgpalette
    LDA level+Area::paladdr1
    STA palptr
    LDA level+Area::paladdr2
    STA palptr+1
    JSR LoadAttributes ;currently not changing anything about the attributes
    JMP FillCollMap
FillNametablesDown:
    LDA $2002           ;PPUSTATUS    Is he reading to clear vblank flag? Neads to be changed to use NMI instead
    LDA #$20
    STA $2006           ;PPUADDR      $2000 for nametable1
    LDA #$00
    STA $2006           ;PPUADDR
    LDA level+Area::btaddr1
    STA btptr
    LDA level+Area::btaddr2
    STA btptr+1
    LDA #$00
    STA colltemp1       ;set tempvar with flag for "level" struct tileset
    JSR LoadNametable
    LDA #$28
    STA $2006           ;PPUADDR      $2800 for nametable 2
    LDA #$00
    STA $2006           ;PPUADDR

    LDA nextarea+Area::btaddr1
    STA btptr
    LDA nextarea+Area::btaddr2
    STA btptr+1
    LDA #$01            ;tileset flag
    STA colltemp1

    JSR LoadNametable

    LDA #$23
    STA $2006           ;PPUADDR      nametable1 attribute layer
    LDA #$C0
    STA $2006           ;PPUADDR
    LDA level+Area::paladdr1
    STA palptr
    LDA level+Area::paladdr2
    STA palptr+1
    JSR LoadAttributes  
    LDA nextarea+Area::paladdr1
    STA palptr
    LDA nextarea+Area::paladdr2
    STA palptr+1
    LDA #$2B
    STA $2006           ;PPUADDR      nametable1 attribute layer
    LDA #$C0
    STA $2006           ;PPUADDR
    LDA #$00
    STA bgpalette
    JSR LoadAttributes ;currently not changing anything about the attributes

FillCollMap:
    LDA nextarea+Area::cmaddr1
    STA cmptr
    LDA nextarea+Area::cmaddr2
    STA cmptr+1
    JSR LoadCollMap
    LDX #$0C        ;entry after player data
ClearEntitiesLoop:
    LDA ENTITIES, x
    CMP #$FF
    BEQ EndClearEntitiesLoop
    LDA #$00
    STA ENTITIES, x
    INX
    JMP ClearEntitiesLoop
EndClearEntitiesLoop:
    LDX #$0C
    LDA #$FF
    STA ENTITIES, x

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

;;;------------------------Draw Text ---------------------------------------;;;

DrawText:   ;my my, can't use sprites. Gotta use BG Tiles. Write to specific addresses in $2000
;TODO: I should move this back out of VBLANK later too
    PHA
    TXA
    PHA
    TYA
    PHA

    LDX #$00    ;bank iterator
GetLineFeatures:
    LDA $2002
    LDA TEXTBANK, x
    CLC
    ADC #$20
    STA $2006
    INX
    LDA TEXTBANK, x
    STA $2006
    INX
DrawTextLoop:
    LDA TEXTBANK, x     ;sprite address
    CMP #$FE
    BEQ NewLine
    STA $2007
    INX
    JMP DrawTextLoop
NewLine:
    INX
    LDA TEXTBANK, x
    CMP #$FF
    BNE GetLineFeatures    ;As game text becomes more robust, I need to add a branch here for the next "set" of text instead of ending.
DoneDrawingText:
    PLA
    TAY
    PLA
    TAX
    PLA
    RTS















;-------------------------------------------------------------------------------------------------;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    NMI / VBLANK    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-------------------------------------------------------------------------------------------------;
VBLANK:
    PHA ;push registers - A, P, X, Y
    PHP
    TXA
    PHA
    TYA
    PHA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


    LDA gamestate
    CMP #$08        ;is lava flowing?
    BNE NoLava
    LDA $2002
    LDA lavaaddress
    STA $2006
    LDA lavaaddress+1
    STA $2006
    LDX #$00
    ;LDY #$00
AddLavaToNametable:
    LDA tilebufferA, x
    STA $2007
    INX
    TXA
    CMP #$20
    BNE AddLavaToNametable

NoLava:
;begin populating the OAM data in memory
    LDX #$00        ;x will index mem locations of entity data (getter)
    LDA #$00        ;low byte of graphics page $0200
    LDY #$00        ;y will index the OAM memory location (putter)
    STA spritemem
    LDA #$02        ;high byte of graphics page $0200
    STA spritemem+1

;TODO: Damn, to save cycles do I need to alter my ENTITIES structure to have y, spr, pal, x?
; Even better, shift the metatile loading entirely out of vblank.
; At the end of the gameloop updates, write OAM data in order to a new RAM area
; Read sequentially from RAM so no calc done in vblank.
DRAWENTITIES:               ;Load info into a Metatile struct then draw
    LDX #$00
BeginLoadEntityLoop: 
    LDA ENTITIES, x      ;check if there are no sprites at all
    CMP #$FF
    BNE LoadEntityLoop
    JMP DONESPRITE
LoadEntityLoop:
    LDA ENTITIES, x     ;type
    CMP #$00            ;type nothing
    BEQ AdvanceToNextEntity
    JMP ContinueCurrentEntity
AdvanceToNextEntity:
    TXA
    CLC
    ADC #$0C
    TAX
    JMP BeginLoadEntityLoop
ContinueCurrentEntity:
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
    LDA ENTITIES, x     ;palette
    STA metatile+Metatile::pal
;What conditional for alternating frames?
;Frame will be set before draw routine and sent in
LoadSprite:
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
EndLoadSprite:
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
    LDA metatile+Metatile::pal ; palette etc
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
    LDA metatile+Metatile::pal   ;palette
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
    LDA metatile+Metatile::pal     ;palette %01000001   palette 1, flip horizontal
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
    LDA metatile+Metatile::pal   ;palette with h-flip
    STA (spritemem), y
    INY
    LDA metatile+Metatile::xpos
    CLC
    ADC #$08
    STA (spritemem), y
    INY
    JMP ClearMetatile

;TODO: Replace this whole repetitive function with a simple rewrite of the palette variable
DRAWMETATILEHRZMIRROR: ;in--struct of meta tile data
;swap tiles and set horizontal flip
    LDA metatile+Metatile::ypos ; y
    STA (spritemem), y
    INY
    LDA metatile+Metatile::spritetr ; tile
    STA (spritemem), y
    INY
    LDA metatile+Metatile::pal ; palette etc
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
    LDA metatile+Metatile::pal   ;palette
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
    LDA metatile+Metatile::pal     ;palette %01000001   palette 1, flip horizontal
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
    LDA metatile+Metatile::pal   ;palette with h-flip
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
    LDA gamestate
    CMP #$01
    BNE DoneDrawHearts
    LDA playerdata+Entity::health
    CMP #$00
    BEQ DoneDrawHearts
DrawUI:
    LDX #$00
BeginDrawUILoop:
    LDA UI, x
    CMP #$FF
    BEQ EndDrawUI
    STA (spritemem), y
    INY
    INX
    JMP BeginDrawUILoop
EndDrawUI:
DrawHearts:             ;$2F @ (D8, 16)
    LDX #$D4
    LDA #$00
    STA counter
DrawHeartsLoop:     ;y, spr, pal, x
    LDA #$10
    STA (spritemem), y
    INY
    LDA #$2F
    STA (spritemem), y
    INY
    LDA #$02
    STA (spritemem), y
    INY
    TXA
    STA (spritemem), y
    INY
    CLC
    ADC #$0A            ;increment the x position
    TAX
    INC counter         ;increment the counter
    LDA counter
    CMP playerdata+Entity::health
    BMI DrawHeartsLoop  ;less than or equal to health, keep drawing
DoneDrawHearts:
;DMA copy sprites
    LDA #$00
    STA $2003 ;reset counter
    LDA #$02    ;set memory to 0x200 range
    STA $4014   ;OAMDMA byte - This action shoves everything we wrote to $0200 with the registerss into the PPU via OAMDMA
    NOP         ;pause for sync
    NOP
    NOP
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










;Yes, this is the product of procrastination. But also, it makes this section much easier to find in the right side nav bar on vscode.

;-------------------------------------------------------------------------------------------------------------------------;
;--------------------RRRRRRR----------OOOO---------MM----------MM--------------------------------------------------------;
;--------------------RR----RR-------OO----OO-------MM-MM----MM-MM--------------------------------------------------------;
;--------------------RR---RR--------OO----OO-------MM--MM--MM--MM--------------------------------------------------------;
;--------------------RRRR-----------OO----OO-------MM---MMMM---MM--------------------------------------------------------;
;--------------------RR--RR---------OO----OO-------MM----MM----MM--------------------------------------------------------;
;--------------------RR----RR-------OO----OO-------MM----------MM--------------------------------------------------------;
;--------------------RR-----RR--------OOOO---------MM----------MM--------------------------------------------------------;
;-------------------------------------------------------------------------------------------------------------------------;


PALETTE:  ;seems like background can only access last 4 palettes?   32b
    ;sprite palettes     
    .byte $0E, $17, $1C, $37 ;palette 3  ;browns and blue - main char
    .byte $0E, $1C, $2B, $39 ;palette 2  ;pastel green, blue-green, blue
    .byte $0E, $26, $30, $05 ;palette 3  ;green, crimson, pink, white
    .byte $0E, $13, $23, $33 ;palette 4   ;purples 

    ;bg palettes
    .byte $0F, $06, $15, $18 ;palette 1  ;browns, golds, reds, bg1
    .byte $0E, $27, $2A, $07 ;palette 2   
    .byte $0E, $2C, $00, $20 ;palette 3  ;black, blue, gray, white (cloud)  
    .byte $0E, $2C, $17, $3A ;palette 4  ;black, blue, green, brown (exterior)   
    
    ;TODO: potential for compressing again by half:
    ;If I have a row-ender, I don't need a Y value, so II can pack two blocks into one byte: X1{0000}X2{0000}

;--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*
;--*--*--*--*--*--*--*--*--Title Screen ROM--*--*--*--*--*--*--*--*--*--*--*
;--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*
;Replace with rad image.
;Replace text with a draw text subroutine
;$2007 is written to sequentially, $2000-$23C0, then $2800-2BC0, then mirrored to the other two quadrants.
TITLESCREEN:    ;3 lines of text   50b
    .byte $01, $08, $ED, $E1, $DE, $D0, $E5, $DA, $EC, $ED, $D0, $E9, $E2, $DE, $DC, $DE, $FE ;Sector (0-3),Offset (00-FF, or BF),text, EOL
    .byte $02, $48, $DB, $F2, $D0, $E6, $E2, $ED, $EC, $EE, $DD, $DA, $D4, $E9, $EB, $DE, $EC, $FE
    .byte $02, $8A, $E9, $EB, $DE, $EC, $EC, $D0, $EC, $ED, $DA, $EB, $ED, $FE, $FF           ;EOL, EOF

GAMEOVERSCREEN:    ;3 lines of text   50b
    .byte $01, $0C, $E0, $DA, $E6, $DE, $D0, $E8, $EF, $DE, $EB, $FE                 ;Sector (0-3),Offset (00-FF, or BF),text, EOL
    .byte $01, $8B, $E9, $EB, $DE, $EC, $EC, $D0, $EC, $ED, $DA, $EB, $ED, $FE, $FF           ;EOL, EOF



VICTORYSCREEN:    ;
    ;AT LAST YOU HAVE IT
    .byte $01, $07, $DA, $ED, $D0, $E5, $DA, $EC, $ED, $D0, $F2, $E8, $EE, $D0, $E1, $DA, $EF, $DE, $D0, $E2, $ED, $FE     ;Sector (0-3),Offset (00-FF, or BF),text, EOL
    .byte $01, $6E, $80, $81, $82, $83, $FE           ;EOL, EOF
    .byte $01, $8E, $90, $91, $92, $93, $FE

    .byte $01, $AE, $84, $85, $86, $87, $FE
    .byte $01, $CE, $94, $95, $96, $97, $FE           ;EOL, EOF

    ;THE FABLED MEDALLION
    .byte $02, $C7, $ED, $E1, $DE, $D0, $DF, $DA, $DB, $E5, $DE, $DD, $D0, $E6, $DE, $DD, $DA, $E5, $E5, $E2, $E8, $E7, $FE           ;EOL, EOF
    ;WILL MAKE YOU A LEGEND
    .byte $03, $06, $F0, $E2, $E5, $E5, $D0, $E6, $DA, $E4, $DE, $D0, $F2, $E8, $EE, $D0, $DA, $D0, $E5, $DE, $E0, $DE, $E7, $DD, $FE, $FF          ;EOL, EOF

;--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*
;--*--*--*--*--*--*--*--*--Game areas ROM--*--*--*--*--*--*--*--*--*--*--*
;--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*
;If further compression is needed down the road:
;   -Add PALMAP toggle signal. Several of these stages only use one palette, so there's no need to burn 64b
;       in that instance. If it is replaced with a hex combo that couldn't be a palette option, or perhaps a signal
        ;somewhere else that tells the nametable routine that there is just one palette choice for this stage.
;If more collision types are needed:
;   -I'll have to double to a 4 bit collision map process. I think 3 bit would be too onerous to program decrompression for and would
;   waste lots of cycles.
AREA0:  ;233 bytes, How to traverse these memory chunks. Start with a little "table of contents"? A few bytes to load as an adder to each segment?
SELFID0:
    .byte $00
EXITS0:          ;$FF is no exit
    .byte $04   ;up
    .byte $02   ;down   Exits have to be a multiple of 2
    .byte $FF   ;left
    .byte $FF   ;right
TILESET0:
    .word BGTILES0   ;BGTILES0
CONTENTS0:
    .byte $0B   ;offset to entities
    .byte $10   ;offset to collmap
    .byte $4C     ;offset to palette map
    .byte $8C   ;offset to block table
ENTS0:
    .byte $84, $04       ;snake at (8, 4)
    .byte $52, $00    ;DEBUG: the last piece @ (15,14)    
    ;.byte $2D, $04
    .byte $FF           ;end of list
COLLMAP0: ;will reduce to 60 bytes
    .byte %01011001, %01010101, %01111111, %11111101
    .byte %01001000, %00000000, %00000000, %00000001
    .byte %01001000, %00000000, %00000000, %00000001
    .byte %01010101, %01010000, %00000000, %00000001
    .byte %01000000, %00000000, %00000000, %00000001
    .byte %01000000, %00111111, %11110000, %00000001
    .byte %01000000, %00000000, %00000000, %10000001
    .byte %01000000, %00000000, %00000000, %10000001
    .byte %01000000, %00000000, %00000101, %10111101
    .byte %01000000, %00000000, %00000000, %10000001
    .byte %01000000, %00000000, %00000000, %10000001
    .byte %01000000, %00000000, %00000000, %10000001
    .byte %01000000, %00000000, %00000000, %10000001
    .byte %01000000, %00000000, %00000000, %10000001
    .byte %01010101, %01010101, %01010101, %10010101
PALMAP0:
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
BLOCKTABLE0: ;These blocks are all in mem location 0 and will never flip, so all they need is position X{0000}  --Y{0000}-- Actually they don't need Y
    .byte $00, $10, $27, $30, $40, $50, $60, $70, $80, $91, $A1, $B1, $C1, $D1, $E1, $F0, $FF   ;right bit = tile id
    .byte $00,      $27,                                                             $F0, $FF   ;0 = block
    .byte $00,      $27,                                                             $F0, $FF   ;7 = rope
    .byte $00, $10, $20, $30, $40, $50,                                              $F0, $FF   ;8 = vine
    .byte $00,                                                                       $F0, $FF
    .byte $00,                     $51, $61, $71, $81, $91  ,                          $F0, $FF
    .byte $00,                                                        $C8,           $F0, $FF
    .byte $00,                                                        $C8,           $F0, $FF
    .byte $00,                                               $A0, $B0,$C8, $D1, $E1, $F0, $FF                              
    .byte $00,                                                        $C8,           $F0, $FF
    .byte $00,                                                        $C8,           $F0, $FF
    .byte $00,                                                        $C8,           $F0, $FF
    .byte $00,                                                        $C8,           $F0, $FF
    .byte $00,                                                        $C8,           $F0, $FF
    .byte $00, $10, $20, $30, $40, $50, $60, $70, $80, $90, $A0, $B0, $C8, $D0, $E0, $F0, $FF

AREA1:  ;224 bytes
SELFID1:
    .byte $02   ;RAM lookup table place number. By 2s for storing two-byte PRGROM addresses
EXITS1:
    .byte $00   ;up
    .byte $06   ;down
    .byte $FF   ;left
    .byte $FF   ;right
TILESET1:
    .word BGTILES0   ;BGTILES0
CONTENTS1:
    .byte $0B   ;offset to entities
    .byte $0E   ;offset to collmap
    .byte $4A   ;offset to palette map
    .byte $8A   ;offset to block table
ENTS1:
    .byte $7A, $04       ;snake at (9, A)
    .byte $FF        ;end of list
;2-bit collision map: 00 - nothing, 01 - solid, 10 - climbable, 11 - damaging.
COLLMAP1:   ;60 bytes
    .byte %01010000, %01010101, %01010101, %10010101
    .byte %01000000, %00000000, %00000000, %10000001
    .byte %01000000, %00000000, %00000000, %10000001
    .byte %01000000, %00000000, %00000000, %10000001
    .byte %01000000, %00000000, %00000000, %10000001
    .byte %01000000, %00000000, %00000000, %00000001
    .byte %01000000, %00000000, %00000000, %00000001
    .byte %01000000, %00000000, %00000001, %01010001
    .byte %01000000, %00000000, %00000101, %00000001
    .byte %01000000, %00000000, %00000000, %00000001
    .byte %01000000, %00000000, %00000000, %00000001
    .byte %01000000, %00000001, %01010101, %00000001
    .byte %01000010, %00000000, %00000000, %00000001
    .byte %01000010, %00000000, %00000000, %00000001
    .byte %01010110, %01010101, %01010101, %01010101
PALMAP1:
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
BLOCKTABLE1:    ;xxxxiiii - x = xpos on row, i = id pointer (see BGTILES0 table, etc)
    .byte $00, $10, $20, $30, $40, $50, $60, $70, $80, $90, $A0, $B0, $C8, $D0, $E0, $F0, $FF
    .byte $00,                                                        $C8,           $F0, $FF
    .byte $00,                                                        $C8,           $F0, $FF
    .byte $00,                                                        $C8,           $F0, $FF
    .byte $00,                                                        $C8,           $F0, $FF
    .byte $00,                                                                       $F0, $FF
    .byte $00,                                                                       $F0, $FF
    .byte $00,                                                   $B0, $C0, $D0,      $F0, $FF
    .byte $00,                                              $A0, $B0,                $F0, $FF                              
    .byte $00,                                                                       $F0, $FF
    .byte $00,                                                                       $F0, $FF
    .byte $00,                               $70, $80, $90, $A0, $B0,                $F0, $FF
    .byte $00,           $38,                                                        $F0, $FF
    .byte $00,           $38,                                                        $F0, $FF
    .byte $00, $10, $20, $38, $40, $50, $60, $70, $80, $90, $A0, $B0, $C0, $D0, $E0, $F0, $FF

AREA2:  ;288 bytes
SELFID2:
    .byte $04   ;RAM lookup table place number. By 2s for storing two-byte PRGROM addresses
EXITS2:
    .byte $FF   ;up
    .byte $00   ;down
    .byte $FF   ;left
    .byte $FF   ;right
TILESET2:
    .word BGTILES1   ;BGTILES0
CONTENTS2:
    .byte $0B   ;offset to entities
    .byte $0C   ;offset to collmap
    .byte $48   ;offset to palette map
    .byte $88   ;offset to block table
ENTS2:
    .byte $FF     ;end of list
;2-bit collision map: 00 - nothing, 01 - solid, 10 - climbable, 11 - damaging.
COLLMAP2:   ;60 bytes
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00001000, %00000000, %00000000, %00000000
    .byte %01011001, %01010101, %01010101, %01010101
PALMAP2:
    .byte %11111111, %11111111, %11111111, %11111111
    .byte %11111111, %11111111, %11111111, %11111111
    .byte %11111111, %11111010, %11111111, %10101111
    .byte %11111111, %11111011, %11111110, %11111111
    .byte %11111111, %11111111, %11111111, %11111111
    .byte %11111111, %11111111, %11111111, %11111010
    .byte %11111111, %11111111, %11111111, %11111111
    .byte %11111111, %11111111, %11111111, %11111111
    .byte %11111111, %11111111, %11111111, %11111111
    .byte %11111111, %11111111, %11111111, %11111111
    .byte %11111111, %11111111, %11111111, %11111111
    .byte %11111111, %11111111, %11111111, %11111111
    .byte %11111111, %11111111, %11111111, %11111111
    .byte %11111111, %11111111, %11111111, %11111111
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
BLOCKTABLE2:    ;xxxxiiii - x = xpos on row, i = id pointer (see BGTILES0 table, etc)
    .byte                                                                                 $FF   ;1
    .byte                                                                                 $FF   ;1
    .byte $07, $1A, $22, $33,                                    $B2, $C3,                $FF   ;7
    .byte $05, $17, $2A,                $62, $73,                                         $FF   ;6
    .byte $08, $15, $27, $3A,                                                   $E2, $F3, $FF   ;7
    .byte $05, $18, $25, $39, $4D, $5A,                                                   $FF   ;7
    .byte $05, $15, $28, $35, $45, $57, $6A,                                              $FF   ;8
    .byte $05, $15, $25, $35, $45, $55, $67, $7A,                                         $FF   ;9
    .byte $05, $15, $25, $35, $45, $55, $65, $77, $8A,                                    $FF   ;10
    .byte $05, $15, $25, $35, $45, $55, $65, $75, $87, $9B, $AC, $BD, $CA,                $FF   ;14
    .byte $05, $18, $25, $35, $45, $55, $65, $75, $85, $98, $A5, $B5, $C7, $DA,           $FF   ;15
    .byte $05, $15, $28, $35, $48, $55, $65, $75, $85, $95, $A8, $B5, $C5, $D7, $EA,      $FF   ;16
    .byte $05, $15, $25, $35, $45, $58, $65, $75, $85, $95, $A5, $B8, $C5, $D5, $E7, $FA, $FF   ;17
    .byte $05, $15, $21, $35, $45, $55, $65, $75, $85, $95, $A5, $B5, $C8, $D5, $E5, $F7, $FF   ;17
    .byte $06, $16, $20, $36, $46, $56, $66, $76, $86, $96, $A6, $B6, $C6, $D6, $E6, $F6, $FF   ;17

AREA3:  ;237 bytes
SELFID3:
    .byte $06   ;RAM lookup table place number. By 2s for storing two-byte PRGROM addresses
EXITS3:
    .byte $02   ;up
    .byte $08   ;down
    .byte $FF   ;left
    .byte $FF   ;right
TILESET3:
    .word BGTILES0   ;BGTILES0
CONTENTS3:
    .byte $0B   ;offset to entities
    .byte $0E   ;offset to collmap
    .byte $4A   ;offset to palette map
    .byte $8A   ;offset to block table
ENTS3:
    .byte $98, $04       ;snake at (9, 8)
    .byte $FF        ;end of list
;2-bit collision map: 00 - nothing, 01 - solid, 10 - climbable, 11 - damaging.
COLLMAP3:   ;60 bytes
    .byte %01010110, %01010101, %01010101, %01010101
    .byte %01000010, %00000000, %00000000, %00000001
    .byte %01000001, %01000000, %00000000, %00000001
    .byte %01000010, %01000000, %00000000, %00000001
    .byte %01000010, %01000000, %00000000, %00000001
    .byte %01010010, %01010101, %00000000, %00000001
    .byte %01000010, %00000000, %00000000, %00000001
    .byte %01000010, %00000000, %00000000, %00000001
    .byte %01000010, %00000000, %00000000, %00000001
    .byte %01000000, %00000000, %00010101, %00000001
    .byte %01000000, %00000000, %00000000, %00010101
    .byte %01000100, %00010001, %00000000, %00011001
    .byte %01000000, %00000000, %00000000, %00001001
    .byte %01000000, %00000000, %00000000, %00001001
    .byte %01010101, %01010101, %01010101, %01011001
PALMAP3:
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
BLOCKTABLE3:    ;xxxxiiii - x = xpos on row, i = id pointer (see BGTILES0 table, etc)
    .byte $00, $10, $20, $38, $40, $50, $60, $70, $80, $90, $A0, $B0, $C0, $D0, $E0, $F0, $FF
    .byte $00,           $38,                                                        $F0, $FF
    .byte $00,           $30, $40,                                                   $F0, $FF
    .byte $00,           $38, $40,                                                   $F0, $FF
    .byte $00,           $38, $40,                                                   $F0, $FF
    .byte $00, $10,      $38, $40, $50, $60, $70,                                    $F0, $FF
    .byte $00,           $38,                                                        $F0, $FF
    .byte $00,           $38,                                                        $F0, $FF
    .byte $00,           $38,                                                        $F0, $FF                              
    .byte $00,                                         $90, $A0, $B0,                $F0, $FF
    .byte $00,                                                             $D0, $E0, $F0, $FF
    .byte $00,      $20,           $50,      $70,                          $D0, $E8, $F0, $FF
    .byte $00,                                                                  $E8, $F0, $FF
    .byte $00,                                                                  $E8, $F0, $FF
    .byte $00, $10, $20, $30, $40, $50, $60, $70, $80, $90, $A0, $B0, $C0, $D0, $E8, $F0, $FF

AREA4:
SELFID4:
    .byte $08   ;RAM lookup table place number. By 2s for storing two-byte PRGROM addresses
EXITS4:
    .byte $06   ;up
    .byte $0A   ;down
    .byte $FF   ;left
    .byte $FF   ;right
TILESET4:
    .word BGTILES0   ;BGTILES0
CONTENTS4:
    .byte $0B   ;offset to entities
    .byte $0C   ;offset to collmap
    .byte $48   ;offset to palette map
    .byte $88   ;offset to block table
ENTS4:
    ;.byte $ED, $00    ;the last piece @ (15,14)
    .byte $FF        ;end of list
;2-bit collision map: 00 - nothing, 01 - solid, 10 - climbable, 11 - damaging.
COLLMAP4:   ;60 bytes
    .byte %01010101, %01010101, %01010101, %01011001
    .byte %01000000, %00000000, %00000000, %00001001
    .byte %01000000, %00000000, %00000000, %00001001
    .byte %01000000, %00000000, %10000000, %00001001
    .byte %01000000, %00001000, %00001010, %10101001
    .byte %01000000, %00100000, %00001000, %00000001
    .byte %01100000, %00101000, %00001010, %00000001
    .byte %01100000, %00001000, %00000010, %10000001
    .byte %01100000, %00001010, %00000000, %10000001
    .byte %01100000, %00000000, %00000010, %10000001
    .byte %01000000, %00000010, %00000010, %00000001
    .byte %01000000, %00000000, %00000000, %00000001
    .byte %01000000, %00001010, %00000000, %00000001
    .byte %01000000, %00001000, %00000000, %00000001
    .byte %01010101, %01011001, %01010101, %01010101
PALMAP4:
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
BLOCKTABLE4:    ;xxxxiiii - x = xpos on row, i = id pointer (see BGTILES0 table, etc)
    .byte $00, $10, $20, $30, $40, $50, $60, $70, $80, $90, $A0, $B0, $C0, $D0, $E8, $F0, $FF
    .byte $00,                                                                  $E8, $F0, $FF
    .byte $00,                                                                  $E8, $F0, $FF
    .byte $00,                                    $88,                          $E8, $F0, $FF
    .byte $00,                          $68,                $A8, $B8, $C8, $D8, $E8, $F0, $FF
    .byte $00,                     $58,                     $A8,                     $F0, $FF
    .byte $00, $18,                $58, $68,                $A8, $B8,                $F0, $FF
    .byte $00, $18,                     $68,                     $B8, $C8,           $F0, $FF
    .byte $00, $18,                     $68, $78,                     $C8,           $F0, $FF
    .byte $00, $18,                                              $B8, $C8,           $F0, $FF
    .byte $00,                               $78,                $B8,                $F0, $FF
    .byte $00,                                                                       $F0, $FF
    .byte $00,                          $68, $78,                                    $F0, $FF
    .byte $00,                          $68,                                         $F0, $FF
    .byte $00, $10, $20, $30, $40, $50, $68, $70, $80, $90, $A0, $B0, $C0, $D0, $E0, $F0, $FF

AREA5:
SELFID5:
    .byte $0A   ;RAM lookup table place number. By 2s for storing two-byte PRGROM addresses
EXITS5:
    .byte $08   ;up
    .byte $FF   ;down
    .byte $FF   ;left
    .byte $FF   ;right
TILESET5:
    .word BGTILES0   ;BGTILES0
CONTENTS5:
    .byte $0B   ;offset to entities
    .byte $10   ;offset to collmap
    .byte $4C   ;offset to palette map
    .byte $8C   ;offset to block table
ENTS5:
    .byte $85, $04       ;snake at (8, 4)
    .byte $CD, $00    ;the last piece @ (15,14)
    .byte $FF        ;end of list
;2-bit collision map: 00 - nothing, 01 - solid, 10 - climbable, 11 - damaging.
COLLMAP5:   ;60 bytes
    .byte %01010101, %01011001, %01010101, %01010101
    .byte %01000000, %00001001, %00000000, %00000001
    .byte %01000000, %00000001, %00000000, %00000001
    .byte %01000000, %00000101, %00000001, %01000001
    .byte %01000000, %01010100, %00000101, %10000001
    .byte %01000001, %01000000, %00000100, %10000001
    .byte %01000101, %00000000, %01010100, %10000101
    .byte %01000001, %00000001, %01000000, %10000001
    .byte %01000000, %00000101, %00000000, %10000001
    .byte %01010000, %00010100, %00000000, %10000001
    .byte %01010101, %01010000, %00000000, %10000001
    .byte %01000000, %00000000, %00000001, %01010001
    .byte %01000000, %00000000, %00000101, %00010101
    .byte %01000000, %01010100, %00000000, %00000101
    .byte %01010101, %01010101, %01010101, %01010101
PALMAP5:
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
BLOCKTABLE5:    ;xxxxiiii - x = xpos on row, i = id pointer (see BGTILES0 table, etc)
    .byte $00, $10, $20, $30, $40, $50, $68, $70, $80, $90, $A0, $B0, $C0, $D0, $E0, $F0, $FF
    .byte $00,                          $68, $70,                                    $F0, $FF
    .byte $00,                               $70,                                    $F0, $FF
    .byte $00,                          $60, $70,                $B0, $C0,           $F0, $FF
    .byte $00,                $40, $50, $60,                $A0, $B0, $C8,           $F0, $FF
    .byte $00,           $30, $40,                          $A0,      $C8,           $F0, $FF
    .byte $00,      $20, $30,                     $80, $90, $A0,      $C8,      $E0, $F0, $FF
    .byte $00,           $30,                $70, $80,                $C8,           $F0, $FF
    .byte $00,                          $60, $70,                     $C8,           $F0, $FF
    .byte $00, $10,                $50, $60,                          $C8,           $F0, $FF
    .byte $00, $10, $20, $30, $40, $50,                               $C8,           $F0, $FF
    .byte $00,                                                   $B0, $C0, $D0,      $F0, $FF
    .byte $00,                                              $A0, $B0,      $D0, $E0, $F0, $FF
    .byte $00,                $40, $50, $60,                                    $E0, $F0, $FF
    .byte $00, $10, $20, $30, $40, $50, $60, $70, $80, $90, $A0, $B0, $C0, $D0, $E0, $F0, $FF


ENTPOINTERS: ;load with offset from AREA data. Offset runs by 2s for word size.
            ;this shold work essentially the same as AREABANK, but in ROM not RAM.
    .word LASTPIECE ;00
    .word HAT       ;02
    .word SNAKE     ;04

LASTPIECE:
    .byte %00100000             ;type/env - 2 (treasure) | no env factor
    .byte $01                   ;palette
    .byte $01                   ;number of frames
    .byte $26, $27, $36, $37    ;sprite1 - tl, tr, bl, br
HAT:
    .byte %00100000
    .byte $00                   ;palette
    .byte $01                   ;number of frames
    .byte $0E, $0F, $1E, $1F
SNAKE:
    .byte %00110000
    .byte $01                   ;palette
    .byte $02                   ;number of frames
    .byte $F0, $41, $50, $51    ;sprite1
    .byte $F0, $40, $52, $42    ;sprite2


;Second bit of BLOCKTABLE entry will point here
BGTILES0:                       ;background tiles for world 0 (16 available) - tl, tr, bl, br
    .byte $00, $01, $10, $11    ;00     block wall
    .byte $00, $01, $20, $21    ;01     stalactite
    .byte $00, $00, $00, $00    ;02
    .byte $00, $00, $00, $00    ;03
    .byte $00, $00, $00, $00    ;04
    .byte $00, $00, $00, $00    ;05
    .byte $00, $00, $00, $00    ;06
    .byte $06, $07, $16, $17    ;07     rope
    .byte $04, $05, $14, $15    ;08     vine
    .byte $00, $00, $00, $00    ;09
    .byte $00, $00, $00, $00    ;0A
    .byte $00, $00, $00, $00    ;0B
    .byte $00, $00, $00, $00    ;0C
    .byte $00, $00, $00, $00    ;0D
    .byte $00, $00, $00, $00    ;0E
    .byte $00, $00, $00, $00    ;0F
    .byte $02, $02, $12, $13    ;10     canvas tile

BGTILES1:                       ;background tiles for world 1 (extreior) (16 available) - tl, tr, bl, br
    .byte $06, $07, $16, $17    ;00     rope
    .byte $0C, $0D, $D0, $D0    ;01     door
    .byte $08, $09, $18, $19    ;02     cloud1
    .byte $0A, $0B, $1A, $1B    ;03     cloud2
    .byte $1D, $1D, $1D, $1D    ;04     sky
    .byte $1C, $1C, $1C, $1C    ;05     mountain face (solid yellow)
    .byte $28, $29, $38, $39    ;06     ground
    .byte $0E, $0F, $1C, $1F    ;07     M1 - right slope
    .byte $2B, $2C, $1C, $3C    ;08     M2 - interior ridge
    .byte $1F, $1E, $1C, $1C    ;09     M3 - plateau
    .byte $1D, $1D, $0F, $1D    ;0A     M4 - right slope corner filler
    .byte $1D, $3A, $3A, $3B    ;0B     M5 - left slope
    .byte $2A, $1E, $1C, $1C    ;0C     M6 - peak
    .byte $0F, $1D, $1F, $0F    ;0D     M7 - right low slope
    .byte $00, $00, $00, $00    ;0E
    .byte $00, $00, $00, $00    ;0F
    .byte $1D, $1D, $1D, $1D    ;10     canvas tile - sky

.segment "VECTORS"
    .word VBLANK
    .word RESET
    .word 0

.segment "CHARS"
    .incbin "sprites.chr"