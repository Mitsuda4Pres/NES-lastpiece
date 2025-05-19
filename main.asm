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

;Entity management fields. I can shrink these when I find my hard cap for max entities
;We're definitely not getting 64 bytes so I may as well shrink to 32
;Define 64B data arrays in $0400 and $0500 block,
XPOS            = $0400 ;fill with $FF
YPOS            = $0420
TYPE            = $0500 ;fill with $00
DATA            = $0520
HEALTH          = $0540


;Now eschewing the OOP approach, entity type, along with x/y/other data, will each get it's own array, and we'll see how big
;we can make it.
.scope EntityType       ;similar to an enum
    NoEntity = 0
    PlayerType = 1
    Bullet = 2
    Enemy = 3
.endscope

;.struct Entity
;    xpos .byte
;    ypos .byte
;    type .byte
;    data .byte ;entity specific information
;.endstruct

.segment "STARTUP"

;I had to open of the zero page in nes.cfg to range from $0002 to $00FF. Idk if that will break something later.
.segment "ZEROPAGE"
controller:     .res 1    ;reserve 1 byte for controller input
drawcomplete:   .res 1
objcount:       .res 1
tempvar:        .res 1
collisiontmp:   .res 1     ;used in collision routine to store extra value
playerspr:      .res 1
xhold:          .res 1      ;vars to hold position values during collision detection
yhold:          .res 1      ;I could potentially just keep a handful of generic vars on zero page and reuse them
scrollx:        .res 1
scrolly:        .res 1
nametableswap:  .res 1
swaptoggle:     .res 1
buttonflag:     .res 1
spritemem:      .res 2 
ptr:            .res 2

    


MAXENTITIES = 18            ;We don't have any entity types defined yet, so I'm going to skip some of the tutorial lines
;entities:   .res .sizeof(Entity) * MAXENTITIES  ;Here I can reserve space for commonly used game objects
;TOTALENTITIES = .sizeof(Entity) * MAXENTITIES   ;He does the sizeof(Entity) * MAXENTITIES to make sure he always has 
                                                ;enough space on zeropage to handle objects on screen quickly


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
    STA $0500, x
    STA $0600, x
    STA $0700, x
    LDA #$FF
    STA $0200, x    ;Fills $0200 with $FF, not 0.
    STA $0400, x 
    LDA #$00
    STA controller
    INX
    BNE CLEARMEM

;initialize entities + Entity::xpos
;Here is first change. Initialize player entity only.
;Almost this entire routine is rendered useless because
;All of these values are set in the array

;initialize player entity
    LDA #EntityType::PlayerType
    STA TYPE
    LDA #$80
    STA XPOS
    LDA #$78
    STA YPOS

    LDA #$00
    LDX #$00
    STA objcount

    LDA #$00
    STA scrollx
    STA nametableswap
    LDA #$EE
    STA scrolly

;looks good up to here


    ;LDA #$80
    ;STA entities+Entity::xpos
    ;LDA #$78
    ;STA entities+Entity::ypos
    ;LDA #EntityType::PlayerType
    ;STA entities+Entity::type

    ;LDX #.sizeof(Entity)
    ;LDA #$FF
;CLEARENTITIES:
    ;STA entities+Entity::xpos, x 
    ;STA entities+Entity::ypos, x
    ;LDA #$00
    ;STA entities+Entity::data, x
    ;STA entities+Entity::type, x
    ;LDA #$FF
    ;TXA
    ;CLC
    ;ADC #.sizeof(Entity)
    ;TAX
    ;CPX #TOTALENTITIES
    ;BNE CLEARENTITIES

;Clear register and set palette address
    LDA $2002
    LDA #$3F
    STA $2006
    LDA #$10
    STA $2006

; initialize background hi and low

    LDX #$00
PALETTELOAD:
    LDA PALETTE, x
    STA $2007           ;PPUDATA
    INX
    CPX #$20
    BNE PALETTELOAD

;;;;;;;;;;;;;;;;;;;;;;;;
BGLOAD:
    LDY #$00            ;use y index to cycle through bg sprite sheet

    LDA $2002           ;PPUSTATUS    Is he reading to clear vblank flag? Neads to be changed to use NMI instead
    LDA #$20
    STA $2006           ;PPUADDR      we are using $2000 for graphics memory
    LDA #$00
    STA $2006           ;PPUADDR
    STA tempvar
    STA ptr
    LDA #<NAMETABLE0
    STA ptr+0
    LDA #>NAMETABLE0
    STA ptr+1
BGLOADPREP:
    LDX #$04            ;loop eight times $FF to get 2 full kilobytes
    LDY #$00
BGLOADLOOP:
    LDA (ptr), Y   ;load data from NAMETABLE0
    STA $2007           ;PPUDATA
    INY
    BNE BGLOADLOOP         ;when Y cycles past $FF to $00, decrement X
    DEX
    BEQ LOADNEXTTABLE          ;jump to end if X hits 0
    INC ptr+1       ;increase the high byte by 1 (ptr is low byte, little endian)
    JMP BGLOADLOOP          ;loop back on X
LOADNEXTTABLE:
    LDA tempvar
    CMP #$01
    BEQ BGLOADDONE
    LDA #$28
    STA $2006
    LDA #$00
    STA $2006
    LDA #<NAMETABLE1
    STA ptr+0
    LDA #>NAMETABLE1
    STA ptr+1
    INC tempvar
    JMP BGLOADPREP
BGLOADDONE:
; configure for loading the attributes
    LDA $2002
    LDA #$23
    STA $2006
    LDA #$C0
    STA $2006
    LDX #$00
    TXA
ATTLOAD:
    LDA ATTRIBUTE0, X
    STA $2007           ;PPUDATA
    INX
    CPX #$08
    BNE ATTLOAD

    JSR WAITFORVBLANK




    LDA #%10000000
    STA $2000           ;PPUCONTROL
    LDA #%00011110
    STA $2001           ;PPUMASK

    LDA #$00
    STA spritemem
    LDA #$02
    STA spritemem+1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GAMELOOP:

resetobjcount:
    LDA #$00
    STA tempvar
    LDX #$20
findlastloop:
    DEX
    LDA TYPE, X
    CMP #EntityType::NoEntity
    BEQ findlastloop

    TXA
    STA objcount
    CLC
    ADC #$01
    STA tempvar



INITSPRITES:
    LDY #$00
    LDA #$FF
    ;set player sprite to center
    LDA #$00
    STA playerspr

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
    DEC XPOS   ;decrement x position
    LDA #02         ;After ROR check, will be $01 carry clear
    STA playerspr
    JMP checkup ; don't allow for left and right at the same time (jump past checkright if left was pressed)

checkright:
    LDA controller
    AND #$01
    BEQ checkup
    INC XPOS
    LDA #$03        ;after ROR check, will be $01 carry set
    STA playerspr
checkup:
    LDA controller
    AND #$08
    BEQ checkdown
    DEC YPOS
    JMP donecheckingdirectional ;jump past check down so not getting both simultaneous

checkdown:
    LDA controller
    AND #$04
    BEQ donecheckingdirectional
    INC YPOS

donecheckingdirectional:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
checkbuttons:

checkb:
    LDA controller
    AND #$40            ;b button bit
    BEQ checkbrelease
    LDA buttonflag
    ORA #$02            ;local b flag
    STA buttonflag
    JMP checka
checkbrelease:
    LDA buttonflag
    AND #$02
    BEQ checka
    LDA buttonflag
    EOR #$02
    STA buttonflag
    JMP addenemy

checka:
    LDA controller
    AND #$80
    BEQ checkarelease  ;Branch if Equal is a bit of a misnomer. It branches if the zero flag is set (1), so if A is no longer pressed, the
                        ;AND #$80 will produce a 0 (not true), and cause the branch. Hover opcode to see description.
    LDA buttonflag
    ORA #$01
    STA buttonflag
    JMP finishcontrols
checkarelease:
    LDA buttonflag
    AND #$01
    BEQ finishcontrols
    LDA buttonflag
    EOR #$01        ;turn off bit 1 - 1 and 1 becomes 0
    STA buttonflag
    JMP addbullet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Second area to change, entity logic                                    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
addbullet:  ;gather attributes for bullet then begin create process
    LDY #EntityType::Bullet
    LDA XPOS        ;player's x-pos
    CLC
    ADC #$04        ;center of player sprite
    STA xhold
    LDA YPOS        ;player's y-pos
    STA yhold               
    JMP addentity
addenemy:
    LDY #EntityType::Enemy
    LDA XPOS        ;player's x-pos
    STA xhold
    LDA #$08        
    STA yhold
    JMP addentity

addentity:
    INC objcount
    LDA objcount
    CMP #MAXENTITIES               ;arrays hold 64 objects, max entities
    BCS finishcontrols
    LDX #$00
findlowestindexloop:        ;potential for infinte loop? If TYPE is full, it shouldn't make it this far
    LDA TYPE, X
    CMP #EntityType::NoEntity
    BEQ createentity
    INX
    JMP findlowestindexloop
createentity:
    TYA                     ;pull new entity's type from Y
    STA TYPE, X
    LDA xhold
    STA XPOS, X
    LDA yhold
    STA YPOS, X
    JMP finishcontrols

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
finishcontrols:

;no scrolling for the moment
;processscrolling:
;    LDA scrolly
;    SEC
;    SBC #$02
;    STA scrolly
;    CMP #$00
;    BNE donescroll
;    LDA #$EE
;    STA scrolly
;    LDA swap
;    EOR #$02
;    STA swap
;donescroll:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;           Third area to alter                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

processentities:
    LDX #$01        ;no need to process "player" here, start at 1
    LDA objcount
    STA tempvar
    INC tempvar          ;compare current iteration to objcount+1 (index needs to run loop if it is the same number as objcount)
processentitiesloop:
    LDA TYPE, X     ;What type of entity is this iteration?
    CMP #EntityType::Bullet
    BEQ processbullet
    CMP #EntityType::Enemy
    BEQ processenemy
    ;This instruction should never be called. Worst case, JMP to end, nothing happens.
    JMP nextentity   ;Thought about jumping up to top of loop, but could create infinite loop theoretically       
processbullet:
    LDA YPOS, X
    SEC
    SBC #$03                ;move bullet forward 3 pixels
    ;collision detection - (check before or after store y ?), set up explosion, destroy entities, etc.
    JSR checkbulletcollision   ;going in: x is bullet index, A is new bullet y-pos

    STA YPOS, X             ;store back into ypos
    BCS nextentity ;if greater than zero (didn't use carry bit), move to next entity
    JMP clearentity         ;Otherwise it has moved off screen, clear
processenemy:
    INC DATA, x    ;counter for subpixel movement (increment to, say 3, for "1/3" pixel per frame)
    LDA DATA, x
    CMP #$03
    BNE processenemyskipyinc
    LDA #$00
    STA DATA, x
    INC YPOS, x    ;move enemy forward 1
processenemyskipyinc:
    LDA YPOS, x
    CMP #$FE
    BNE nextentity
    ;JMP clearentity
clearentity:
    LDA #EntityType::NoEntity       ;NoEntity is $00 anyway
    STA TYPE, X
    LDA #$FF
    STA YPOS, x
    STA XPOS, x
    LDA #$00
    STA DATA, X
    JMP nextentity
nextentity:
    INX
    CPX tempvar
    ;CPX objcount
    BNE processentitiesloop
doneprocessentities:
    
    ;Increment the indices?
waitfordrawtocomplete:
    LDA drawcomplete
    CMP #$01
    BNE waitfordrawtocomplete
    LDA #$00
    STA drawcomplete

    JMP GAMELOOP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;           Logical subroutines              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;in: We also need that new y-pos from A right?
;in: X contains index of current bullet, out: none.       ---modifies Y
checkbulletcollision: 
;I'm not convinced a subroutine is the least expensive option here because it only applies to the one entity.
;However I'm following the video so it will be good practice in ordering code by subroutines.
    PHA         ;Save A and X registers (new y value of bullet and bullet index respectively)
    TXA
    PHA
    PHP         ;saving P because carry flag is needed right after subroutine\

    TAY
    LDX #$00            ;X will index all other entities, looking for collidable (enemy), can skip player again by starting at 1
    LDA objcount
    STA tempvar         ;compare current iteration to objcount+1 (index needs to run loop if it is the same number as objcount)
    INC tempvar         ;borrowing ptr location          
checkbulletcollisionloop: ;Y = iterator, X = bullet index
    CPX tempvar
    BEQ finishedbulletcollision
    LDA TYPE, X
    CMP #EntityType::Enemy           ;for now simply check for "enemy". This section can become more robust as we add new objects.
    BNE checkbulletentityfinished

    ;Carry flag truth set: A < Mem = C0; A => Mem = C1
    ;Check X left of enemy
    LDA XPOS, Y                     ;get bullet's current x pos (index held in Y)
    CLC
    ADC #$05                        ;get bullet's rightmost collision xpos (point C) 
    STA collisiontmp               
    LDA XPOS, X                     ;left most position of enemy
    CMP collisiontmp
    BCS checkbulletentityfinished   ;CHECK 1: A(enemy x left) is greater than Mem(bullet x right), it's a miss.
    ;Check X right of enemy
    LDA XPOS, Y
    CLC
    ADC #$02
    STA collisiontmp
    LDA XPOS, X
    CLC
    ADC #$08                        ;x(e) becomes rightmost x-pixel of enemy
    CMP collisiontmp                ;CHECK 2: A(enemy x, right side) is less than Mem(bullet x left)
    BCC checkbulletentityfinished   ;this time, if it's less than, we know that it's a miss. Bullet is to the right of the enemy.
    ;Check Y
    LDA YPOS, Y                     ;Bullet y pos
    STA collisiontmp
    LDA YPOS, X                     ;Enemy y (add 8)
    CLC
    ADC #$08
    CMP collisiontmp                ;CHECK 3: A(enemy y bottom) vs. Mem(bullet y top). If A < Mem (BCC), it's a miss 
    BCC checkbulletentityfinished
    ;If we made it this far, we have COLLISION! Using CMP solves the signed byte problem too!!!
    ;Collision logic - for now just clear enemy and bullet by setting type/data to 0, and x/y to FF

    LDA #EntityType::NoEntity
    STA TYPE, x
    STA TYPE, y
    STA DATA, x
    STA DATA, y
    LDA #$FF
    STA XPOS, x
    STA XPOS, y
    STA YPOS, x
    STA YPOS, y
    JMP finishedbulletcollision
    ;;;;;;;;;;;;;;;;;;;;;;pause writing at video 5, 45:00. I want to see where he gets to before commiting to a collision engine
    ;;Gonna try writing it myself from here.
checkbulletentityfinished:      ;increment to next entity
    INX
    ;CPY tempvar
    ;BEQ finishedbulletcollision
    JMP checkbulletcollisionloop
finishedbulletcollision:
    PLP
    PLA
    TAX
    PLA     ;Pull X then A, reverse order as always
    RTS

VBLANK:
    PHA ;push registers - A, P, X, Y
    PHP
    TXA
    PHA
    TYA
    PHA

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;DOD: is it worth having another array for OAM data? prob not.
;Although as many more graphical options are added, maybe it is better so that one draw process can be called.
;And OAM data sent in as arguments

;begin populating the OAM data in memory
    
    LDX #$00        ;x will index mem locations of entity data (getter)
    LDA #$00        ;low byt of graphics page $0200
    LDY #$00        ;y will index the OAM memory location (putter)
    STA spritemem
    LDA #$02        ;high byte of graphics page $0200
    STA spritemem+1

DRAWENTITIES:                       ;copied his code in for now. 
    LDA TYPE, x
    CMP #EntityType::PlayerType
    BEQ PLAYERSPRITE
    CMP #EntityType::Bullet
    BEQ BULLET
    CMP #EntityType::Enemy
    BEQ ENEMY
    JMP CHECKENDSPRITE
    ;see https://www.nesdev.org/wiki/PPU_OAM
ENEMY:
    LDA YPOS, x ; y   ;byte 1 = y position
    STA (spritemem), y
    INY
    LDA #$04 ; tile
    STA (spritemem), y                  ;byte 2 = sprite from table
    INY
    LDA #$01 ; palette etc
    STA (spritemem), y                  ;byte 3 = palette and layer
    INY
    LDA XPOS, x ; x
    STA (spritemem), y                  ;byte 4 = x position
    INY
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    JMP CHECKENDSPRITE

BULLET:
    LDA YPOS, x ; y   ;byte 1 = y position
    STA (spritemem), y
    INY
    LDA #$03 ; tile
    STA (spritemem), y                  ;byte 2 = sprite from table
    INY
    LDA #$01 ; palette etc
    STA (spritemem), y                  ;byte 3 = palette and layer
    INY
    LDA XPOS, x ; x
    STA (spritemem), y                  ;byte 4 = x position
    INY
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    JMP CHECKENDSPRITE

PLAYERSPRITE:       ;;;;;;;;;;;;;CHECK!!! Check these comparisons first if buggy
    LDA playerspr
    CMP #$00
    BEQ CENTERED
    CLC
    ROR
    BCS BANKRIGHT
    JMP BANKLEFT
    ;If neither, fall through
CENTERED:
    ;top left sprite
    LDA YPOS, x ; y
    STA (spritemem), y
    INY
    LDA #$00        ;#$00 straight tile
    STA (spritemem), y
    INY
    LDA #$01 ; palette etc
    STA (spritemem), y
    INY
    LDA XPOS, x   ; x
    STA (spritemem), y
    INY
    ;bottom left sprite
    LDA YPOS, x ; y
    CLC
    ADC #$08   ;Add 8 pixels to y-pos for second sprite
    STA (spritemem), y
    INY
    LDA #$10      ;first tile location + 16, first tile of second row
    STA (spritemem), y
    INY
    LDA #$01   ;palette
    STA (spritemem), y
    INY
    LDA XPOS, x ; x position
    STA (spritemem), y
    INY
    ;top right sprite
    LDA YPOS
    STA (spritemem), y
    INY
    LDA #$00     ;same as top left but we will flip it and add 8 to xpos
    STA (spritemem), y
    INY
    LDA #$41     ;palette %01000001   palette 1, flip horizontal
    STA (spritemem), y
    INY
    LDA XPOS, x
    CLC
    ADC #$08
    STA (spritemem), y
    INY
    ;bottom right
    LDA YPOS, x ; y
    CLC
    ADC #$08   ;Add 8 pixels to y-pos for second sprite
    STA (spritemem), y
    INY
    LDA #$10   ;tile location 16, first tile of second row
    STA (spritemem), y
    INY
    LDA #$41   ;palette with h-flip
    STA (spritemem), y
    INY
    LDA XPOS, x
    CLC
    ADC #$08
    STA (spritemem), y
    INY
    JMP CHECKENDSPRITE
BANKRIGHT:
    ;top left sprite
    LDA YPOS, x ; y
    STA (spritemem), y
    INY
    LDA #$02        ;#$00 straight tile
    STA (spritemem), y
    INY
    LDA #$41 ; palette etc
    STA (spritemem), y
    INY
    LDA XPOS, x   ; x
    STA (spritemem), y
    INY
    ;bottom left sprite
    LDA YPOS, x ; y
    CLC
    ADC #$08   ;Add 8 pixels to y-pos for second sprite
    STA (spritemem), y
    INY
    LDA #$12      ;first tile location + 16, first tile of second row
    STA (spritemem), y
    INY
    LDA #$41   ;palette
    STA (spritemem), y
    INY
    LDA XPOS, x ; x position
    STA (spritemem), y
    INY
    ;top right sprite
    LDA YPOS
    STA (spritemem), y
    INY
    LDA #$01     ;same as top left but we will flip it and add 8 to xpos
    STA (spritemem), y
    INY
    LDA #$41     ;palette %01000001   palette 1, flip horizontal
    STA (spritemem), y
    INY
    LDA XPOS, x
    CLC
    ADC #$08
    STA (spritemem), y
    INY
    ;bottom right
    LDA YPOS, x ; y
    CLC
    ADC #$08   ;Add 8 pixels to y-pos for second sprite
    STA (spritemem), y
    INY
    LDA #$11   ;tile location 16, first tile of second row
    STA (spritemem), y
    INY
    LDA #$41   ;palette with h-flip
    STA (spritemem), y
    INY
    LDA XPOS, x
    CLC
    ADC #$08
    STA (spritemem), y
    INY
    JMP CHECKENDSPRITE
BANKLEFT:
    ;top left sprite
    LDA YPOS, x ; y
    STA (spritemem), y
    INY
    LDA #$01            ;first tile of banked position
    STA (spritemem), y
    INY
    LDA #$01 ; palette etc
    STA (spritemem), y
    INY
    LDA XPOS, x   ; x
    STA (spritemem), y
    INY
    ;bottom left sprite
    LDA YPOS, x ; y
    CLC
    ADC #$08   ;Add 8 pixels to y-pos for second sprite
    STA (spritemem), y
    INY
    LDA #$11      ;first tile location + 16, first tile of second row
    STA (spritemem), y
    INY
    LDA #$01   ;palette
    STA (spritemem), y
    INY
    LDA XPOS, x ; x position
    STA (spritemem), y
    INY
    ;top right sprite
    LDA YPOS
    STA (spritemem), y
    INY
    LDA #$02     ;top right tile location
    STA (spritemem), y
    INY
    LDA #$01     ;palette 
    STA (spritemem), y
    INY
    LDA XPOS, x
    CLC
    ADC #$08
    STA (spritemem), y
    INY
    ;bottom right
    LDA YPOS, x ; y
    CLC
    ADC #$08   ;Add 8 pixels to y-pos for second sprite
    STA (spritemem), y
    INY
    LDA #$12   ;tile location 16, first tile of second row
    STA (spritemem), y
    INY
    LDA #$01   ;palette 
    STA (spritemem), y
    INY
    LDA XPOS, x
    CLC
    ADC #$08
    STA (spritemem), y
    INY
    ;Fall through
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CHECKENDSPRITE:
    INX
    LDA objcount
    STA tempvar
    INC tempvar          ;compare current iteration to objcount+1 (index needs to run loop if it is the same number as objcount)
    CPX tempvar
    ;CPX objcount
    BEQ DONESPRITE
    JMP DRAWENTITIES

DONESPRITE:
    LDA #$00
    STA $2003 ;reset counter
    LDA #$02    ;set memory to 0x200 range
    STA $4014   ;OAMDMA byte - This action shoves everything we wrote to $0200 with the registerss into the PPU via OAMDMA
    NOP         ;pause for sync

    
    LDA #$00    ;clear register
    STA $2006
    STA $2006   ;$2006 takes a double write PPUDATA

    LDA #%10001000
    ORA nametableswap
    LDX $2002   ;clear the register before
    STA $2000

    ;when ready for scrolling background
    LDA $2002
    LDA scrollx
    STA $2005
    LDA scrolly
    STA $2005

    DEC scrolly
    LDA scrolly
    CMP #$00
    BEQ swapnametables
    DEC scrolly
    LDA scrolly
    CMP #$00
    BEQ swapnametables
    JMP cleanup
swapnametables:
    LDA nametableswap     ;either %00000000 or %00000010
    EOR #$02
    STA nametableswap
    LDA #$EF
    STA scrolly
cleanup:

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;       DOD data           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Let's start with 64 byte arrays for 64 entities on screen
;That seems pretty chaotic for an NES game

;TODO: if only 8 entity types, consider packing 4 entities to each byte
;Obviously uses 1/4 memory, but would it take more cycles to parse in action?

;On processing, iterate through TYPE as controller. Store a entitycount byte on
;zero page to count how many entities are active at a time, use for the iterator.
;This will make it longer to process even after entities have gone away because if count has
;gone up to 32, even if 30 of the things have been destroyed, #32 might still be alive.
;One cheap subroutine to at least only iterate to highest number is to run through TYPE one time and
;find highest non-zero number then reduce count to that.
;More expensive in a moment, but better overall would be to recompress the TYPE array to fill
;in blanks if there's a convenient time in game (perhaps anytime player pauses or calls the ring menu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;      Graphics Data       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PALETTE:  ;seems like background can only access last 4 palettes?
    ;sprite
    .byte $0E, $1C, $2B, $39 ;palette 1  ;pastel green, blue-green, blue  map grabs this
    .byte $0E, $06, $15, $36 ;palette 2   ;crimson, red, pink    bullet/enemy
    .byte $0A, $05, $26, $30 ;palette 3  ;green, crimson, pink, white
    .byte $0E, $13, $23, $33 ;palette 4   ;purples
    ;background palettes     
    .byte $0E, $1C, $2B, $39 ;palette 1  ;pastel green, blue-green, blue  map grabs this
    .byte $0E, $06, $15, $36 ;palette 2   ;crimson, red, pink    bullet/enemy
    .byte $0A, $05, $26, $30 ;palette 3  ;green, crimson, pink, white
    .byte $0E, $13, $23, $33 ;palette 4   ;purples 

;Consider how to compress. Lookup table? mostly $00s
NAMETABLE0: 
          ;00   01   02   03   04   05   06   07   08   09   0A   0B   0C   0D   0E   0F ; 10   11   12   13   14   15   16   17   18   19   1A   1B   1C   1D   1E   1F  
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $05, $01, $02, $01, $02, $0C, $00, $00, $00, $00, $00 ;00
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $06, $03, $04, $03, $04, $0B, $00, $00, $00, $00, $00 ;01
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $05, $01, $02, $01, $02, $01, $02, $0C, $00, $00, $00, $00, $00 ;02
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $06, $03, $04, $03, $04, $03, $04, $0B, $00, $00, $00, $00, $00 ;03
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $05, $01, $02, $01, $02, $01, $02, $0A, $00, $00, $00, $00, $00 ;04
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $06, $03, $04, $03, $04, $03, $04, $0C, $00, $00, $00, $00, $00 ;05
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00 ,$00, $00, $00, $00, $00, $08, $05, $01, $02, $01, $02, $0C, $00, $00, $00, $00, $00 ;06
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $06, $03, $04, $03, $04, $0B, $00, $00, $00, $00, $00 ;07
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $05, $01, $02, $01, $02, $01, $02, $0C, $00, $00, $00 ;08
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $06, $03, $04, $03, $04, $03, $04, $0B, $00, $00, $00 ;09
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $08, $05, $01, $02, $01, $02, $0C, $00, $00, $00 ;0A
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $06, $03, $04, $03, $04, $0B, $00, $00, $00 ;0B
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $05, $01, $02, $01, $02, $0C, $00, $00, $00 ;0C
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $06, $03, $04, $03, $04, $0B, $00, $00, $00 ;0D
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $05, $01, $02, $01, $02, $01, $02, $0C, $00, $00, $00 ;0E
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $06, $03, $04, $03, $04, $03, $04, $0B, $00, $00, $00 ;0F
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $05, $01, $02, $01, $02, $01, $02, $0C, $00, $00, $00 ;10
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $06, $03, $04, $03, $04, $03, $04, $0B, $00, $00, $00 ;11
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $05, $01, $02, $01, $02, $01, $02, $0C, $00, $00, $00 ;12
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $06, $03, $04, $03, $04, $03, $04, $0B, $00, $00, $00 ;13
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $05, $01, $02, $01, $02, $01, $02, $0C, $0A, $0A, $00, $00, $00 ;14
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $06, $03, $04, $03, $04, $03, $04, $0B, $00, $00, $00, $00, $00 ;15
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $05, $01, $02, $01, $02, $01, $02, $0C, $00, $00, $00, $00, $00 ;16
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $06, $03, $04, $03, $04, $03, $04, $0B, $00, $00, $00, $00, $00 ;17
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $05, $01, $02, $01, $02, $01, $02, $01, $02, $01, $02, $0C, $00 ;18
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $06, $03, $04, $03, $04, $03, $04, $03, $04, $03, $04, $0B, $00 ;19
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $05, $01, $02, $08, $09, $01, $02, $01, $02, $0C, $09, $0A, $00 ;1A
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $06, $03, $04, $0B, $06, $03, $04, $03, $04, $0B, $00, $00, $00 ;1B
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $08, $09, $0A, $00, $08, $09, $08, $09, $0A, $00, $00, $00 ;1C
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00 ;1D
NAMETABLE1:                                                                              ;                                      ;
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $05, $0B, $00, $00, $00, $00, $00, $00, $00, $00, $00
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $05, $01, $02, $0B, $00, $00, $00, $00, $00, $00, $00, $00
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $06, $03, $04, $03, $0B, $00, $00, $00, $00, $00, $00, $00
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $05, $01, $02, $01, $02, $0C, $00, $00, $00, $00, $00, $00
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $06, $03, $04, $03, $04, $0B, $00, $00, $00, $00, $00, $00
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $05, $01, $02, $01, $02, $0C, $00, $00, $00, $00, $00, $00
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $06, $03, $04, $03, $04, $0B, $00, $00, $00, $00, $00, $00
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $05, $01, $02, $01, $02, $0C, $00, $00, $00, $00, $00, $00
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $06, $03, $04, $03, $04, $0B, $00, $00, $00, $00, $00, $00
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $08, $09, $08, $09, $0A, $00, $00, $00, $00, $00, $00
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $05, $0B, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    .byte $00, $00, $00, $00, $00, $00, $00, $05, $01, $02, $0B, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    .byte $00, $00, $00, $00, $00, $00, $05, $01, $02, $01, $02, $0C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    .byte $00, $00, $00, $00, $00, $00, $06, $03, $04, $03, $04, $0B, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    .byte $00, $00, $00, $00, $00, $00, $05, $01, $02, $01, $02, $0C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    .byte $00, $00, $00, $00, $00, $00, $06, $03, $04, $03, $04, $0B, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    .byte $00, $00, $00, $00, $00, $00, $05, $01, $02, $07, $09, $0A, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    .byte $00, $00, $00, $00, $00, $05, $04, $03, $04, $0B, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    .byte $00, $00, $00, $00, $05, $01, $02, $01, $02, $0C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    .byte $00, $00, $00, $00, $06, $03, $04, $03, $04, $0B, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    .byte $00, $00, $00, $00, $05, $01, $02, $07, $09, $0A, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    .byte $00, $00, $00, $05, $04, $03, $04, $0B, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    .byte $00, $00, $05, $01, $02, $01, $02, $0C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    .byte $00, $00, $06, $03, $04, $03, $04, $0B, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    .byte $00, $00, $00, $08, $09, $08, $09, $0A, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00


ATTRIBUTE0:   ;8x8 = 64 bytes $23C0 -> $23FF
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000

ATTRIBUTE1:   ;8x8 = 64 bytes $23C0 -> $23FF
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000

.segment "VECTORS"
    .word VBLANK
    .word RESET
    .word 0

.segment "CHARS"
    .incbin "sprites.chr"