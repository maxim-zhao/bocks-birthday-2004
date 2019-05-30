;.define AlwaysPerfect ; Used to find the maximum scores
.define OffsetArrows 3 ; I originally placed all the artwork by hand, rather than edit a bunch of numbers this lets me apply a modifier to them.
.define DebugTiming ; If defined, we change the border colour during VBlank to measure how long different functions take

;==============================================================
; WLA-DX banking setup
;==============================================================
.memorymap
defaultslot 0
slotsize $7fe0
slot 0 $0000 ; ROM - no banks
slotsize $0010
slot 1 $7fe0 ; Headers act to pad the slot numbers so RAM is in slot 3
slot 2 $7ff0
slotsize $2000
slot 3 $c000 ; RAM
.endme

.rombankmap
bankstotal 1
banksize $7fe0
banks 1
.endro

;==============================================================
; RAM
;==============================================================
.ramsection "Game RAM" slot 3
SpriteTable          dsb $100 ; RAM copy of it
NumSprites           db       ; how many sprites there are in it
SpriteDirection      db       ; flip between 0 and 1 every frame for flickering
ActualPalette        dsb 32
FadeDirection        db ; 1 to fade in, 0 to fade out
ButtonsDown          db ; buttons currently pressed --21RLDU 1 = pressed
ButtonsPressed       db ; buttons currently pressed that weren't pressed last frame
StepDrawingPointer   dw ; pointer to step to be drawn next
StepHappeningPointer dw ; pointer to step that's supposed to be happening around now
StepCounter          db ; frame counter between drawing steps
YScroll              db ; current Y scroll value
Difficulty           db ; which level we're at, 0-3
FramesBetweenEvents  db ; how many frames between step events
FrameCounter         db ; index into FrameCountArray, counts 0..FramesBetweenEvents-1
FrameCountArray      dw ; pointer to an array of delta scroll values for each frame
LastDrawnArrows      db ; a copy of the last drawn data
AmountScrolledThisInterval db ; total amount scrolled since the start of the current event interval

CurrentRating        db ; what grade to show at the moment
RatingTimer          db ; counts down to 0 to hide rating
RatingLength         db ; how long to show ratings for the current track
                        ; must be less than FramesBetweenEvents because I can't show 2 at once
RatingSprites        db ; how many sprites in the current rating, useful for other stuff

ArrowsYOffset        db ; y coordinate for target arrows - needs to be tweaked per-track so that the exact match is not missed
StepLagTime          db ; how many frames after drawing should a step happen
HasHalfSteps         db ; non-zero if the step data includes half-steps, which are coloured differently

ArrowHighByte        db ; high byte for writing arrows to tilemap; for selecting the sprite palette for half-steps when enabled

StepsFrameCounter    db ; for timing in steps handler

ButtonsPressedThisInterval db ; if it's zero then either noting, or an incomplete subset, was pressed

; Scoring variables
ComboLength          dw ; how many steps in the current combo
RatingProcessed      db ; zero when CurrentRating hasn't been processed into the score yet
ScoreHigh            dw ; score (divided by 100) high word
ScoreLow             dw ; score (divided by 100) low word
ScoreTileLocation    dw
ComboTileLocation    dw


VBlankHandler        dw ; so I can have different handlers

.ends

.define End_Of_Step_Data $f0

.org 0

;==============================================================
; SDSC tag and SMS rom header
;==============================================================
.sdsctag 1.1,"Bock's Birthday 2004",SDSCNotes,"Maxim"
.section "Notes" free
SDSCNotes:
.db "https://github.com/maxim-zhao/bocks-birthday-2004"
.db 0
.ends

.bank 0 slot 0
.org $0000
.include "Useful functions.inc"

.section "ZX7" free
.define ZX7ToVRAM
.include "ZX7 decompressor.asm"
.ends

.include "psglib.inc"

;==============================================================
; Boot section
;==============================================================
.org $0000
.section "Boot section" force
  di        ; disable interrupts
  im 1      ; Interrupt mode 1
  jp main   ; jump to main program
.ends

;==============================================================
; VBlank handler
;==============================================================
.org $0038
.section "VBlank handler" force
  in a,$bf ; clear interrupt flag
  ld hl,(VBlankHandler)
  call CallHL
  ei
  reti

CallHL:
  jp (hl)
.ends

.org 0

.section "Game VBlank handler" free
.macro DebugColour args colour
.ifdef DebugTiming
  ld a,colour|$f0
  out (VDPAddress),a
  ld a,$87
  out (VDPAddress),a
.endif
.endm

  ; stuff that has to happen first
GameVBlankHandler:
  DebugColour 1
  call ScrollAndManageArrows ; sometimes slow
  DebugColour 2
  call GetInputs
  ; Stuff that has to happen in the VBlank, and timing-sensitive
  ; things that it depends on
  DebugColour 3
  call ColourArrows
  DebugColour 4
  call UpdatePalette
  DebugColour 5
  call OutputSpriteTable ; always slow
  DebugColour 6
  ; Everything else that can overflow into the active display period without breaking
  ; count down StepLagTime
  ld a,(StepLagTime)
  or a
  jp z,StepsActiveVBlank
  ; stuff to do to count down steps, and to avoid certain things that mustn't happen at that time
  ; if StepLagTime is non-zero then count it down
  dec a
  ld (StepLagTime),a
  DebugColour 0
  ret

StepsActiveVBlank:
  ; stuff to do when steps are active
  call ProcessInputs
  call UpdateRating
  call PSGFrame
  call SlideRating
  call UpdateScore
  ret
.ends

;==============================================================
; Pause button handler
;==============================================================
.org $0066
.section "Pause button handler" force
  ; Do nothing
  retn
.ends

;==============================================================
; Main program
;==============================================================
.section "Main program" free
main:
  ld sp, $dff0

  ; Initialise mapper state
  xor a
  ld ($fffc),a
  ld ($fffd),a
  inc a
  ld ($fffe),a
  inc a
  ld ($ffff),a

  call DefaultInitialiseVDP

  call ClearVRAM

  call TitleScreen
  ld a,(Difficulty)

  push af
    call ClearVRAM

    ; clear RAM
    ld bc,$1f00 ; avoid killing the stack
    ; I don't care about the port $3e value in ram at $c000
    ld hl,$c000
    ld de,$c001
    ld a,0
    ld (hl),a ; zero
    ldir     ; propagate that through RAM

    ; now I can assume uninitialised variables are zero.

    ; load tiles
    ld de,$4000 ; tile index 0
    ld hl,ArrowTiles
    call zx7_decompress

    ld de,$4000+256*32 ; tile index 256
    ld hl,SpriteTiles
    call zx7_decompress

    ; load palette
    ld hl,GamePalette
    ld de,ActualPalette
    ld bc,32
    ldir

  pop af
  ld (Difficulty),a

  xor a
  ld (SpriteDirection),a

  call PSGInit

  ld hl,ButterflyMusic
  call PSGPlayNoRepeat

  ld hl,ButterflySteps ; todo one day: more tracks!
  ld a,(Difficulty)
  add a,a              ; add a*2 to hl
  add a,l
  ld l,a
  jr nc,+
  inc hl
+:ld a,(hl)            ; read what's there into hl
  inc hl
  ld h,(hl)
  ld l,a

  ; read data in
  ld a,(hl)
  inc hl
  ld (FramesBetweenEvents),a

  srl a ; divide by 2 to get half a frame
  ld (StepsFrameCounter),a ; so the current step pointer will update exactly halfway between steps (must be an even number though!)

  ld a,0
  ld (FrameCounter),a
  ld (AmountScrolledThisInterval),a

  ld c,(hl)
  inc hl
  ld b,(hl)
  inc hl
  ld (FrameCountArray),bc

  ; get various track data
  ld a,(hl)
  inc hl
  ld (ArrowsYOffset),a

  ld a,(hl)
  inc hl
  ld (StepLagTime),a

  ld a,(hl)
  inc hl
  push hl
    ld hl,StepsFrameCounter
    add a,(hl)
    ld (hl),a ; add on that extra bit so the steps code will exactly match the beat
  pop hl

  ld a,(hl)
  inc hl
  ld (HasHalfSteps),a
  or a
  jr z,+
  ld a,%00001000
  ld (ArrowHighByte),a ; if HasHalfSteps then first arrow is a half-step
+:

  ld (StepHappeningPointer),hl ; the happening pointer is in time with the music, so it starts late
  ld (StepDrawingPointer),hl   ; the drawing pointer leads it, so it starts straight away

  ; set up sprites - must come after ArrowsYOffset was loaded
  call LoadArrowSprites
  call InitialiseScore ; set up sprites for this too

  ld a,60 ; TODO :change this?
  ld (RatingLength),a

  ld hl,GameVBlankHandler
  ld (VBlankHandler),hl
  
  call WaitForVBlankNoInt

  ; Turn screen on
  ld a,%11100101
;        |||| |`- Zoomed sprites -> 16x16 pixels
;        |||| `-- Doubled sprites -> 2 tiles per sprite, 8x16
;        |||`---- 30 row/240 line mode
;        ||`----- 28 row/224 line mode
;        |`------ VBlank interrupts
;        `------- Enable display
  out ($bf),a
  ld a,$81
  out ($bf),a

  ei

  ; Do everything in interrupts
  Loop:
    jr Loop
.ends




.define L P1L
.define R P1R
.define U P1U
.define D P1D
.define X L+R ; X and Y axes
.define Y U+D
.define Z L+D ; corners of square at left of keyboard for corner-type combos
.define C R+D
.define Q U+L
.define E U+R
.define end End_Of_Step_Data ; low bits unset so it won't draw anything


; All steps are fairly similar, obviously
; The step patterns I downloaded (labelled with "smile.dk" but that seems
; to be something else now) are in the form
;
; I1 I2 A1 A2 B1 B2 C A1 A2 B1 B2 D1 D2
;
; The MIDI I converted with some effort to PSGMOD is in the form
;
; I1 I2 I3 A1 A2 B1 B2 C A1 A2 B1 B2 D1 D2 B1 B2 B1 B2 D1 D2 + outro x 3
;
; so I'm copying and pasting in the extra bits, although they'd be better if there
; was some original source instead since I'm approximating.
; I'm mapping I1 I2 I3 to I1 I1 I2.
;
; RushJet1's awesome VGM seems to be in the form
;
; I1 I2 I3 A1 A2 B1 B2 C A1 A2 B1 B2 D1 D2 + outro?
;
; so I will truncate the steps again for that.

.section "steps" free
ButterflySteps:
.dw ButterflyStepsLevel1, ButterflyStepsLevel2, ButterflyStepsLevel3, ButterflyStepsLevel4
ButterflyStepsLevel1:
.db 24 ; frames between events
.dw ScrollTable2440 ; scrolling delta amount table
.db 0 ; ArrowsYOffset = how much to shift arrow sup so they line up with an exact scrolling point
.db $82 ; StepLagTime = how long to wait after drawing arrows before starting music and processing steps
.db 4 ; StepLagTimeExtra = a little extra wait because the step "beat" may not exactly match the start of the music
.db 0 ; HasHalfSteps = 1 if alternate steps should be a different colour
.db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ; I1
.db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ; I1
.db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ; I2
.db 0,0,R,R,0,0,L,L,0,R,0,L,0,0,R,R ; A1
.db 0,0,L,L,0,0,R,R,0,L,0,R,0,0,L,L ; A2
.db 0,U,U,0,R,0,L,0,R,0,L,0,R,0,0,L ; B1
.db 0,U,U,0,R,0,L,0,R,0,L,0,R,0,0,L ; B2
.db 0,R,0,R,0,L,0,L,0,R,0,R,0,L,0,L ; C
.db 0,0,R,R,0,0,L,L,0,R,0,L,0,0,R,R ; A1
.db 0,0,L,L,0,0,R,R,0,L,0,R,0,0,L,L ; A2
.db 0,U,U,0,R,0,L,0,R,0,L,0,R,0,0,L ; B1
.db 0,U,U,0,R,0,L,0,R,0,L,0,R,0,L,0 ; B2
.db 0,U,0,U,0,U,0,U,0,U,0,U,0,U,0,L ; D1
.db 0,U,0,U,0,U,0,U,0,U,0,U,0,D,0,R ; D2
/*
.db 0,U,U,0,R,0,L,0,R,0,L,0,R,0,0,L ; B1
.db 0,U,U,0,R,0,L,0,R,0,L,0,R,0,L,0 ; B2
.db 0,U,U,0,R,0,L,0,R,0,L,0,R,0,0,L ; B1
.db 0,U,U,0,R,0,L,0,R,0,L,0,R,0,L,0 ; B2
.db 0,U,0,U,0,U,0,U,0,U,0,U,0,U,0,L ; D1
.db 0,U,0,U,0,U,0,U,0,U,0,U,0,D,0,R ; D2
*/
.db end
ButterflyStepsLevel2:
.db 24 ; frames between events
.dw ScrollTable2440
.db 0 ; ArrowsYOffset
.db $82 ; StepLagTime
.db 4 ; StepLagTimeExtra
.db 0 ; HasHalfSteps
.db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ; I1
.db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ; I1
.db 0,0,0,0,0,0,0,0,U,R,U,L,U,R,0,0 ; I2
.db R,0,R,X,L,0,L,X,0,R,D,L,U,R,L,X ; A
.db R,0,R,X,L,0,L,X,0,R,D,L,U,R,L,X ; A
.db 0,U,U,X,R,U,L,R,U,D,L,0,R,L,D,U ; B1
.db 0,U,U,X,R,U,L,D,U,D,L,0,L,R,U,D ; B2
.db D,R,D,R,D,L,D,L,D,R,D,R,D,L,D,L ; C
.db R,0,R,X,L,0,L,X,0,R,D,L,U,R,L,X ; A
.db R,0,R,X,L,0,L,X,0,R,D,L,U,R,L,X ; A
.db 0,U,U,X,R,U,L,D,U,D,L,0,R,L,D,U ; B1
.db 0,U,U,X,R,U,L,D,U,D,R,0,L,R,U,D ; B2
.db 0,D,R,D,L,D,R,D,L,D,R,D,L,U,D,X ; D1
.db 0,D,L,D,R,D,L,D,R,D,L,D,R,U,D,X ; D2
/*
.db 0,U,U,X,R,U,L,D,U,D,L,0,R,L,D,U ; B1
.db 0,U,U,X,R,U,L,D,U,D,R,0,L,R,U,D ; B2
.db 0,U,U,X,R,U,L,D,U,D,L,0,R,L,D,U ; B1
.db 0,U,U,X,R,U,L,D,U,D,R,0,L,R,U,D ; B2
.db 0,D,R,D,L,D,R,D,L,D,R,D,L,U,D,X ; D1
.db 0,D,L,D,R,D,L,D,R,D,L,D,R,U,D,X ; D2
*/
.db end
ButterflyStepsLevel3:
.db 24 ; frames between events
.dw ScrollTable2440
.db 0 ; ArrowsYOffset
.db $82 ; StepLagTime
.db 4 ; StepLagTimeExtra
.db 0 ; HasHalfSteps
.db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ; I1
.db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ; I1
.db 0,0,0,0,0,0,0,0,U,R,U,L,U,R,0,0 ; I2
.db U,0,Y,X,U,0,Y,X,0,R,D,L,U,R,L,X ; A
.db U,0,Y,X,U,0,Y,X,0,R,D,L,U,R,L,X ; A
.db 0,U,U,X,R,E,L,Z,U,D,Z,0,R,L,D,C ; B1
.db 0,U,U,X,R,E,L,Z,U,D,C,0,R,L,U,Z ; B2
.db D,R,D,R,D,L,D,L,D,R,D,R,D,L,D,L ; C
.db U,0,Y,X,U,0,Y,X,0,R,D,L,U,R,L,X ; A
.db U,0,Y,X,U,0,Y,X,0,R,D,L,U,R,L,X ; A
.db 0,U,U,X,R,E,L,Z,U,D,Z,0,R,L,D,E ; B1
.db 0,U,U,X,R,E,L,Z,U,D,C,0,L,R,U,Z ; B2
.db 0,D,R,D,L,D,R,D,L,D,R,D,L,U,D,X ; D1
.db 0,D,L,D,R,D,L,D,R,D,L,D,R,U,D,X ; D2
/*
.db 0,U,U,X,R,E,L,Z,U,D,Z,0,R,L,D,E ; B1
.db 0,U,U,X,R,E,L,Z,U,D,C,0,L,R,U,Z ; B2
.db 0,U,U,X,R,E,L,Z,U,D,Z,0,R,L,D,E ; B1
.db 0,U,U,X,R,E,L,Z,U,D,C,0,L,R,U,Z ; B2
.db 0,D,R,D,L,D,R,D,L,D,R,D,L,U,D,X ; D1
.db 0,D,L,D,R,D,L,D,R,D,L,D,R,U,D,X ; D2
*/
.db end
ButterflyStepsLevel4:
; debugging data:
.db 12 ; frames between events - very fast!
.dw ScrollTable1240 ; scroll table
.db 3 ; ArrowsYOffset
.db $40 ; StepLagTime
.db 4 ; StepLagTimeExtra
.db 1 ; HasHalfSteps
.db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ; I1
.db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ; I1
.db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,D,U ; I2
.db U,0,D,0,X,0,L,D,U,0,D,0,X,0,R,D,L,0,D,0,R,0,D,L,D,0,R,0,L,0,D,R ; A1
.db U,0,D,0,X,0,R,D,U,0,D,0,X,0,L,D,R,0,D,0,L,0,D,R,D,0,L,0,R,0,L,R ; A2
.db X,0,U,0,X,0,L,D,U,0,E,0,Z,0,0,0,R,0,D,0,L,U,R,0,D,0,L,0,R,0,D,L ; B1
.db U,0,U,0,X,0,R,D,U,0,Q,0,C,0,0,0,L,0,D,0,R,U,L,0,D,0,R,0,L,0,R,L ; B2
.db X,0,U,0,R,0,U,R,U,0,R,0,U,0,R,U,L,0,U,0,L,0,U,L,U,0,L,0,U,0,D,U ; C
.db U,0,D,0,X,0,L,D,U,0,D,0,X,0,R,D,L,0,D,0,R,0,D,L,D,0,R,0,L,0,D,R ; A1
.db U,0,D,0,X,0,R,D,U,0,D,0,X,0,L,D,R,0,D,0,L,0,D,R,D,0,L,0,R,0,L,R ; A2
.db X,0,U,0,X,0,L,D,U,0,E,0,Z,0,0,0,R,0,D,0,L,U,R,0,D,0,L,0,R,0,D,L ; B1
.db U,0,U,0,X,0,R,D,U,0,Q,0,C,0,0,0,L,0,D,0,R,U,L,0,D,0,R,0,L,0,R,L ; B2
.db X,0,U,0,R,0,U,R,U,0,L,0,U,0,L,U,R,0,U,0,R,0,U,R,D,0,L,0,R,0,L,R ; D1
.db X,0,D,0,L,0,D,L,D,0,R,0,D,0,R,D,L,0,D,0,L,0,D,L,U,0,R,0,L,0,X,0 ; D2
/*
.db X,0,U,0,X,0,L,D,U,0,E,0,Z,0,0,0,R,0,D,0,L,U,R,0,D,0,L,0,R,0,D,L ; B1
.db U,0,U,0,X,0,R,D,U,0,Q,0,C,0,0,0,L,0,D,0,R,U,L,0,D,0,R,0,L,0,R,L ; B2
.db X,0,U,0,X,0,L,D,U,0,E,0,Z,0,0,0,R,0,D,0,L,U,R,0,D,0,L,0,R,0,D,L ; B1
.db U,0,U,0,X,0,R,D,U,0,Q,0,C,0,0,0,L,0,D,0,R,U,L,0,D,0,R,0,L,0,R,L ; B2
.db X,0,U,0,R,0,U,R,U,0,L,0,U,0,L,U,R,0,U,0,R,0,U,R,D,0,L,0,R,0,L,R ; D1
.db X,0,D,0,L,0,D,L,D,0,R,0,D,0,R,D,L,0,D,0,L,0,D,L,U,0,R,0,L,0,X,0 ; D2
*/
.db end

ScrollTable1240: ; scroll 40 lines in 12 frames
.db 3,3,4,3,3,4,3,3,4,3,3,4
ScrollTable2440: ; scroll 40 lines in 24 frames
.db 1,2,2,1,2,2,1,2,2,1,2,2,1,2,2,1,2,2,1,2,2,1,2,2

.ends

.undefine L, R, U, D, X, Y, Z, C, Q, E, end

.section "Graphics data" free
ArrowTiles:
.incbin "backgrounds/arrows.png.tiles.zx7"
GamePalette:
.db cl012 cl000 cl111 cl333 cl330 cl231 cl232 0     0     0     0     0     0     0     0     0
.db cl012 cl000 cl111 cl333 cl211 cl212 cl202 0     0     0     0     0     0     0     0     0
;   BG    <-black-g-white-> >-Arrow---------> <-Text colour scroll--> <-Top arrow entries-> Unused
; Notice the alternative arrow colour using the sprite palette
.enum 0
  PaletteBG db
  PaletteGreys dsb 3
  PaletteArrows dsb 3
  PaletteText dsb 4
  PaletteArrowHighlights dsb 4
.ende
.ends

.section "Sprites data" free
SpriteTiles:
.incbin "sprites/sprites.png.tiles.zx7"
.ends

.section "Output sprite table to VRAM with flickering" free
OutputSpriteTable:
; I need to see how many sprites there are
; then output data accordingly
  ld hl,SpriteTableAddress
  call VRAMToHL

  ld a,(SpriteDirection)
  xor 1 ; invert low bit
  ld (SpriteDirection),a
  jr z,_OutputBackwards

  ; output forwards
  ld hl,SpriteTable
  ld c,VDPData
  ld b,0
  otir
  call Output208IfNeeded ; to signal end of sprite list
  ret

_OutputBackwards:
  ; first, output y coordinates backwards
  ld hl,SpriteTable

  ld b,0
  ld a,(NumSprites)
  ld c,a
  add hl,bc
  dec hl           ; hl points to last y coordinate

  ld b,a           ; counter
  ld c,VDPData     ; port
  otdr             ; output from (hl) backwards

  ld hl,SpriteTableAddress+128
  call VRAMToHL

  ld ix,SpriteTable+128 ; point to x/index section
  ld b,0
  ld a,(NumSprites)
  ld c,a
  dec c
  sla c ; multiply by 2 to give the section size
  add ix,bc ; hl points to last x coordinate (the index is after it)

  ld b,a ; b = counter for all sprites

-:ld a,(ix+0)
  out (VDPData),a ; output x coordinate
  ld a,(ix+1)
  out (VDPData),a ; output index
  dec ix
  dec ix
  djnz -

  call Output208IfNeeded ; to signal end of sprite list

  ret

Output208IfNeeded:
  ; if NumSprites<64 then I must also output a y coordinate of 208 to stop any more sprites showing
  ; it should be 0 <= NumSprites <= 64
  ld a,(NumSprites)
  cp 64
  ret z
  ; calcualte VRAM address for it
  ; which is SpriteTableAddress+a
  ld hl,SpriteTableAddress
  add a,l
  ld l,a
  call VRAMToHL
  ld a,208
  out (VDPData),a
  ret
.ends

.section "Write palette to CRAM" free
UpdatePalette:
  ld hl,$c000
  call VRAMToHL
  ld c,VDPData
  ld hl,ActualPalette
  ld b,32
  otir            ; Output b bytes starting at hl to port c
  ret
.ends

.section "Get inputs" free
GetInputs:
  in a,(IOPort1)     ; Get controls
  ld hl,ButtonsDown
  cpl                ; Invert so 1 = pressed
  ld b,a             ; b = all buttons currently pressed
  xor (hl)           ; now bits in a are only set for buttons that have changed since last time (either just pressed or just released)
  ld (hl),b          ; Store b in ButtonsDown
  inc hl             ; assume ButtonsDown precedes ButtonsPressed in RAM
  and b              ; this filters out the "just released" so now bits are set only for buttons just pressed
  ld (hl),a          ; Store a in ButtonsPressed
  ret
.ends

.section "Colour arrows according to the buttons pressed" free
ColourArrows:
  ld ix,ActualPalette+16+PaletteArrowHighlights
  ; colours are at indices 11-14 in the sprite palette
  ; in the order LDUR
  ld a,(ButtonsDown)
;  ld b,a

  bit 2,a ; is L pressed?
  jr z,+
  ld (ix+0),cl300
  jr ++
+:ld (ix+0),cl333
++:
  bit 1,a ; is D pressed?
  jr z,+
  ld (ix+1),cl300
  jr ++
+:ld (ix+1),cl333
++:

  bit 0,a ; is U pressed?
  jr z,+
  ld (ix+2),cl300
  jr ++
+:ld (ix+2),cl333
++:

  bit 3,a ; is R pressed?
  jr z,+
  ld (ix+3),cl300
  jr ++
+:ld (ix+3),cl333
++:
  ret
.ends

.section "Scroll and manage arrows" free
LeftArrow:
; Tilemap data cut out of BMP2Tile text format
; Must be recreated any time arrows.png is edited
.dw $0000 $0001 $0002 $0003 $0000
.dw $0006 $0009 $000A $000B $000C
.dw $0017 $0018 $0019 $001A $001B
.dw $0406 $0616 $0615 $0614 $040C
.dw $0000 $0401 $0608 $0403 $0000

DownArrow:
.dw $0000 $0004 $0005 $0204 $0000
.dw $000D $000E $000F $0010 $020D
.dw $001C $001D $001E $001F $0020
.dw $0401 $0411 $0412 $0413 $0601
.dw $0000 $0406 $0407 $0606 $0000

UpArrow:
.dw $0000 $0006 $0007 $0206 $0000
.dw $0001 $0011 $0012 $0013 $0201
.dw $041C $041D $041E $041F $0420
.dw $040D $040E $040F $0410 $060D
.dw $0000 $0404 $0405 $0604 $0000

RightArrow:
.dw $0000 $0203 $0008 $0201 $0000
.dw $020C $0014 $0015 $0016 $0206
.dw $061B $061A $0619 $0618 $0617
.dw $060C $060B $060A $0609 $0606
.dw $0000 $0603 $0602 $0601 $0000

BlankSpace:
.dsw 5*5, 0

; Scroll the screen up
; If it's time for a new arrow, draw it, overwriting any old ones
ScrollAndManageArrows:

  ; FrameCounter counts betwen step events
  ; it's also the index into FrameCountArray for delta scroll values
  ld a,(FrameCounter)        ; increment counter
  inc a
  ld hl,FramesBetweenEvents
  cp (hl)                    ; if it's time for a new event
  jr nz,+
  xor a                      ; reset the counter
+:ld (FrameCounter),a        ; save result

  ; look up how much to scroll this frame
  ld hl,(FrameCountArray)
  add a,l
  ld l,a
  jr nc,+
  inc h ; if there was a carry then h should be incremented
+:      ; hl points to the delta scroll for this frame

  ; debug: save to RAM where I can see it
;  ld a,(hl)
;  ld ($d000),a

  ld a,(AmountScrolledThisInterval)
  add a,(hl)
  ld (AmountScrolledThisInterval),a

  ld a,(YScroll) ; add that on to the current Y scroll value
  add a,(hl)
  cp 224 ; has it overflowed? it will carry for values < 224
  jr c,+
  sub 224 ; so subtract 224 to keep it looping properly
+:ld (YScroll),a        ; save it
  out (VDPAddress),a    ; output it
  ld a,VDPRegVScroll
  out (VDPAddress),a    ; to the VScroll register

  ; debug: put in RAM so I can watch it
;  ld a,(hl)
;  ld ($d000),a

  ; check if it's time to draw the bottom half of any arrows
  ld a,(AmountScrolledThisInterval) ; get amount scrolled
  cp 2*8                            ; see if it's more than 2 rows
  jr c,+                            ; skip if not
  ; OK, check which to draw
  ld ix,LastDrawnArrows
  ld a,(ix+0)
  cp $ff ; ff means don't draw anything
  jr z,+

  call GetReadyForDrawing ; find where to draw and set hl to the VRAM pointer value

  ld de,LeftArrow+5*2*2
  bit 2,(ix+0)
  call z,NoArrow
  call DrawArrowSection
  ld bc,6*2
  add hl,bc
  ld de,DownArrow+5*2*2
  bit 1,(ix+0)
  call z,NoArrow
  call DrawArrowSection
  ld bc,6*2
  add hl,bc
  ld de,UpArrow+5*2*2
  bit 0,(ix+0)
  call z,NoArrow
  call DrawArrowSection
  ld bc,6*2
  add hl,bc
  ld de,RightArrow+5*2*2
  bit 3,(ix+0)
  call z,NoArrow
  call DrawArrowSection
  ; signal that I've done it
  ld a,$ff
  ld (ix+0),a
+:

  ; check back if it was time for a new event - if not, I've finished
  ld a,(FrameCounter)
  or a
  ret nz

  ; if I'm here then the counter was just reset to zero
  ; and it's time to think about drawing a new arrow

  ; reset AmountScrolledThisInterval
  xor a
  ld (AmountScrolledThisInterval),a

  ; now I know where to draw
  ; now I need to figure out what to draw

  ld ix,(StepDrawingPointer)

  call GetReadyForDrawing ; find where to draw and set hl to the VRAM pointer value

  ; set up ArrowHighByte if needed
  ld a,(HasHalfSteps)
  or a
  jr z,+
  ld a,(ArrowHighByte)
  xor %00001000
  ld (ArrowHighByte),a


+:
  ; ----RLDU
  ld de,LeftArrow
  bit 2,(ix+0)
  call z,NoArrow
  call DrawArrowSection
  ld bc,6*2
  add hl,bc
  ld de,DownArrow
  bit 1,(ix+0)
  call z,NoArrow
  call DrawArrowSection
  ld bc,6*2
  add hl,bc
  ld de,UpArrow
  bit 0,(ix+0)
  call z,NoArrow
  call DrawArrowSection
  ld bc,6*2
  add hl,bc
  ld de,RightArrow
  bit 3,(ix+0)
  call z,NoArrow
  call DrawArrowSection
  ; signal to draw the bottom half later
  ld a,(ix+0)
  ld (LastDrawnArrows),a

  ; move pointer forwards if the high bit's not set
  bit 7,(ix+0)
  ret nz

  inc ix
  ld (StepDrawingPointer),ix
  ret


GetReadyForDrawing:
; clobbers a, b, c, returns in hl

  ; I want to draw it at row (scroll mod 8)+24 to be offscreen
  ; so I'll calculate that
  ld a,(YScroll)
  srl a
  srl a
  srl a         ; divide by 8 = number of full rows currently scrolled
  add a,24      ; add 24 rows <----------------------------------------- debug: lower value so it's on-screen
  cp 28         ; 28 or more?
  jr c,_lessthan28
  sub 28
_lessthan28:
  ld b,a        ; b = row number to draw at

  ; multiply that by 64 and add to TileMapAddress to get the VRAM address
  ; shift b right 2 bits into a and ba will be the answer
  xor a        ; a=0
  srl b        ; shift b into carry
  rr a         ; shift carry into a
  srl b        ; repeat
  rr a         ;
  ld h,b       ; hl = b*64
  ld l,a
  ld bc,(TileMapAddress-$4000)+2*(2+OffsetArrows)  ; draw from 2 tiles from the left
  add hl,bc
  ; hl now holds the VRAM address to draw at
  ret


NoArrow:
  ld de,BlankSpace
  ret

DrawArrowSection:
; draw 5x3 word tile data from (de) to VRAM at (hl)
; do not destroy hl
  push hl
    call VRAMToHL
    call Output5
    call NextRow
    call VRAMToHL
    call Output5
    call NextRow
    call VRAMToHL
    call Output5
  pop hl
  ret

Output5:
  ; output 5 tiles from de to VDPData
  ; save hl, increment de by 5
  push hl
    ld h,d ; ld hl,de
    ld l,e
    ld b,5
  -:ld a,(hl)
    out (VDPData),a
    inc hl
    ld a,(ArrowHighByte)
    or (hl)
    out (VDPData),a
    inc hl
    djnz -
    ld d,h ; ld de,hl
    ld e,l
  pop hl
  ret

NextRow:
; add 64 to hl
; if it's more than $3f00 then subtract $700
; do not detroy de
  ld bc,64 ; 1 row
  add hl,bc
  ld a,h
  cp $3f
  ret c
  ; it is more than $3f
  sub $07
  ld h,a
  ret

.ends

.section "Rating handler" free
; parameters: a = rating
NewRating:
  ; check if a rating is active; if so, turn it off
  push af
    ld a,(RatingTimer)
    or a ; will be non-zero if a rating is active
    call nz,TurnOffRating
  pop af
  ld (CurrentRating),a ; save it to RAM, for when I need to look at it somewhere else (? do I need to any more?)

  add a,a ; multiply by 2
  ld hl,Ratings
  add a,l
  ld l,a
  jr nc,+
  ; carry means h needs to change
  inc h
+:ld c,(hl)
  inc hl
  ld b,(hl)
  push bc
  pop hl ; hl now points to the data

  push hl
    ; find where to write it
    ld ix,SpriteTable
    ld b,0
    ld a,(NumSprites)
    ld c,a
    add ix,bc
    ; ix points to the place to write the y coordinates (in RAM)
    ld c,55 ; c = initial y coordinate. 55 is the highest it can go before it starts clashing with the arrows.
    ; run through the list, writing a to RAM
    push af
      ld a,(hl)
      ld (RatingSprites),a
      ld b,a ; b = how many sprites
    pop af
    inc hl
    inc hl
  -:ld a,(hl)
    inc hl
    or a ; is it zero?
    jr nz,+
    ; if it is, add 16 to the y coordinate
    ld a,c
    add a,16
    ld c,a
    inc hl ; move hl on so I don't lose count when there's more than 2 rows (not used)
  +:ld (ix+0),c ; write y coordinate to RAM
    inc ix
    djnz -      ; repeat for all sprites
  pop hl

  ; next, x coordinates and tile indices
  ; find where to write these now
  ld ix,SpriteTable+128
  ld b,0
  ld a,(NumSprites)
  ld c,a
  add ix,bc
  add ix,bc
  ; ix points to the next free space

  ld b,(hl)   ; get sprite count
  ld e,b      ; remember that for in a minute
  inc hl
  ld c,(hl)   ; get initial x value
  ld d,c      ; remember that initial value
  inc hl

-:ld a,(hl)   ; get data
  inc hl

  or a        ; is it zero?
  jr nz,+

  ; zero means I need to reset the x value and get another byte
  ld c,d
  ld a,(hl)
  inc hl

+:ld (ix+0),c ; output x
  inc ix
  ld (ix+0),a ; output tile number
  inc ix

  ; add 16 to x coordinate
  ld a,c
  add a,16
  ld c,a

  djnz -      ; loop over all sprites

  ; increase NumSprites so the sprite table copier will include them
  ld a,(NumSprites)
  add a,e
  ld (NumSprites),a

  ; load palette
  ; hl points to it
  ld de,ActualPalette+16+PaletteText ; that's where my colour cycling is
  ldi
  ldi
  ldi
  ldi
  ; faster than ldir

  ; reset the timer
  ld a,(RatingLength)
  ld (RatingTimer),a

  ; signal that the rating needs to be applied to the score
  xor a
  ld (RatingProcessed),a

  ret

Ratings:
.dw RatingPerfect
.dw RatingGreat
.dw RatingGood
.dw RatingBoo
.dw RatingMiss

RatingPerfect:
.db 22 ; number of sprites
.db OffsetArrows*8+20 ; starting x value
.db $21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,0 ; tile numbers - 0 signals end-of-row
.db $2c,$2d,$2e,$2f,$30,$31,$32,$33,$34,$35,$36
.db cl030,cl020,cl010,cl020 ; palette (4 colours)

RatingGreat:
.db 18,OffsetArrows*8+40
.db $37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f,0
.db $40,$41,$42,$43,$44,$45,$46,$47,$48
.db cl033,cl032,cl031,cl032

RatingGood:
.db 14,OffsetArrows*8+58
.db $49,$4a,$4b,$4c,$4d,$4e,$4f,0
.db $50,$51,$52,$53,$54,$55,$56
.db cl021,cl221,cl330,cl221

RatingBoo:
.db 10,OffsetArrows*8+68
.db $57,$58,$59,$5a,$5b,0
.db $5c,$5d,$5e,$5f,$60
.db cl100,cl200,cl300,cl200

RatingMiss:
.db 14,OffsetArrows*8+57
.db $61,$62,$63,$64,$65,$66,$67,0
.db $68,$69,$6a,$6b,$6c,$6d,$6e
.db cl300,cl201,cl000,cl302

UpdateRating:
  ; decrement counter
  ld a,(RatingTimer)
  or a
  ret z ; do nothing if counter is at zero
  dec a
  ld (RatingTimer),a
  jr nz,++

  ; counter just ran out
  call TurnOffRating
  ret

++:
  ; rating is active
  ld a,(RatingTimer)
  and $3
  jr nz,+
  ; every 4 frames, rotate the palette
  ld a,16+PaletteText ; starting index
  ld c,4    ; count
  call RotatePalette
+:

  ret

TurnOffRating:
  ; turn off rating display
  
/*
  ld a,(CurrentRating)
  and $f ; cut off high bits
  add a,a ; multiply by 2
  ld hl,Ratings
  add a,l
  ld l,a
  jr nc,+
  ; carry means h needs to change
  inc h
+:ld c,(hl)
  inc hl
  ld b,(hl)
  push bc
  pop hl ; hl now points to the data
  */
  ld a,(NumSprites)
  ld hl,RatingSprites
  sub (hl) ; subtract the current rating's sprite count
  ld (NumSprites),a ; save count
  ret ; all done


.ends

.section "Rotate palette" free
; c = starting index
; b = count
RotatePalette:
  ld hl,ActualPalette
  add a,l
  ld l,a
  jr nc,+
  inc h ; if a+l overflowed
+:; hl now points to the starting index
  push hl ; ld de,hl
  pop de

  ld a,(hl) ; get first one

  inc hl
  ld b,0
  dec c
  ldir ; copy (count-1) bytes to the left

  dec hl
  ld (hl),a ; and put the first one on the end

  ret
.ends

.section "Arrow sprites" free
LoadArrowSprites:
  ld hl,SpriteData
  ld de,SpriteTable
  ld bc,$100
  ldir ; copy to RAM sprite table copy

  ld a,32
  ld (NumSprites),a

  ; I want to tweak their positions according to the value of ArrowsYOffset
  ; ie. I subtract that from each of them
  ld a,(ArrowsYOffset)
  ld c,a
  ld b,32
  ld hl,SpriteTable
-:ld a,(hl)
  sub c
  ld (hl),a
  inc hl
  djnz -

  ret

SpriteData:
.define X OffsetArrows*8
; very lazy method: raw VRAM data
; y positions
.db  8, 8, 8,  9, 9, 9,  9, 9, 9,  8, 8, 8 ; 12
.db 24,24,24, 25,25,25, 25,25,25, 24,24,24 ; 12
.db 40,40,    41,41,    41,41,    40,40 ; 8
.dsb 32,0 ; unused
; unused section
.dsb 64,0
; x positions/tile indices
.db X+18,$01,X+34,$02,X+50,$03, X+65,$04,X+81,$05,X+97,$06, X+113,$07,X+129,$08,X+145,$09, X+162,$0a,X+178,$0b,X+194,$0c
.db X+18,$0d,X+34,$0e,X+50,$0f, X+65,$10,X+81,$11,X+97,$12, X+113,$13,X+129,$14,X+145,$15, X+162,$16,X+178,$17,X+194,$18
.db X+18,$19,X+34,$1a,          X+65,$1b,X+81,$1c,          X+113,$1d,X+129,$1e,           X+162,$1f,X+178,$20
.undef X
.ends

.section "Input testing" free
; this has to
; 1. keep up to date with what step is supposed to be happening nearest to now (not necessarily which one was last or which is next)
; 2. check if player input is correct
; 3. apply a rating to the player's input
; 4. (ultimately) keep score
ProcessInputs:
  ; First, checking what step is supposed to happen now
  ; the current step changes every FramesBetweenEvents frames
  ld ix,(StepHappeningPointer)

  ld a,(StepsFrameCounter)
  dec a
  jr nz,+

  ; zero:
  ld a,(FramesBetweenEvents)  ; reset counter
+:ld (StepsFrameCounter),a    ; save to RAM
  jr nz,++
  ; counter is zero, do stuff that must happen at the start of the step interval

  ; increment pointer if it is not pointing at End_Of_Step_Data
  ld a,(ix+0)
  cp End_Of_Step_Data
  jr z,++
  
  and $f ; zero high bits

  ; if the current step is non-zero
  or a
  jr z,+
  ld a,(ButtonsPressedThisInterval)
  or a ; and ButtonsPressedThisInterval IS zero
  jr nz,+
  ; then it's a miss
  push ix
.ifdef AlwaysPerfect
    ; a cheat for getting Perfect ratings instead of Miss
    xor a
.else
    ld a,4
.endif
    call NewRating
  pop ix

+:xor a
  ld (ButtonsPressedThisInterval),a ; reset ButtonsPressedThisInterval

  inc ix
  ld (StepHappeningPointer),ix  ; increment pointer

  ; StepHappeningPointer is now pointing at the closest step

++:
/*
  ; debug: make it viewable
  xor a
  bit 2,(ix+0) ; L
  jr z,+
  cpl
+:ld ($d000),a
  xor a
  bit 1,(ix+0) ; D
  jr z,+
  cpl
+:ld ($d001),a
  xor a
  bit 0,(ix+0) ; U
  jr z,+
  cpl
+:ld ($d002),a
  xor a
  bit 3,(ix+0) ; R
  jr z,+
  cpl
+:ld ($d003),a
*/
  ; next: compare with player input
  ; when the player presses something, it'll be in ButtonsPressed for one frame only

  ld a,(ButtonsPressed)
  and %00001111 ; I only care about the direction buttons
  or a
  ; if there's nothing there, don't process it
  jr z,_NoButtonsPressed

  ; merge into ButtonsPressedThisInterval
  push af
    ld hl,ButtonsPressedThisInterval
    or (hl)
    ld (hl),a
  pop af

  ; test values
  ; Wanted   %0011
  ; Pressed  %0011 -> match! apply rating
  ; Pressed  %0000 -> do nothing, maybe it'll be pressed later
  ; Pressed  %01?? -> incorrect button pressed, automatic failure
  ; Pressed  %0010 -> subset pressed - wait to see if the full set is pressed, so do nothing


  ; compare what was pressed with what was wanted
  ; at the end, it's End_Of_Step_Data, I need to mask off those high bits
  push af
    ld a,(ix+0)
    and $0f
    ld b,a
  pop af
  cp b
  jr z,_ExactMatch

  ; Does not exactly match
  ; first, see if any of the pressed buttons are outside the required set, which is an immediate failure
  ; a is the buttons pressed
  ; b is what's wanted
  ; so a AND NOT B will be non-zero in that case
  ; By some mind-breaking bitwise logical stuff, I can show that
  ; a AND NOT B === a OR b XOR b
  ; a  b  NOT b  a AND NOT b  a OR b  a OR b XOR b
  ; 0  0    1         0         0          0
  ; 0  1    0         0         1          0
  ; 1  0    1         1         1          1
  ; 1  1    0         0         1          0
  ; so that's a good test for me
  ld c,a ; save value
  or b
  xor b
  jr nz,_WrongButtonsPressed ; at least one of the pressed buttons is outside the wanted set

  ; by this point, at least one button is pressed, it's in the wanted set but it does not equal it
  ; so it's a subset of the wanted buttons
  ; so let's be nice and allow it to roll over into the next frame as if it wasn't pressed yet
  ld hl,ButtonsDown
  ld a,c ; restore saved value
  xor (hl) ; a = ButtonsDown with the corresponding bit unset
  ld (hl),a ; store that so next time around, it'll light up that bit in ButtonsPressed again

  ; I also need to remove that bit from ButtonsPressedThisInterval so it'll signal a miss if the set isn't completed
  ld hl,ButtonsPressedThisInterval
  ld a,c
  xor (hl)
  ld (hl),a

  ; and act as if no buttons were pressed
  jr _NoButtonsPressed

_ExactMatch:
  call CalculateRating
  call NewRating
  ret

_WrongButtonsPressed:
  ld a,4
  call NewRating
  ret

_NoButtonsPressed:

  ret
.ends

.section "Rating calculator" free
; calculates what rating to give the player
; returns in a
CalculateRating:
  ; StepsFrameCounter should be halfway between 0 and FramesBetweenEvents for an exact match
  ld a,(FramesBetweenEvents)
  srl a ; divide by 2
  ld hl,StepsFrameCounter
  sub (hl)
  or a ; is it negative?
  jp p,+ ; can't jr on this condition
  neg ; if so, negate it
+:

.ifdef DEBUG
  ; debug: save stuff in RAM where I can see it
  push af
    ld ($d010),a
    ld a,(FramesBetweenEvents)
    ld ($d020),a
    ld a,(StepsFrameCounter)
    ld ($d021),a
  pop af
.endif

  ; how close?
  ; 0 or 1: perfect (I am generous)
  cp 1+1
  jr nc,+
  ld a,0
  ret
  ; 2 or 3: great
+:cp 3+1
  jr nc,+
  ld a,1
  ret
  ; 4, 5, 6: good
+:cp 6+1
  jr nc,+
  ld a,2 ; return 2
  ret
  ; any more: boo
+:ld a,3
  ret

.ends

.section "Rating Y position slider" free
SlideRating:
  ; only if rating is active!
  ld a,(RatingTimer)
  or a
  ret z
  ld hl,RatingSprites
  ld a,(NumSprites)
  sub (hl) ; subtract the current rating's sprite count
  ld b,(hl)
  ; I want to increment the y position of the b sprites starting at index a
  ld hl,SpriteTable
  add a,l ; won't overflow because SpriteTable is 256-aligned
  ld l,a
-:inc (hl)
  inc hl
  djnz -
  ret ; all done
.ends

.section "Score routines" free
UpdateScore:
  ld a,(RatingProcessed)
  or a
  ret nz ; do nothing if it's non-zero

  ; signal that I've handled it for the above code
  ld a,1
  ld (RatingProcessed),a

  ld a,(CurrentRating) ; get current rating

  ld hl,(ComboLength) ; get current combo length
  
  ; check rating
  or a
  jr z,_Perfect
  dec a
  jr z,_Great
  dec a
  jr z,_Good
  
  jr _Bad
  
_Perfect:
  call _IncrementComboLength
  call _CalcM
  ; score = M * M * 3
  ld b,h
  ld c,l
  add hl,hl
  add hl,bc ; hl = M * 3
  ld d,h
  ld e,l
  call Mul16 ; dehl = M * M * 3
  ; could be something like 10000 for a 250-step combo
  ; assume it's less than 65536 (would require a 592-step combo)
  ; so I can use de
  call _AddToScore
  jp UpdateScoreDisplay

_Great:
  call _IncrementComboLength
  call _CalcM
  ; score = M * M
  ld b,h
  ld c,l
  ld d,h
  ld e,l
  call Mul16
  call _AddToScore
  jp UpdateScoreDisplay

_Good:
  inc hl ; increment ComboLength
  call _ResetComboLength ; but also reset it
  ; score = M
  call _AddToScore
  jp UpdateScoreDisplay

_Bad:
  call _ResetComboLength
  jp UpdateScoreDisplay

_ResetComboLength:
  xor a
  ld (ComboLength),a
  ld (ComboLength+1),a
  ret

_IncrementComboLength:
  inc hl
  ld (ComboLength),hl
  ret

_CalcM:
  ; M = ComboLength / 4
  srl h ; shift h into carry, fill with 0
  rr l ; shift carry into l
  srl h ; do it again -> divide by 4
  rr l
  ; check if it's 0
  ld a,h
  or l
  ; combo <4 should give 1, not zero
  inc hl
  ret

_AddToScore:
  ld de,(ScoreLow)
  add hl,de
  ld (ScoreLow),hl
  ret nc
  ld hl,(ScoreHigh)
  inc hl
  ld (ScoreHigh),hl
  ret

RatingScores:
.db 3,1,1,0,0

; Copied code :)
Mul16:                           ; This routine performs the operation DEHL=BC*DE
  ld hl,0
  ld a,16
Mul16Loop:
  add hl,hl
  rl e
  rl d
jp nc,NoMul16
  add hl,bc
  jp nc,NoMul16
  inc de                         ; This instruction (with the jump) is like an "ADC DE,0"
NoMul16:
  dec a
  jp nz,Mul16Loop
  ret
  
  
; back to my code
InitialiseScore:
  ; set up score in sprite table
  ; score at the bottom of the screen (y = 170ish)
  ; combo centred above it
  ; score is 8 digits

  ; get memory address of y coordinates
  ld hl,SpriteTable
  ld a,(NumSprites)
  add a,l
  ld l,a

  ; draw score: 8 identical Y coordinates
  ld a,170
  ld b,8
-:ld (hl),a
  inc hl
  djnz -

  ; Combo display: 2 (that's all I have space for)
  ld a,170-16-3 ; should be OK
  ld b,2
-:ld (hl),a
  inc hl
  djnz -

  ; Next, x coordinates and tile numbers
  ; get the address:
  ld hl,SpriteTable+128
  ld a,(NumSprites)
  add a,a
  add a,l
  ld l,a

  ld (ScoreTileLocation),hl ; save the index of the first digit

  ; start at x=64 for the score
  ld a,64
  ld b,8
-:ld (hl),a
  add a,16
  inc hl
  ld (hl),DIGIT_0 ; zero digit
  inc hl
  djnz -

  ld (ComboTileLocation),hl

  ; start at 104 for the combo number
  ld a,112
  ld b,2 ; 2 zeroes
-:ld (hl),a
  add a,16
  inc hl
  ld (hl),DIGIT_0 ; zero digit
  inc hl
  djnz -

  ; signal that these sprites are there
  ld a,(NumSprites)
  add a,8+2
  ld (NumSprites),a

  ; set up control variables
  ld a,1
  ld (RatingProcessed),a
  ld hl,0
  ld (ScoreLow),hl
  ld (ScoreHigh),hl
  ld (ComboLength),hl

  ret

UpdateScoreDisplay:

.define DIGIT_0 $6f

  ; read score from ScoreHigh, ScoreLow
  ; convert to decimal (!)
  ; change sprite indices one at a time
  ld iy,(ScoreTileLocation) ; where to write to

  ; this is hard!
  ; get 1st digit (10,000,000s)
  ld hl,(ScoreHigh)
  ld ix,(ScoreLow)

  ld bc,$ff67 ; bcde = -10000000
  ld de,$6980 ;
  ld a,-1 ; reset counter

-:inc a
  add ix,de
  adc hl,bc ; hlix += bcde

  ; by adding a negative number, the result will carry (borrow) and be the same as a subtraction
  ; but I want to know if it doesn't carry because then I've gone one too far

  jr c,-

  ; undo the last subtraction
  ld bc,$0098
  ld de,$9680
  add ix,de
  adc hl,bc

  ; that's my digit
  add a,DIGIT_0 ; index of 0 sprite
  ld (iy+1),a

  ; and repeat for other digits

  ld bc,$fff0 ; bcde = -1000000
  ld de,$bdc0 ;
  ld a,-1 ; reset counter
-:inc a
  add ix,de
  adc hl,bc ; hlix += bcde
  jr c,-
  ld bc,$000f
  ld de,$4240
  add ix,de
  adc hl,bc
  add a,DIGIT_0
  ld (iy+3),a

  ld bc,$fffe ; bcde = -100000
  ld de,$7960 ;
  ld a,-1 ; reset counter
-:inc a
  add ix,de
  adc hl,bc ; hlix += bcde
  jr c,-
  ld bc,$0001
  ld de,$86a0
  add ix,de
  adc hl,bc
  add a,DIGIT_0
  ld (iy+5),a

  ld bc,$ffff ; bcde = -10000
  ld de,$d8f0 ;
  ld a,-1 ; reset counter
-:inc a
  add ix,de
  adc hl,bc ; hlix += bcde
  jr c,-
  ld bc,$0000
  ld de,$2710
  add ix,de
  adc hl,bc
  add a,DIGIT_0
  ld (iy+7),a

  ; maybe I could avoid processing all 32 bits now
  ld bc,$ffff ; bcde = -1000
  ld de,$fc18 ;
  ld a,-1 ; reset counter
-:inc a
  add ix,de
  adc hl,bc ; hlix += bcde
  jr c,-
  ld bc,$0000
  ld de,$03e8
  add ix,de
  adc hl,bc
  add a,DIGIT_0
  ld (iy+9),a

  ; only 8 bits now
  ld bc,$ffff ; bcde = -100
  ld de,$ff9c ;
  ld a,-1 ; reset counter
-:inc a
  add ix,de
  adc hl,bc ; hlix += bcde
  jr c,-
  ld bc,$0000
  ld de,$0064
  add ix,de
  adc hl,bc
  add a,DIGIT_0
  ld (iy+11),a

  ld bc,$ffff ; bcde = -10
  ld de,$fff6 ;
  ld a,-1 ; reset counter
-:inc a
  add ix,de
  adc hl,bc ; hlix += bcde
  jr c,-
  ld bc,$0000
  ld de,$000a
  add ix,de
  adc hl,bc
  add a,DIGIT_0
  ld (iy+13),a

  ; only the last digit left
  ld a,ixl
  add a,DIGIT_0
  ld (iy+15),a

  ; Now, do the same for the combo
  ld iy,(ComboTileLocation) ; where to write to
  
  ld a,(ComboLength+1)
  or a
  jr nz,_MoreThan99

  ld a,(ComboLength) ; only look at the LSB which is the first one

  ; check if it's more than 99
  cp 99+1
  jr nc,_MoreThan99

  ; count the tens
  ld c,-1 ; how many I found
  ld b,10 ; how much to subtract each time
-:inc c
  sub b
  jr nc,-
  add a,b
  ; that's it
  push af
    ld a,c
    add a,DIGIT_0 ; index of 0 tile
    ld (iy+1),a
  pop af
  add a,$6f
  ld (iy+3),a

  ret

_MoreThan99:
  ld a,DIGIT_0+10 ; index of >99 tiles
  ld (iy+1),a
  inc a
  ld (iy+3),a

  ret

.ends

.section "Intro screens" free

ShowScreenAndWait:
  call ShowScreen
  call WaitForButton
  call FadePaletteOut
  jp TurnOffScreen ; and ret

ShowScreen:
  ld l,(ix+0)
  ld h,(ix+1)
  ld de,$4000 ; tile index 0
  call zx7_decompress
  ld l,(ix+2)
  ld h,(ix+3)
  ld de,TileMapAddress
  call zx7_decompress
  ld l,(ix+4)
  ld h,(ix+5)
  ld de,ActualPalette
  ld bc,16
  ldir
  call TurnOnScreen
  jp FadePaletteIn ; and ret

TitleScreen:
  call TurnOffScreen

  ; turn off sprites
  call NoSprites

  ld ix,xiao
  call ShowScreenAndWait

  ld ix,bb2k4
  call ShowScreenAndWait
/*
  ld ix,aka
  call ShowScreenAndWait

  ld ix,s3
  call ShowScreenAndWait

  ld ix,aka
  call ShowScreenAndWait

  ld ix,sp8
  call ShowScreenAndWait

  ld ix,aka
  call ShowScreenAndWait

  ld ix,cv3
  call ShowScreenAndWait

  ld ix,aka
  call ShowScreenAndWait
*/
  ld ix,bbr
  call ShowScreenAndWait

  ; difficulty select

  ld ix,DifficultySelect
  call ShowScreen

  ld c,2 ; current difficulty

LevelSelect:
  ld a,c
  and 3
  ld c,a
  push bc
    call HighlightDifficulty
    call WaitForVBlankNoInt
    call UpdatePalette
  pop bc

  call WaitForInput
  cp %00000001
  jr nz,+
  ; up
  dec c
+:cp %00000010
  jr nz,+
  ; down
  inc c
+:cp %00010000 ; button 1
  jr z,StartGame

  jr LevelSelect


StartGame:
  ld a,c
  ld (Difficulty),a
  call FadePaletteOut
  call TurnOffScreen

  ret

WaitForInput:
  ; wait for a button to be pressed and released
  ; return inverted code in a
-:in a,($dc)
  cpl
  and %00010011 ; ---1--DU
  jr z,-
  ld b,a
  ; wait for release
-:in a,($dc)
  cpl
  and %00010011 ; ---1--DU
  jr nz,-
  ld a,b
  ret


HighlightDifficulty:
  ; a = difficulty (0-3)
  ; palette 2,3,4,5 shows it in red
  ld ix,ActualPalette+2
  ; colours are at indices 12,13,14,15 in the sprite palette
  ; in the order LDUR

  cp 0 ; is L pressed?
  jr nz,+
  ld (ix+0),cl300
  jr ++
+:ld (ix+0),cl333
++:
  cp 1 ; is D pressed?
  jr nz,+
  ld (ix+1),cl300
  jr ++
+:ld (ix+1),cl333
++:

  cp 2 ; is U pressed?
  jr nz,+
  ld (ix+2),cl300
  jr ++
+:ld (ix+2),cl333
++:

  cp 3 ; is R pressed?
  jr nz,+
  ld (ix+3),cl300
  jr ++
+:ld (ix+3),cl333
++:
  ret

FadePaletteIn:
  ld a,0
  ld (FadeDirection),a
  jp FadePalette

FadePaletteOut:
  ld a,1
  ld (FadeDirection),a
  jp FadePalette

TurnOnScreen:
  ; turn off screen
  ld a,%11000100
;        |||| |`- Zoomed sprites -> 16x16 pixels
;        |||| `-- Doubled sprites -> 2 tiles per sprite, 8x16
;        |||`---- 30 row/240 line mode
;        ||`----- 28 row/224 line mode
;        |`------ VBlank interrupts
;        `------- Enable display
  out ($bf),a
  ld a,$81
  out ($bf),a
  ret


; Palette fader from 2002
; TODO: make it use the colourful fader from Phantasy Star?

PaletteFadeLookup:
.db 0,0,0,0
.db 0,0,1,1
.db 0,1,1,2
.db 0,1,2,3

AdjustColour:   ; pass colour in a, amount<<2 in b; returns adjusted colour in a, b unaffected
    or b            ; now a=amount:colour, which is the index to PaletteFadeLookup
    ld d,0
    ld e,a
    ld hl,PaletteFadeLookup
    add hl,de
    ld a,(hl)     ; now a=adjusted amount
    ret

FadePalette:
    ; Load ActualPalette with palette
    ; Set FadeDirection to 1 to fade to black, 0 to fade from black

    ; Initial multiplication value:
    ld b,%0000  ; Zero, unless FadeDirection!=0
    ld a,(FadeDirection)
    cp 0
    jp z,+
    ld b,%1000
    +:

    PaletteFadeLoop:
    ; Copy palette using lookup to fade colours in
        ld c,16 ; 16 palette entries to process
        ld ix,ActualPalette ; original palette and offset to new one
        AdjustColourAtIX:
            push bc
            ld a,(ix+0)     ; red
            and %00000011
            call AdjustColour
            ld c,a
            ld a,(ix+0)     ; green
            srl a           ; >>2
            srl a
            and %00000011
            call AdjustColour
            sla a
            sla a
            or c
            ld c,a
            ld a,(ix+0)     ; blue
            srl a           ; >>4
            srl a
            srl a
            srl a
            and %00000011
            call AdjustColour
            sla a
            sla a
            sla a
            sla a
            or c
            ld c,a
            ; a is now the colour I want
            ld (ix+$10),a   ; write to fading palette
            inc ix
            pop bc
            dec c
            jr nz,AdjustColourAtIX

        ; Full palette fade done in RAM, now load it
        ld hl,ActualPalette+$10
        push bc
            ld b,16
            ld c,0
            call LoadPaletteOld

;            ld a,%11000000  ; Turn screen on
;            out ($bf),a
;            ld a,$81
;            out ($bf),a

            ld c,15
            call WaitForCFrames
        pop bc
        ; Are we fading in or out?
        ld a,(FadeDirection)
        cp 0
        jp z,+
        ; FadeToBlack=1
        ; so decrement b and jump if >=0
        ld a,b
        sub %100
        ld b,a
        jp p,PaletteFadeLoop   ; if it's >0 then repeat
        jp ++
        +:
        ; FadeToBlack=0
        ; so inceremnt b and jump if !=%1000
        ld a,b
        add a,%100
        ld b,a
        cp %10000   ; if it's not 4<<2 then repeat
        jp nz,PaletteFadeLoop
        ++:
    ret
.ends

.section "Song finished" free
SongFinished:

  di

  call PSGStop

  ; Turn screen off
  ld a,%10100101
;        |||| |`- Zoomed sprites -> 16x16 pixels
;        |||| `-- Doubled sprites -> 2 tiles per sprite, 8x16
;        |||`---- 30 row/240 line mode
;        ||`----- 28 row/224 line mode
;        |`------ VBlank interrupts
;        `------- Enable display
  out ($bf),a
  ld a,$81
  out ($bf),a
  
  jp 0 ; reset game :P


.ends

.section "Wait for c frames" FREE
WaitForCFrames: ; Waits for c frames
    push af
    push bc
        _DecLoop:
            _Loop1:
            call GetVCount
            cp $00
            jp nz,_Loop1

            _Loop2:
            call GetVCount
            cp $c1
            jp nz,_Loop2
    
            dec c
            jp nz,_DecLoop
    pop bc
    pop af
    ret
.ends

.section "Palette loader" FREE
LoadPaletteOld:
	push af
	push bc
	push hl
	    call WaitForVBlankNoInt
	    ld a,c
	    out ($bf),a     ; Palette index
	    ld a,$c0
	    out ($bf),a     ; Palette write identifier
	    ld c,$be
	    otir            ; Output b bytes starting at hl to port c
	pop hl
	pop bc
	pop af
    ret
.ends

.section "Wait for button" free
WaitForButton:
    push af
      -:in a,($dc)
        and %00010000
        cp  %00000000
        jp nz,-
        ; Button down, wait for it to come up
      -:in a,($dc)
        and %00010000
        cp  %00010000
    pop af
    ret
.ends

.section "xiao" free
xiao:
.dw +, ++, +++
+:   .incbin "backgrounds/xiao.png.tiles.zx7"
++:  .incbin "backgrounds/xiao.png.tilemap.zx7"
+++: .incbin "backgrounds/xiao.png.palette.bin"
.ends

.section "bbr" free
bbr:
.dw +, ++, +++
+:   .incbin "backgrounds/BBR.png.tiles.zx7"
++:  .incbin "backgrounds/BBR.png.tilemap.zx7"
+++: .incbin "backgrounds/BBR.png.palette.bin"
.ends

.section "music" free
ButterflyMusic:
.incbin "music/bf_rj1_edit.psg"
.ends

.section "bb2k4" free
bb2k4:
.dw +, ++, +++
+:   .incbin "backgrounds/BB2K4.png.tiles.zx7"
++:  .incbin "backgrounds/BB2K4.png.tilemap.zx7"
+++: .incbin "backgrounds/BB2K4.png.palette.bin"
.ends

.section "Difficulty select" free
DifficultySelect:
.dw +, ++, +++
+:   .incbin "backgrounds/levelselect.png.tiles.zx7"
++:  .incbin "backgrounds/levelselect.png.tilemap.zx7"
+++: ;.incbin "backgrounds/levelselect.png.palette.bin"
; Hack the palette as we tweak it for the menu selection
.db $00 $15 $3F $3F $03 $3F
.ends

