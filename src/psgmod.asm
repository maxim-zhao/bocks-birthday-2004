.define NoGG

;--------------------------
; PSGMOD_LoadModule(A, HL)
PSGMOD_LoadModule:
  ld bc, ($FFFF)
  push bc

  ld (PSGMOD_FRAME), a
  ld ($FFFF), a

  ld (PSGMOD_ADDRESS), hl

  call PSGMOD_Stop

  ld c, (hl)
  inc hl
  ld b, (hl)
  inc hl
  ld (PSGMOD_LENGTH), bc
  ld (PSGMOD_LENGTH_CNT), bc

  ld c, (hl)
  inc hl
  ld b, (hl)
  inc hl
  ld (PSGMOD_NUM_SEQ), bc

  ld de, PSGMOD_INSTRUMENTS
  ld bc, 7*16
  ldir

  ld c, (hl)
  inc hl
  ld b, (hl)
  inc hl
  ld (PSGMOD_REPEAT_POINT), bc

  ; Sequenzlistenpointer setzen
  ld (PSGMOD_SEQ_LIST_START_PTR), hl
  ld (PSGMOD_SEQ_LIST_PTR), hl
  
  ;-------------;
  ; Modul-Reset ;
  ;-------------;
  ;
  ; Startsequenzpointer setzen
  ;
  ;ld hl, (PSGMOD_SEQ_LIST_START_PTR) s.o.
  call PSGMOD_START_SEQUENCES
  ;
  ;
  ;
  
  call PSGMOD_ResetPlayer
  
  pop bc
  ld  a, c
  ld  ($FFFF), a
  ret

;-----------------------
PSGMOD_ResetPlayer:
  xor a
  ld (PSGMOD_SPEED_CNT), a
  
  ; Instrumente resetten
  ld hl, PSGMOD_INSTRUMENTS
  ld (PSGMOD_INSTR_PTR), hl
  ld hl, PSGMOD_INSTRUMENTS + 7 * 4
  ld (PSGMOD_INSTR_PTR+2), hl
  ld hl, PSGMOD_INSTRUMENTS + 7 * 4 * 2
  ld (PSGMOD_INSTR_PTR+4), hl
  ld hl, PSGMOD_INSTRUMENTS + 7 * 4 * 3
  ld (PSGMOD_INSTR_PTR+6), hl
  
  ; Geschwindigkeit resetten
  ld a, 6 << 3
  ld (PSGMOD_SPEED), a
  
  ; Sequenzwartewerte resetten
  ld hl, $0101
  ld (PSGMOD_SEQ_WAIT), hl
  ld (PSGMOD_SEQ_WAIT+2), hl
  
  ; Andere Abspielvariablen resetten
  ld a, 3
  ld hl, PSGMOD_PHASE
  ld (hl), a
  inc hl
  ld (hl), a
  inc hl
  ld (hl), a
  inc hl
  ld (hl), a
  
  ld hl, 0
  ld (PSGMOD_PHASE_VOLUME), hl
  ld (PSGMOD_PHASE_VOLUME+2), hl
  ld (PSGMOD_PHASE_VOLUME_ADD), hl
  ld (PSGMOD_PHASE_VOLUME_ADD+2), hl
  ld (PSGMOD_VOLUME), hl
  ld (PSGMOD_VOLUME+2), hl
  ld (PSGMOD_VIBRATO), hl
  ld (PSGMOD_FREQUENCY), hl
  ld (PSGMOD_FREQUENCY+2), hl
  ld (PSGMOD_FREQUENCY+4), hl
  xor a
  ld (PSGMOD_FREQUENCY+6), a
  ld (PSGMOD_VIBRATO+2), a
  ld (PSGMOD_VIBRATO_CNT), a
  dec hl
  ld (PSGMOD_PHASE_DELAY), hl
  ld (PSGMOD_PHASE_DELAY+2), hl
  ret
  
;---------------
; PSGMOD_Stop()
PSGMOD_Stop:
  xor a
  ld (PSGMOD_PLAYING), a
  ld (PSGMOD_SFX_2_PRIORITY), a
  ld (PSGMOD_SFX_3_PRIORITY), a
  
  ; Lautstärken
  ld a, 15 | 144
  ld c, $7F
  out (c), a
  add a, 32
  out (c), a
  add a, 32
  out (c), a
  add a, 32
  out (c), a
  ret

;----------------
; PSGMOD_Start()
PSGMOD_Start:
  ld a, 1
  ld (PSGMOD_PLAYING), a

.ifndef NoGG
  ; GG-Stereo
  ld a, %11111111
  out ($06), a
.endif
  ret

;---------------
; PSGMOD_Play()
PSGMOD_Play:
  ld a, (PSGMOD_PLAYING)
  or a
  ret Z
  
  ld c, $7F
  
;-------------------------------------------------------
  
  ;
  ; Soundeffekte
  ;
  ld a, (PSGMOD_SFX_2_PRIORITY)
  or a
  jr Z, PSGMOD_Play_NoSFX_2
  
  ld hl, PSGMOD_SFX_2_CNT
  ld a, (hl)
  sub 1                           ; nicht dec, wegen C-Flag
  ld (hl), a
  ;cp -1
  jr NC, PSGMOD_Play_NoSFX_2_End
  
  xor a
  ld (PSGMOD_SFX_2_PRIORITY), a
  
  ;
  ; Frequenz zurücksetzen
  ;
  ld a, (PSGMOD_VIBRATO_CNT)
  or a
  ld hl, (PSGMOD_FREQUENCY+4)
  jr Z, PSGMOD_Play_ResetFreq2_NoVib
  ld d, 0
  ld a, (PSGMOD_VIBRATO + 2)
  ld e, a
  add hl, de
PSGMOD_Play_ResetFreq2_NoVib:
  
  ld a, l
  and 15
  or 128+64
  out (c), a

  ld a, l
  srl a
  srl a
  srl a
  srl a
  ld b, a
  ld a, h
  and 3
  rrca
  rrca
  rrca
  rrca
  or b
  out (c), a
  ;
  ;
  ;
  jr PSGMOD_Play_NoSFX_2
PSGMOD_Play_NoSFX_2_End:
  
  ld hl, (PSGMOD_SFX_2_ADDRESS)
 ; ld b, 03
 ; otir
  outi
  outi
  outi
  ld (PSGMOD_SFX_2_ADDRESS), hl
  
PSGMOD_Play_NoSFX_2:
  
  ld a, (PSGMOD_SFX_3_PRIORITY)
  or a
  jr Z, PSGMOD_Play_NoSFX_3
  
  ld hl, PSGMOD_SFX_3_CNT
  ld a, (hl)
  sub 1                           ; nicht dec, wegen C-Flag
  ld (hl), a
  ;cp -1
  jr NC, PSGMOD_Play_NoSFX_3_End
  
  xor a
  ld (PSGMOD_SFX_3_PRIORITY), a
  ld a, (PSGMOD_FREQUENCY + 6)
  out (c), a
  jr PSGMOD_Play_NoSFX_3
  
PSGMOD_Play_NoSFX_3_End:
  ld hl, (PSGMOD_SFX_3_ADDRESS)
  ld a, (hl)
  inc hl
  or a
  jr Z, PSGMOD_Play_SFX_3_
  ld a, (hl)
  inc hl
  out (c), a
  PSGMOD_Play_SFX_3_:
  ld a, (hl)
  inc hl
  out (c), a
  ld (PSGMOD_SFX_3_ADDRESS), hl
  
PSGMOD_Play_NoSFX_3:
  
  ld a, (PSGMOD_FRAME)
  ld ($FFFF), a
;-------------------------------------------------------
  
  ;
  ; Lautstärken
  ;
  ; Kanal 0
  ld a, (PSGMOD_VOLUME)
  ld b, a
  ld a, (PSGMOD_PHASE_VOLUME)
  srl a
  srl a
  srl a
  srl a
  add a, b
  sub 15
  jr NC, PSGMOD_Play_Vol0
  xor a
PSGMOD_Play_Vol0:
  cpl
  and 15
  or 144
  out (c), a
  
  ; Kanal 1
  ld a, (PSGMOD_VOLUME+1)
  ld b, a
  ld a, (PSGMOD_PHASE_VOLUME+1)
  srl a
  srl a
  srl a
  srl a
  add a, b
  sub 15
  jr NC, PSGMOD_Play_Vol1
  xor a
PSGMOD_Play_Vol1:
  cpl
  and 15
  or 144+32
  out (c), a
  
  ; Kanal 2
  ld a, (PSGMOD_SFX_2_PRIORITY)
  or a
  jr NZ, PSGMOD_Play_Vol2_N
  
  ld a, (PSGMOD_VOLUME+2)
  ld b, a
  ld a, (PSGMOD_PHASE_VOLUME+2)
  srl a
  srl a
  srl a
  srl a
  add a, b
  sub 15
  jr NC, PSGMOD_Play_Vol2
  xor a
PSGMOD_Play_Vol2:
  cpl
  and 15
  or 144+64
  out (c), a
PSGMOD_Play_Vol2_N:
  
  ; Kanal 3
  ld a, (PSGMOD_SFX_3_PRIORITY)
  or a
  jr NZ, PSGMOD_Play_Vol3_N
  
  ld a, (PSGMOD_VOLUME+3)
  ld b, a
  ld a, (PSGMOD_PHASE_VOLUME+3)
  srl a
  srl a
  srl a
  srl a
  add a, b
  sub 15
  jr NC, PSGMOD_Play_Vol3
  xor a
PSGMOD_Play_Vol3:
  cpl
  and 15
  or 144+96
  out (c), a
PSGMOD_Play_Vol3_N:
  
  ;
  ; Frequenzen
  ;
  
  ld a, (PSGMOD_VIBRATO_CNT)
  or a
  jr Z, PSGMOD_Play_V
  
  ; Kanal 0 mit Vibrato
  ld hl, (PSGMOD_FREQUENCY)
  ld d, 0
  ld a, (PSGMOD_VIBRATO)
  ld e, a
  add hl, de

  ld a, l
  and 15
  or 128
  out (c), a
  
  ld a, l
  srl a
  srl a
  srl a
  srl a
  ld b, a
  ld a, h
  and 3
  rrca
  rrca
  rrca
  rrca
  or b
  out (c), a
  
  ; Kanal 1 mit Vibrato
  ld hl, (PSGMOD_FREQUENCY+2)
  ld d, 0
  ld a, (PSGMOD_VIBRATO+1)
  ld e, a
  add hl, de
  
  ld a, l
  and 15
  or 128+32
  out (c), a
  
  ld a, l
  srl a
  srl a
  srl a
  srl a
  ld b, a
  ld a, h
  and 3
  rrca
  rrca
  rrca
  rrca
  or b
  out (c), a
  
  ; Kanal 2 mit Vibrato
  ld a, (PSGMOD_SFX_2_PRIORITY)
  or a
  jp NZ, PSGMOD_Play_V2
  
  ld hl, (PSGMOD_FREQUENCY + 4)
  ld d, 0
  ld a, (PSGMOD_VIBRATO+2)
  ld e, a
  add hl, de
  
  ld a, l
  and 15
  or 128+64
  out (c), a
  
  ld a, l
  srl a
  srl a
  srl a
  srl a
  ld b, a
  ld a, h
  and 3
  rrca
  rrca
  rrca
  rrca
  or b
  out (c), a
  
  jr PSGMOD_Play_V2
PSGMOD_Play_V:
  
  ; Kanal 0 ohne Vibrato
  ld hl, (PSGMOD_FREQUENCY)
  
  ld a, l
  and 15
  or 128
  out (c), a
  
  ld a, l
  srl a
  srl a
  srl a
  srl a
  ld b, a
  ld a, h
  and 3
  rrca
  rrca
  rrca
  rrca
  or b
  out (c), a
  
  ; Kanal 1 ohne Vibrato
  ld hl, (PSGMOD_FREQUENCY+2)
  
  ld a, l
  and 15
  or 128+32
  out (c), a
  
  ld a, l
  srl a
  srl a
  srl a
  srl a
  ld b, a
  ld a, h
  and 3
  rrca
  rrca
  rrca
  rrca
  or b
  out (c), a
  
  ; Kanal 2 ohne Vibrato
  ld a, (PSGMOD_SFX_2_PRIORITY)
  or a
  jr NZ, PSGMOD_Play_V2

  ld hl, (PSGMOD_FREQUENCY+4)
  
  ld a, l
  and 15
  or 128+64
  out (c), a
  
  ld a, l
  srl a
  srl a
  srl a
  srl a
  ld b, a
  ld a, h
  and 3
  rrca
  rrca
  rrca
  rrca
  or b
  out (c), a
  
PSGMOD_Play_V2:
  
  ; Kanal 3 Frequenz
  ;ld a, (PSGMOD_FREQUENCY + 6)
  ;out (c), a
  
;-------------------------------------------------------
  
  ;
  ; Instrumente
  ;
  
.MACRO PSGMOD_PLAY_INSTRUMENT
  ld a, (PSGMOD_PHASE+\1)
  cp 3
  jr Z, PSGMOD_Play_PhasesDone\1
  
  ld a, (PSGMOD_PHASE_VOLUME+\1)
  ld hl, PSGMOD_PHASE_VOLUME_ADD+\1
  add a, (hl)
  ld (PSGMOD_PHASE_VOLUME+\1), a

  ld hl, PSGMOD_PHASE_DELAY+\1
  dec (hl)
  jr NZ, PSGMOD_Play_PhasesDone\1
  
  ld hl, PSGMOD_PHASE+\1
  inc (hl)
  
  ld h, PSGMOD_INSTRUMENTS >> 8
  ld a, (PSGMOD_INSTR_PTR+\1)
  ld l, a
  ld a, (hl)
  ld (PSGMOD_PHASE_VOLUME_ADD+\1), a
  inc l
  ld a, (hl)
  ld (PSGMOD_PHASE_DELAY+\1), a
  inc l
  ld a, l
  ld (PSGMOD_INSTR_PTR+\1), a
PSGMOD_Play_PhasesDone\1:
.ENDM
  
  ; Kanal 0
  PSGMOD_PLAY_INSTRUMENT 0
  ; Kanal 1
  PSGMOD_PLAY_INSTRUMENT 1
  ; Kanal 2
  ld a, (PSGMOD_SFX_2_PRIORITY)
  or a
  jr NZ, PSGMOD_Play_CPSFX2
  PSGMOD_PLAY_INSTRUMENT 2
  PSGMOD_Play_CPSFX2:
  ; Kanal 3
  ld a, (PSGMOD_SFX_3_PRIORITY)
  or a
  jr NZ, PSGMOD_Play_CPSFX3
  PSGMOD_PLAY_INSTRUMENT 3
  PSGMOD_Play_CPSFX3:
  
;-------------------------------------------------------
  
  ;
  ; Nächste Zeile
  ;

  ld a, (PSGMOD_SPEED_CNT)
  sub 8
  jp NC, PSGMOD_Play_NoNextLine
  ld (PSGMOD_SPEED_CNT), a
  
  ld a, (PSGMOD_VIBRATO_CNT)
  xor 1
  ld (PSGMOD_VIBRATO_CNT), a
  
;-------------------------------------------------------

.MACRO _PSGMOD_PLAY_CHANNEL
PSGMOD_Play_Ch\1:
  ld hl, PSGMOD_SEQ_WAIT+\1
  dec (hl)
  jp NZ, PSGMOD_Play_Ch\1_Done
  
PSGMOD_Play_ExecCommand\1:
  ld hl, (PSGMOD_SEQ_PTR+\2)
  ld a, (hl)
  inc hl
  ld (PSGMOD_SEQ_PTR+\2), hl
  
  ld b, a
  and $E0
  jr Z, PSGMOD_Play_Effect\1
  
  rlca
  rlca
  rlca
  and 7
  ld (PSGMOD_SEQ_WAIT+\1), a
  
  ld hl, (PSGMOD_NOTES_PTR+\2)
  ld a, b
  and 31
  add a, a
  ld e, a
  ld d, 0
  add hl, de
  ld e, (hl)
  inc hl
  ld d, (hl)

  .IF \1 == 3
    ld a, e
    ld (PSGMOD_FREQUENCY+\2), a
    
    ld a, (PSGMOD_SFX_3_PRIORITY)
    or a
    jr NZ, PSGMOD_Play_CH3_CSF
    
    ld c, $7F
    out (c), e
  PSGMOD_Play_CH3_CSF:
    
    ; Volume
    ld a, d
    and 15
    ld (PSGMOD_VOLUME+\1), a
    
    ; Instrument
    ld a, d
    and $C0
    rlca
    rlca
    and 3
    add a, 4 * \1
  .ELSE
    ; Frequenz
    ld a, e
    ld (PSGMOD_FREQUENCY+\2), a
    ld a, d
    and 3
    ld (PSGMOD_FREQUENCY+1+\2), a
    
    ; Volume
    ld a, d
    srl a
    srl a
    and 15
    ld (PSGMOD_VOLUME+\1), a
    
    ; Instrument
    ld a, d
    and $C0
    rlca
    rlca
    and 3
    add a, 4 * \1
  .ENDIF
  
  ; * 7
  ld b, a
  add a, a
  add a, a
  add a, a
  sub b
  
  ld h, PSGMOD_INSTRUMENTS >> 8
  ld l, a
  ld a, (hl)
  ld (PSGMOD_PHASE_VOLUME+\1), a
  inc l
  ld a, (hl)
  ld (PSGMOD_PHASE_VOLUME_ADD+\1), a
  inc l
  ld a, (hl)
  ld (PSGMOD_PHASE_DELAY+\1), a
  inc l
  
  ld a, l
  ld (PSGMOD_INSTR_PTR+\1), a
  
  xor a
  ld (PSGMOD_PHASE+\1), a
  
  jr PSGMOD_Play_Ch\1_Done
  
PSGMOD_Play_Effect\1:
  ld a, b
  and 16
  jr NZ, PSGMOD_Play_Effect\1_SetVibrato
  
PSGMOD_Play_Effect\1_Wait:
  ld a, b
  and 15
  ld b, a
  ld a, (PSGMOD_SEQ_WAIT+\1)
  add a, b
  ld (PSGMOD_SEQ_WAIT+\1), a
  jr PSGMOD_Play_Ch\1_Done
  
PSGMOD_Play_Effect\1_SetVibrato:
  
  .IF \1 == 3
    ld hl, (PSGMOD_SEQ_PTR+\2)
    ld a, (hl)
    inc hl
    ld (PSGMOD_SEQ_PTR+\2), hl
    
    ld (PSGMOD_SPEED), a
  .ELSE
    ld a, b
    and 15
    ld (PSGMOD_VIBRATO+\1), a
  .ENDIF
  jp PSGMOD_Play_ExecCommand\1
  
;-------------------------------------------------------
PSGMOD_Play_Ch\1_Done:
.ENDM
  
  ; Kanal 0
  _PSGMOD_PLAY_CHANNEL 0, 0
  ; Kanal 1
  _PSGMOD_PLAY_CHANNEL 1, 2
  ; Kanal 2
  _PSGMOD_PLAY_CHANNEL 2, 4
  ; Kanal 3
  _PSGMOD_PLAY_CHANNEL 3, 6
  
;-------------------------------------------------------
;-------------------------------------------------------
  
  ld hl, PSGMOD_SPEED
  ld a, (PSGMOD_SPEED_CNT)
  add a, (hl)
  ld (PSGMOD_SPEED_CNT), a
  
;-------------------------------------------------------
  ;
  ; Nächste Sequenz
  ;
  ld hl, PSGMOD_SEQ_LENGTH_CNT
  dec (hl)
  ret NZ

  ld hl, (PSGMOD_LENGTH_CNT)
  dec hl
  ld (PSGMOD_LENGTH_CNT), hl
  ld a, l
  or h
  jr NZ, PSGMOD_Play_NoRestart
  
  ; Restart/Repeat
  ld hl, (PSGMOD_REPEAT_POINT)
  add hl, hl
  add hl, hl
  add hl, hl
  ld de, (PSGMOD_SEQ_LIST_START_PTR)
  add hl, de
  ld (PSGMOD_SEQ_LIST_PTR), hl
  
  ld hl, (PSGMOD_LENGTH)
  ld de, (PSGMOD_REPEAT_POINT)
  or a ; CLCF
  sbc hl, de
  ld (PSGMOD_LENGTH_CNT), hl
  
  jr PSGMOD_Play_Restart
  
PSGMOD_Play_NoRestart:
  ld hl, (PSGMOD_SEQ_LIST_PTR)
  ld de, 8
  add hl, de
  ld (PSGMOD_SEQ_LIST_PTR), hl
  jp PSGMOD_START_SEQUENCES
  
/*
PSGMOD_Play_Restart:
  ld hl, (PSGMOD_SEQ_LIST_PTR)
  jp PSGMOD_START_SEQUENCES
  ;ret
*/

; Modified: play once, return on finish
PSGMOD_Play_Restart:
  ld hl, (PSGMOD_SEQ_LIST_PTR)
  call PSGMOD_START_SEQUENCES
  jp SongFinished

PSGMOD_Play_NoNextLine:
  ld (PSGMOD_SPEED_CNT), a
  ret

PSGMOD_START_SEQUENCES: ; (HL)
  ld e, (hl)
  inc hl
  ld d, (hl)
  inc hl
  push hl
  ld h, d
  ld l, e
  ld de, (PSGMOD_ADDRESS)
  add hl, de
  ld d, h
  ld e, l
  ld a, (de)
  ld l, a
  inc de
  ld a, (de)
  ld h, a
  inc de
  ld bc, (PSGMOD_ADDRESS)
  add hl, bc
  ld a, l
  ld (PSGMOD_NOTES_PTR), a
  ld a, h
  ld (PSGMOD_NOTES_PTR+1), a
  ld a, (de)
  inc de
  ld (PSGMOD_SEQ_LENGTH_CNT), a ; nur einmal
  ld (PSGMOD_SEQ_PTR), de
  pop hl
  
  ld e, (hl)
  inc hl
  ld d, (hl)
  inc hl
  push hl
  ld h, d
  ld l, e
  ld de, (PSGMOD_ADDRESS)
  add hl, de
  ld d, h
  ld e, l
  ld a, (de)
  ld l, a
  inc de
  ld a, (de)
  ld h, a
  inc de
  ld bc, (PSGMOD_ADDRESS)
  add hl, bc
  ld a, l
  ld (PSGMOD_NOTES_PTR+2), a
  ld a, h
  ld (PSGMOD_NOTES_PTR+3), a
  ld a, (de)
  inc de
  ld (PSGMOD_SEQ_PTR+2), de
  pop hl

  ld e, (hl)
  inc hl
  ld d, (hl)
  inc hl
  push hl
  ld h, d
  ld l, e
  ld de, (PSGMOD_ADDRESS)
  add hl, de
  ld d, h
  ld e, l
  ld a, (de)
  ld l, a
  inc de
  ld a, (de)
  ld h, a
  inc de
  ld bc, (PSGMOD_ADDRESS)
  add hl, bc
  ld a, l
  ld (PSGMOD_NOTES_PTR+4), a
  ld a, h
  ld (PSGMOD_NOTES_PTR+5), a
  ld a, (de)
  inc de
  ld (PSGMOD_SEQ_PTR+4), de
  pop hl
  
  ld e, (hl)
  inc hl
  ld d, (hl)
  inc hl
  push hl
  ld h, d
  ld l, e
  ld de, (PSGMOD_ADDRESS)
  add hl, de
  ld d, h
  ld e, l
  ld a, (de)
  ld l, a
  inc de
  ld a, (de)
  ld h, a
  inc de
  ld bc, (PSGMOD_ADDRESS)
  add hl, bc
  ld a, l
  ld (PSGMOD_NOTES_PTR+6), a
  ld a, h
  ld (PSGMOD_NOTES_PTR+7), a
  ld a, (de)
  inc de
  ld (PSGMOD_SEQ_PTR+6), de
  pop hl
  ret
  
;----------------------------------------------------------
; PSGMOD_PlaySFX_2(B = length, C = priority, HL = address)
PSGMOD_PlaySFX_2:
  ld a, (PSGMOD_SFX_2_PRIORITY)
  cp c
  jr Z, PSGMOD_PlaySFX_2_OK
  ret NC
PSGMOD_PlaySFX_2_OK:
  
  ld a, b
  ld (PSGMOD_SFX_2_CNT), a
  ld a, c
  ld (PSGMOD_SFX_2_PRIORITY), a
  ld (PSGMOD_SFX_2_ADDRESS), hl
  ret

;----------------------------------------------------------
; PSGMOD_PlaySFX_3(B = length, C = priority, HL = address)
PSGMOD_PlaySFX_3:
  ld a, (PSGMOD_SFX_3_PRIORITY)
  cp c
  jr Z, PSGMOD_PlaySFX_3_OK
  ret NC
PSGMOD_PlaySFX_3_OK:
  
  ld a, b
  ld (PSGMOD_SFX_3_CNT), a
  ld a, c
  ld (PSGMOD_SFX_3_PRIORITY), a
  ld (PSGMOD_SFX_3_ADDRESS), hl
  ret

;----------------------------------------------------------
; PSGMOD_PlaySFX_23(B = length, C = priority, HL = address)
PSGMOD_PlaySFX_23:
  ld a, (PSGMOD_SFX_3_PRIORITY)
  cp c
  jr Z, PSGMOD_PlaySFX_23_OK_3
  ret NC
PSGMOD_PlaySFX_23_OK_3:
  
  ld a, (PSGMOD_SFX_2_PRIORITY)
  cp c
  jr Z, PSGMOD_PlaySFX_23_OK_2
  ret NC
PSGMOD_PlaySFX_23_OK_2:
  
  ld a, b
  ld (PSGMOD_SFX_2_CNT), a
  ld (PSGMOD_SFX_3_CNT), a
  ld a, c
  ld (PSGMOD_SFX_2_PRIORITY), a
  ld (PSGMOD_SFX_3_PRIORITY), a
  
  ld (PSGMOD_SFX_2_ADDRESS), hl
  ld c, b
  ld b, 0
  add hl, bc
  add hl, bc
  add hl, bc
  ld (PSGMOD_SFX_3_ADDRESS), hl
  ret

;------------------
; PSGMOD_StopSFX()
PSGMOD_StopSFX:
  ld c, $7F
  
  ld a, (PSGMOD_SFX_2_PRIORITY)
  or a
  jr Z, PSGMOD_StopSFX_2
  
  xor a
  ld (PSGMOD_SFX_2_PRIORITY), a

  ld a, (PSGMOD_VIBRATO_CNT)
  or a
  ld hl, (PSGMOD_FREQUENCY+4)
  jr Z, PSGMOD_StopSFX_ResetFreq2_NoVib
  ld d, 0
  ld a, (PSGMOD_VIBRATO + 2)
  ld e, a
  add hl, de
PSGMOD_StopSFX_ResetFreq2_NoVib:
  
  ld a, l
  and 15
  or 128+64
  out (c), a
  
  ld a, l
  srl a
  srl a
  srl a
  srl a
  ld b, a
  ld a, h
  and 3
  rrca
  rrca
  rrca
  rrca
  or b
  out (c), a
  ;
  ;
  ;
PSGMOD_StopSFX_2:
  
  ld a, (PSGMOD_SFX_3_PRIORITY)
  or a
  ret Z
  
  xor a
  ld (PSGMOD_SFX_3_PRIORITY), a
  
  ld a, (PSGMOD_FREQUENCY + 6)
  out (c), a
  ret
