; Rigorous tests for file "comparisons.i.asm" (has to be included right after it)

.report_error:      ; could do anything, but must preserve flags for chained tests
    ; report error by changing palette offset to "red" variant (+2)
    nextreg TILEMAP_DEFAULT_ATTR_NR_6C,2
    ret

.test_runner_call_iy:
    jp      (iy)
    ret

.test_runner_8bit_loop:
    ld      a,c         ; mask+compare resulting flags
    and     b
    cp      (ix+3)
    call    nz,.report_error
    ld      bc,4
    add     ix,bc
.test_runner_8bit:      ; loop entry point
    ld      a,(ix+0)
    ld      b,(ix+1)
    call    .test_runner_call_iy
    push    af
    cp      (ix+0)
    call    nz,.report_error    ; A did change
    ld      a,b
    cp      (ix+1)
    call    nz,.report_error    ; B did change
    pop     bc          ; C=result_flags
    ld      b,(ix+2)    ; B=flags_mask == 0 -> end of data
    inc     b
    djnz    .test_runner_8bit_loop
    ret

.test_runner_16bit_loop:
    ld      a,c         ; mask+compare resulting flags
    and     b
    cp      (ix+5)
    call    nz,.report_error
    ld      bc,6
    add     ix,bc
.test_runner_16bit:     ; loop entry point
    ld      hl,(ix+0)   ; fake: ld l,(ix+0) : ld h,(ix+1)
    ld      bc,(ix+2)   ; fake: ld c,(ix+2) : ld b,(ix+3)
    call    .test_runner_call_iy
    push    af
    ld      de,(ix+0)   ; fake: ld e,(ix+0) : ld d,(ix+1)
    sub     hl,de       ; fake: or a : sbc hl,de
    call    nz,.report_error    ; HL did change
    ld      hl,(ix+2)   ; fake: ld l,(ix+2) : ld h,(ix+3)
    sub     hl,bc       ; fake: or a : sbc hl,bc
    call    nz,.report_error    ; BC did change
    pop     bc          ; C=result_flags
    ld      b,(ix+4)    ; B=flags_mask == 0 -> end of data
    inc     b
    djnz    .test_runner_16bit_loop
    ret

.flag_s     EQU $80
.flag_z     EQU $40
.flag_h     EQU $10
.flag_pv    EQU $04
.flag_n     EQU $02
.flag_c     EQU $01
.flag_nz    EQU 0
.flag_nc    EQU 0
.flag_p     EQU 0
.flag_m     EQU .flag_s
.flag_po    EQU 0
.flag_pe    EQU .flag_pv

.uint8_test_data:   ; val1, val2, flags_mask, flags_check
    DB      $00, $00, .flag_z|.flag_c, .flag_z|.flag_nc
    DB      $7F, $7F, .flag_z|.flag_c, .flag_z|.flag_nc
    DB      $80, $80, .flag_z|.flag_c, .flag_z|.flag_nc
    DB      $FF, $FF, .flag_z|.flag_c, .flag_z|.flag_nc
    DB      $00, $01, .flag_z|.flag_c, .flag_nz|.flag_c
    DB      $00, $80, .flag_z|.flag_c, .flag_nz|.flag_c
    DB      $00, $FF, .flag_z|.flag_c, .flag_nz|.flag_c
    DB      $7F, $80, .flag_z|.flag_c, .flag_nz|.flag_c
    DB      $7F, $FF, .flag_z|.flag_c, .flag_nz|.flag_c
    DB      $80, $FF, .flag_z|.flag_c, .flag_nz|.flag_c
    DB      $01, $00, .flag_z|.flag_c, .flag_nz|.flag_nc
    DB      $80, $00, .flag_z|.flag_c, .flag_nz|.flag_nc
    DB      $FF, $00, .flag_z|.flag_c, .flag_nz|.flag_nc
    DB      $80, $7F, .flag_z|.flag_c, .flag_nz|.flag_nc
    DB      $FF, $7F, .flag_z|.flag_c, .flag_nz|.flag_nc
    DB      $FF, $80, .flag_z|.flag_c, .flag_nz|.flag_nc
    DB      0, 0, 0 ; EOD

.int8_test_data:    ; val1, val2, flags_mask, flags_check
    DB      $00, $00, .flag_z|.flag_pv|.flag_s, .flag_z|.flag_po|.flag_p
    DB      $7F, $7F, .flag_z|.flag_pv|.flag_s, .flag_z|.flag_po|.flag_p
    DB      $80, $80, .flag_z|.flag_pv|.flag_s, .flag_z|.flag_po|.flag_p
    DB      $FF, $FF, .flag_z|.flag_pv|.flag_s, .flag_z|.flag_po|.flag_p
    DB      -64, +64, .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_po|.flag_m   ; -128
    DB      -64, +65, .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_pe|.flag_p   ; -129 (overflow)
    DB      +64, -63, .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_po|.flag_p   ; +127
    DB      +64, -64, .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_pe|.flag_m   ; +128 (overflow)
    DB      $00, $01, .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_po|.flag_m
    DB      $00, $81, .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_po|.flag_p
    DB      $00, $80, .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_pe|.flag_m   ; 0 vs -128 overflows
    DB      $00, $FF, .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_po|.flag_p
    DB      $7F, $80, .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_pe|.flag_m
    DB      $7F, $FF, .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_pe|.flag_m
    DB      $80, $FF, .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_po|.flag_m
    DB      $01, $00, .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_po|.flag_p
    DB      $81, $00, .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_po|.flag_m
    DB      $80, $00, .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_po|.flag_m   ; -128 vs 0 is ok
    DB      $FF, $00, .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_po|.flag_m
    DB      $80, $7F, .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_pe|.flag_p
    DB      $FF, $7F, .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_po|.flag_m
    DB      $FF, $80, .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_po|.flag_p
    DB      0, 0, 0 ; EOD

.uint16_test_data:  ; val1, val2, flags_mask, flags_check
    ; equal values
    DW $0000, $0000 : DB .flag_z|.flag_c, .flag_z|.flag_nc
    DW $0001, $0001 : DB .flag_z|.flag_c, .flag_z|.flag_nc
    DW $00FF, $00FF : DB .flag_z|.flag_c, .flag_z|.flag_nc
    DW $0100, $0100 : DB .flag_z|.flag_c, .flag_z|.flag_nc
    DW $7FFF, $7FFF : DB .flag_z|.flag_c, .flag_z|.flag_nc
    DW $8000, $8000 : DB .flag_z|.flag_c, .flag_z|.flag_nc
    DW $FF00, $FF00 : DB .flag_z|.flag_c, .flag_z|.flag_nc
    DW $FFFF, $FFFF : DB .flag_z|.flag_c, .flag_z|.flag_nc
    ; below values
    DW $0000, $0001 : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW $0000, $00FF : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW $0000, $0100 : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW $0000, $7FFF : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW $0000, $8000 : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW $0000, $FF00 : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW $0000, $FFFF : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW $0001, $00FF : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW $0001, $0100 : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW $0001, $7FFF : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW $0001, $8000 : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW $0001, $FF00 : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW $0001, $FFFF : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW $00FF, $0100 : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW $00FF, $7FFF : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW $00FF, $8000 : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW $00FF, $FF00 : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW $00FF, $FFFF : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW $0100, $7FFF : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW $0100, $8000 : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW $0100, $FF00 : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW $0100, $FFFF : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW $7FFF, $8000 : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW $7FFF, $FF00 : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW $7FFF, $FFFF : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW $8000, $FF00 : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW $8000, $FFFF : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW $FF00, $FFFF : DB .flag_z|.flag_c, .flag_nz|.flag_c
    ; above values
    DW $0001, $0000 : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW $00FF, $0000 : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW $0100, $0000 : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW $7FFF, $0000 : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW $8000, $0000 : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW $FF00, $0000 : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW $FFFF, $0000 : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW $00FF, $0001 : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW $0100, $0001 : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW $7FFF, $0001 : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW $8000, $0001 : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW $FF00, $0001 : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW $FFFF, $0001 : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW $0100, $00FF : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW $7FFF, $00FF : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW $8000, $00FF : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW $FF00, $00FF : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW $FFFF, $00FF : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW $7FFF, $0100 : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW $8000, $0100 : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW $FF00, $0100 : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW $FFFF, $0100 : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW $8000, $7FFF : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW $FF00, $7FFF : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW $FFFF, $7FFF : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW $FF00, $8000 : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW $FFFF, $8000 : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW $FFFF, $FF00 : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW 0, 0 : DB 0  ; EOD

.int16_test_data:   ; val1, val2, flags_mask, flags_check
    ; equal values
    DW  $0000,  $0000 : DB .flag_z|.flag_pv|.flag_s, .flag_z|.flag_po|.flag_p
    DW  $0001,  $0001 : DB .flag_z|.flag_pv|.flag_s, .flag_z|.flag_po|.flag_p
    DW  $00FF,  $00FF : DB .flag_z|.flag_pv|.flag_s, .flag_z|.flag_po|.flag_p
    DW  $0100,  $0100 : DB .flag_z|.flag_pv|.flag_s, .flag_z|.flag_po|.flag_p
    DW  $7FFF,  $7FFF : DB .flag_z|.flag_pv|.flag_s, .flag_z|.flag_po|.flag_p
    DW  $8000,  $8000 : DB .flag_z|.flag_pv|.flag_s, .flag_z|.flag_po|.flag_p
    DW  $FF00,  $FF00 : DB .flag_z|.flag_pv|.flag_s, .flag_z|.flag_po|.flag_p
    DW  $FFFF,  $FFFF : DB .flag_z|.flag_pv|.flag_s, .flag_z|.flag_po|.flag_p
    ; less values
    DW -$4000, +$4000 : DB .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_po|.flag_m  ; -32768
    DW -$4000, +$4001 : DB .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_pe|.flag_p  ; -32769 (overflow)
    DW  $0100,  $0101 : DB .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_po|.flag_m
    DW  $0100,  $7F00 : DB .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_po|.flag_m
    DW  $0100,  $7FFF : DB .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_po|.flag_m
    DW  $0001,  $0100 : DB .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_po|.flag_m
    DW  $0001,  $7F00 : DB .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_po|.flag_m
    DW  $0000,  $0001 : DB .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_po|.flag_m
    DW  $0000,  $0100 : DB .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_po|.flag_m
    DW  $FFFF,  $0000 : DB .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_po|.flag_m
    DW  $FFFF,  $7FFF : DB .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_po|.flag_m
    DW  $8001,  $0000 : DB .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_po|.flag_m
    DW  $8000,  $FFFF : DB .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_po|.flag_m
    DW  $8000,  $0000 : DB .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_po|.flag_m  ; -32768 vs 0: no overflow
    DW  $8000,  $7FFF : DB .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_pe|.flag_p
    ; greater values
    DW +$4000, -$3FFF : DB .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_po|.flag_p  ; +32767
    DW +$4000, -$4000 : DB .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_pe|.flag_m  ; +32768 (overflow)
    DW  $0101,  $0100 : DB .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_po|.flag_p
    DW  $7F00,  $0100 : DB .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_po|.flag_p
    DW  $7FFF,  $0100 : DB .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_po|.flag_p
    DW  $0100,  $0001 : DB .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_po|.flag_p
    DW  $7F00,  $0001 : DB .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_po|.flag_p
    DW  $0001,  $0000 : DB .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_po|.flag_p
    DW  $0100,  $0000 : DB .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_po|.flag_p
    DW  $0000,  $FFFF : DB .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_po|.flag_p
    DW  $7FFF,  $FFFF : DB .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_pe|.flag_m
    DW  $0000,  $8001 : DB .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_po|.flag_p
    DW  $FFFF,  $8000 : DB .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_po|.flag_p
    DW  $0000,  $8000 : DB .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_pe|.flag_m  ; 0 vs -32768: overflow
    DW  $7FFF,  $8000 : DB .flag_z|.flag_pv|.flag_s, .flag_nz|.flag_pe|.flag_m
    DW 0, 0 : DB 0  ; EOD

.int16_v2_test_data:    ; val1, val2, flags_mask, flags_check
    ; equal values
    DW  $0000,  $0000 : DB .flag_z|.flag_c, .flag_z|.flag_nc
    DW  $0001,  $0001 : DB .flag_z|.flag_c, .flag_z|.flag_nc
    DW  $00FF,  $00FF : DB .flag_z|.flag_c, .flag_z|.flag_nc
    DW  $0100,  $0100 : DB .flag_z|.flag_c, .flag_z|.flag_nc
    DW  $7FFF,  $7FFF : DB .flag_z|.flag_c, .flag_z|.flag_nc
    DW  $8000,  $8000 : DB .flag_z|.flag_c, .flag_z|.flag_nc
    DW  $FF00,  $FF00 : DB .flag_z|.flag_c, .flag_z|.flag_nc
    DW  $FFFF,  $FFFF : DB .flag_z|.flag_c, .flag_z|.flag_nc
    ; less values
    DW -$4000, +$4000 : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW -$4000, +$4001 : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW  $0100,  $0101 : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW  $0100,  $7F00 : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW  $0100,  $7FFF : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW  $0001,  $0100 : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW  $0001,  $7F00 : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW  $0000,  $0001 : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW  $0000,  $0100 : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW  $FFFF,  $0000 : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW  $FFFF,  $7FFF : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW  $8001,  $0000 : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW  $8000,  $FFFF : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW  $8000,  $0000 : DB .flag_z|.flag_c, .flag_nz|.flag_c
    DW  $8000,  $7FFF : DB .flag_z|.flag_c, .flag_nz|.flag_c
    ; greater values
    DW +$4000, -$3FFF : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW +$4000, -$4000 : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW  $0101,  $0100 : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW  $7F00,  $0100 : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW  $7FFF,  $0100 : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW  $0100,  $0001 : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW  $7F00,  $0001 : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW  $0001,  $0000 : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW  $0100,  $0000 : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW  $0000,  $FFFF : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW  $7FFF,  $FFFF : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW  $0000,  $8001 : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW  $FFFF,  $8000 : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW  $0000,  $8000 : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW  $7FFF,  $8000 : DB .flag_z|.flag_c, .flag_nz|.flag_nc
    DW 0, 0 : DB 0  ; EOD

.test:
    ld      ix,.uint8_test_data
    ld      iy,.uint8
    call    .test_runner_8bit

    ld      ix,.int8_test_data
    ld      iy,.int8
    call    .test_runner_8bit

    ld      ix,.uint16_test_data
    ld      iy,.uint16
    call    .test_runner_16bit

    ld      ix,.int16_test_data
    ld      iy,.int16
    call    .test_runner_16bit

    ld      ix,.int16_v2_test_data
    ld      iy,.int16_v2
    call    .test_runner_16bit

    ret
