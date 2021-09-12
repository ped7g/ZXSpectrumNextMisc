    MODULE mul

test:
    ; mul 16x8 tests
    ld      ix,.data_16x8
.l0:
    ld      a,ixl
    cp      low .data_16x8.e
    jr      z,.done_16x8
    ld      a,(ix+0)
    ld      l,(ix+1)
    ld      e,(ix+2)
    call    mul_16_8_24_HLE
    ld      a,(ix+3)
    sub     e
    jp      nz,.error
    ld      a,(ix+4)
    sub     l
    jp      nz,.error
    ld      a,(ix+5)
    sub     h
    jp      nz,.error
    ld      de,6
    add     ix,de
    jr      .l0
.done_16x8:

    ; mul 24x8 tests
    ld      ix,.data_24x8
    ld      b,.data_24x8.count
.l1:
    ld      a,(ix+0)
    ld      e,(ix+1)
    ld      l,(ix+2)
    ld      h,(ix+3)
    call    mul_24_8_32_DELC    ; A * HLE = DELC
    ld      a,(ix+4)
    sub     c
    jp      nz,.error
    ld      a,(ix+5)
    sub     l
    jp      nz,.error
    ld      a,(ix+6)
    sub     e
    jp      nz,.error
    ld      a,(ix+7)
    sub     d
    jp      nz,.error
    ld      de,8
    add     ix,de
    djnz    .l1

    ; mul 32x8 tests
    ld      ix,.data_32x8
    ld      b,.data_32x8.count
.l2:
    ld      a,b
    ld      (.l2_b),a           ; preserve loop counter
    ld      d,(ix+0)            ; +D
    ld      c,(ix+1)            ; C (8b arg1)
    ld      b,(ix+2)            ; EHLB (32b arg2)
    ld      l,(ix+3)
    ld      h,(ix+4)
    ld      e,(ix+5)
    call    muladd_32_8_8_40_DEHLB  ; D + C * EHLB = DEHLB
    ld      a,(ix+6)
    sub     b
    jr      nz,.error
    ld      a,(ix+7)
    sub     l
    jr      nz,.error
    ld      a,(ix+8)
    sub     h
    jr      nz,.error
    ld      a,(ix+9)
    sub     e
    jr      nz,.error
    ld      a,(ix+10)
    sub     d
    jr      nz,.error
    ld      de,11
    add     ix,de
.l2_b+1: ld b,0         ; loop counter (SMC code)
    djnz    .l2

    ; mul 32x8 performance tests
    ld      ix,.data_32x8
    ld      b,.data_32x8.count
.l3:
    ld      a,b
    ld      (.l3_b),a           ; preserve loop counter
    ld      a,(ix+0)            ; +A
    ld      c,(ix+1)            ; C (8b arg1)
    ld      e,(ix+2)            ; HLBE (32b arg2)
    ld      b,(ix+3)
    ld      l,(ix+4)
    ld      h,(ix+5)
    call    muladd_32_8_8_40_DEHLB_perf ; A + C * HLBE = DEHLB
    ld      a,(ix+6)
    sub     b
    jr      nz,.error
    ld      a,(ix+7)
    sub     l
    jr      nz,.error
    ld      a,(ix+8)
    sub     h
    jr      nz,.error
    ld      a,(ix+9)
    sub     e
    jr      nz,.error
    ld      a,(ix+10)
    sub     d
    jr      nz,.error
    ld      de,11
    add     ix,de
.l3_b+1: ld b,0         ; loop counter (SMC code)
    djnz    .l3

    ret                 ; test finished

.error:
    ; return signalling error
    nextreg TILEMAP_DEFAULT_ATTR_NR_6C,2
    ret                 ; test finished

.data_16x8:
    ; A * EL = HLE
    db $00 : dw $0000 : d24 $000000
    db $01 : dw $0000 : d24 $000000
    db $83 : dw $0000 : d24 $000000
    db $FF : dw $0000 : d24 $000000
    db $00 : dw $0001 : d24 $000000
    db $01 : dw $0001 : d24 $000001
    db $83 : dw $0001 : d24 $000083
    db $FF : dw $0001 : d24 $0000FF
    db $00 : dw $0100 : d24 $000000
    db $01 : dw $0100 : d24 $000100
    db $83 : dw $0100 : d24 $008300
    db $FF : dw $0100 : d24 $00FF00
    db $00 : dw $11FF : d24 $000000
    db $01 : dw $11FF : d24 $0011FF
    db $83 : dw $11FF : d24 $09357D
    db $FF : dw $11FF : d24 $11ED01
    db $00 : dw $C1C1 : d24 $000000
    db $01 : dw $C1C1 : d24 $00C1C1
    db $83 : dw $C1C1 : d24 $6325C3
    db $FF : dw $C1C1 : d24 $C0FF3F
    db $00 : dw $DEDE : d24 $000000
    db $01 : dw $DEDE : d24 $00DEDE
    db $83 : dw $DEDE : d24 $720B9A
    db $FF : dw $DEDE : d24 $DDFF22
    db $FF : dw $FFFF : d24 $FEFF01
.data_16x8.e:           ; end of test data for 16x8 mul
    ASSERT .data_16x8.e-.data_16x8 < 256    ; because how the test checks for end of data

.data_24x8:
    ; A * HLE = DELC
    db $00 : d24 $000000 : dd $00000000
    db $01 : d24 $000000 : dd $00000000
    db $83 : d24 $000000 : dd $00000000
    db $FF : d24 $000000 : dd $00000000
    db $00 : d24 $000001 : dd $00000000
    db $01 : d24 $000001 : dd $00000001
    db $83 : d24 $000001 : dd $00000083
    db $FF : d24 $000001 : dd $000000FF
    db $00 : d24 $000100 : dd $00000000
    db $01 : d24 $000100 : dd $00000100
    db $83 : d24 $000100 : dd $00008300
    db $FF : d24 $000100 : dd $0000FF00
    db $00 : d24 $010000 : dd $00000000
    db $01 : d24 $010000 : dd $00010000
    db $83 : d24 $010000 : dd $00830000
    db $FF : d24 $010000 : dd $00FF0000
    db $00 : d24 $11FF11 : dd $00000000
    db $01 : d24 $11FF11 : dd $0011FF11
    db $83 : d24 $11FF11 : dd $093585B3
    db $FF : d24 $11FF11 : dd $11ED11EF
    db $00 : d24 $3579BD : dd $00000000
    db $01 : d24 $3579BD : dd $003579BD
    db $83 : d24 $3579BD : dd $1B5D4BB7
    db $FF : d24 $3579BD : dd $35444343
    db $00 : d24 $FF11FF : dd $00000000
    db $01 : d24 $FF11FF : dd $00FF11FF
    db $83 : d24 $FF11FF : dd $8286357D
    db $FF : d24 $FF11FF : dd $FE12ED01
    db $FF : d24 $FFFFFF : dd $FEFFFF01
.data_24x8.count:  equ  ($-.data_24x8)/8

.data_32x8:
    ;    D +  C *       EHLB =    EHLB of result   D of result
    db $00, $00 : dd $00000000 : dd $00000000 : db $00
    db $0A, $00 : dd $00000000 : dd $0000000A : db $00
    db $A0, $00 : dd $00000000 : dd $000000A0 : db $00
    db $00, $01 : dd $00000000 : dd $00000000 : db $00
    db $0A, $01 : dd $00000000 : dd $0000000A : db $00
    db $A0, $01 : dd $00000000 : dd $000000A0 : db $00
    db $00, $55 : dd $00000000 : dd $00000000 : db $00
    db $0A, $55 : dd $00000000 : dd $0000000A : db $00
    db $A0, $55 : dd $00000000 : dd $000000A0 : db $00
    db $00, $AA : dd $00000000 : dd $00000000 : db $00
    db $0A, $AA : dd $00000000 : dd $0000000A : db $00
    db $A0, $AA : dd $00000000 : dd $000000A0 : db $00
    db $00, $FF : dd $00000000 : dd $00000000 : db $00
    db $0A, $FF : dd $00000000 : dd $0000000A : db $00
    db $A0, $FF : dd $00000000 : dd $000000A0 : db $00
    db $00, $00 : dd $13579BDF : dd $00000000 : db $00
    db $0A, $00 : dd $13579BDF : dd $0000000A : db $00
    db $A0, $00 : dd $13579BDF : dd $000000A0 : db $00
    db $00, $01 : dd $13579BDF : dd $13579BDF : db $00
    db $0A, $01 : dd $13579BDF : dd $13579BE9 : db $00
    db $A0, $01 : dd $13579BDF : dd $13579C7F : db $00
    db $00, $55 : dd $13579BDF : dd $6C16C10B : db $06
    db $0A, $55 : dd $13579BDF : dd $6C16C115 : db $06
    db $A0, $55 : dd $13579BDF : dd $6C16C1AB : db $06
    db $00, $AA : dd $13579BDF : dd $D82D8216 : db $0C
    db $0A, $AA : dd $13579BDF : dd $D82D8220 : db $0C
    db $A0, $AA : dd $13579BDF : dd $D82D82B6 : db $0C
    db $00, $FF : dd $13579BDF : dd $44444321 : db $13
    db $0A, $FF : dd $13579BDF : dd $4444432B : db $13
    db $A0, $FF : dd $13579BDF : dd $444443C1 : db $13
    db $00, $00 : dd $FFFFFFFF : dd $00000000 : db $00
    db $0A, $00 : dd $FFFFFFFF : dd $0000000A : db $00
    db $A0, $00 : dd $FFFFFFFF : dd $000000A0 : db $00
    db $00, $01 : dd $FFFFFFFF : dd $FFFFFFFF : db $00
    db $0A, $01 : dd $FFFFFFFF : dd $00000009 : db $01
    db $A0, $01 : dd $FFFFFFFF : dd $0000009F : db $01
    db $00, $55 : dd $FFFFFFFF : dd $FFFFFFAB : db $54
    db $0A, $55 : dd $FFFFFFFF : dd $FFFFFFB5 : db $54
    db $A0, $55 : dd $FFFFFFFF : dd $0000004B : db $55
    db $00, $AA : dd $FFFFFFFF : dd $FFFFFF56 : db $A9
    db $0A, $AA : dd $FFFFFFFF : dd $FFFFFF60 : db $A9
    db $A0, $AA : dd $FFFFFFFF : dd $FFFFFFF6 : db $A9
    db $00, $FF : dd $FFFFFFFF : dd $FFFFFF01 : db $FE
    db $0A, $FF : dd $FFFFFFFF : dd $FFFFFF0B : db $FE
    db $A0, $FF : dd $FFFFFFFF : dd $FFFFFFA1 : db $FE
    db $FF, $FF : dd $FFFFFFFF : dd $00000000 : db $FF
.data_32x8.count:  equ  ($-.data_32x8)/8

    ENDMODULE
