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
    ld      de,.data_16x8.itemSize
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
    ld      de,.data_24x8.itemSize
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
    jp      nz,.error
    ld      a,(ix+7)
    sub     l
    jp      nz,.error
    ld      a,(ix+8)
    sub     h
    jp      nz,.error
    ld      a,(ix+9)
    sub     e
    jp      nz,.error
    ld      a,(ix+10)
    sub     d
    jp      nz,.error
    ld      de,.data_32x8.itemSize
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
    jp      nz,.error
    ld      a,(ix+7)
    sub     l
    jp      nz,.error
    ld      a,(ix+8)
    sub     h
    jp      nz,.error
    ld      a,(ix+9)
    sub     e
    jp      nz,.error
    ld      a,(ix+10)
    sub     d
    jp      nz,.error
    ld      de,.data_32x8.itemSize
    add     ix,de
.l3_b+1: ld b,0         ; loop counter (SMC code)
    djnz    .l3

    ; mul 16x16_16 tests
    ld      ix,.data_16x16
    ld      b,.data_16x16.count
.l4:
    ld      l,(ix+0)            ; HL (16b arg1)
    ld      h,(ix+1)
    ld      c,(ix+2)            ; DC (16b arg2)
    ld      d,(ix+3)
    call    mul_16_16_16_AE ; HL * DC = AE
    sub     (ix+5)
    jr      nz,.error
    ld      a,(ix+4)
    sub     e
    jr      nz,.error
    ld      de,.data_16x16.itemSize
    add     ix,de
    djnz    .l4

    ; mul 16x16_32 tests
    ld      ix,.data_16x16
    ld      b,.data_16x16.count
.l5:
    push    bc
    ld      c,(ix+0)            ; BC (16b arg1)
    ld      b,(ix+1)
    ld      l,(ix+2)            ; HL (16b arg2)
    ld      h,(ix+3)
    call    mul_16_16_32_DELC   ; HL * BC = DELA
    ld      a,c
    pop     bc
    sub     (ix+4)
    jr      nz,.error
    ld      a,(ix+5)
    sub     l
    jr      nz,.error
    ld      a,(ix+6)
    sub     e
    jr      nz,.error
    ld      a,(ix+7)
    sub     d
    jr      nz,.error
    ld      de,.data_16x16.itemSize
    add     ix,de
    djnz    .l5

    ret                 ; test finished

.error:
    ; return signalling error
    nextreg TILEMAP_DEFAULT_ATTR_NR_6C,2
    ret                 ; test finished

.data_16x8:
    ; A * EL = HLE
    db $00 : dw $0000 : d24 $000000
.data_16x8.itemSize: equ $-.data_16x8
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
.data_24x8.itemSize: equ $-.data_24x8
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
.data_24x8.count:  equ  ($-.data_24x8)/.data_24x8.itemSize

.data_32x8:
    ;    D +  C *       EHLB =    EHLB of result   D of result
    db $00, $00 : dd $00000000 : dd $00000000 : db $00
.data_32x8.itemSize: equ $-.data_32x8
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
.data_32x8.count:   equ  ($-.data_32x8)/.data_32x8.itemSize

.data_16x16:    ; (used by routines with different result size from 16 to 32)
    dw $0000, $0000 : dd {$-4}*{$-2}
.data_16x16.itemSize: equ $-.data_16x16
    dw $0001, $0000 : dd {$-4}*{$-2}
    dw $0307, $0000 : dd {$-4}*{$-2}
    dw $5678, $0000 : dd {$-4}*{$-2}
    dw $ABCD, $0000 : dd {$-4}*{$-2}
    dw $FFFF, $0000 : dd {$-4}*{$-2}
    dw $0000, $0001 : dd {$-4}*{$-2}
    dw $0001, $0001 : dd {$-4}*{$-2}
    dw $0307, $0001 : dd {$-4}*{$-2}
    dw $5678, $0001 : dd {$-4}*{$-2}
    dw $ABCD, $0001 : dd {$-4}*{$-2}
    dw $FFFF, $0001 : dd {$-4}*{$-2}
    dw $0000, $0205 : dd {$-4}*{$-2}
    dw $0001, $0205 : dd {$-4}*{$-2}
    dw $0307, $0205 : dd {$-4}*{$-2}
    dw $5678, $0205 : dd {$-4}*{$-2}
    dw $ABCD, $0205 : dd {$-4}*{$-2}
    dw $FFFF, $0205 : dd {$-4}*{$-2}
    dw $0000, $5432 : dd {$-4}*{$-2}
    dw $0001, $5432 : dd {$-4}*{$-2}
    dw $0307, $5432 : dd {$-4}*{$-2}
    dw $5678, $5432 : dd {$-4}*{$-2}
    dw $ABCD, $5432 : dd {$-4}*{$-2}
    dw $FFFF, $5432 : dd {$-4}*{$-2}
    dw $0000, $A987 : dd {$-4}*{$-2}
    dw $0001, $A987 : dd {$-4}*{$-2}
    dw $0307, $A987 : dd {$-4}*{$-2}
    dw $5678, $A987 : dd {$-4}*{$-2}
    dw $ABCD, $A987 : dd {$-4}*{$-2}
    dw $FFFF, $A987 : dd {$-4}*{$-2}
    dw $0000, $FFFF : dd {$-4}*{$-2}
    dw $0001, $FFFF : dd {$-4}*{$-2}
    dw $0307, $FFFF : dd {$-4}*{$-2}
    dw $5678, $FFFF : dd {$-4}*{$-2}
    dw $ABCD, $FFFF : dd {$-4}*{$-2}
    dw $FFFF, $FFFF : dd {$-4}*{$-2}
.data_16x16.count:  equ  ($-.data_16x16)/.data_16x16.itemSize
    ENDMODULE
