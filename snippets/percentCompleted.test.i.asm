    MODULE percentCompleted

reportError:
    nextreg TILEMAP_DEFAULT_ATTR_NR_6C,2
    ret

test:
;     OPT --zxnext=cspect : break     ;FIXME DEBUG
    call    test.handpickedValues
    call    test.total100           ; 0..100 out of 100 (result == value)
    ld      bc,17<<8                ; usage similar to one real world case, total 1700
    call    test.fullRangeForD
    ret

test.total100:      ; trying all HL=0..100 out of 100 (ie. A == HL)
    ld      de,0
    ld      hl,100
.loop:
    push    hl,de
    call    calc
    pop     de,hl
    cp      e
    call    nz,reportError
    inc     e
    cp      l
    jr      c,.loop
    ret

test.fullRangeForD:     ; input: B = size of 1% (so total will become 100 * B exactly), C = 0
    ld      d,100
    ld      e,b
    mul     de
    ex      de,hl       ; HL = total = 100 * B
    ld      de,0        ; DE starts at zero
.nextPerc:
    push    bc          ; B for how many loops, C expected result
.samePerc:
    push    hl,de,bc
    call    calc
    pop     bc,de,hl
    cp      c
    call    nz,reportError
    inc     de
    djnz    .samePerc
    pop     bc          ; reset loop counter B
    inc     c           ; next +1%
    ld      a,c
    cp      100
    jr      c,.nextPerc ; C < 100, run regular loop
    ; 100 expected, so DE should be equal to HL here, do one more iteration to check for 100 result
    dec     b
    ret     z           ; already was 1 iteration
    ; assert hl == de here, CF=0 here
    sbc     hl,de
    call    nz,reportError
    add     hl,de
    ld      b,1         ; last final iteration
    jr      .nextPerc

test.handpickedValues:
    ld      hl,.data
.loop:
    ldi     de,(hl)     ; fake ; ld e,(hl) : inc hl : ld d,(hl) : inc hl ; fetch TOTAL (will become HL)
    push    de
    ldi     de,(hl)     ; fake ; ld c,(hl) : inc hl : ld b,(hl) : inc hl ; fetch VALUE to DE
    ex      (sp),hl     ; preserve data ptr, HL = TOTAL
    call    calc
    pop     hl
    cpi
    call    nz,reportError
    ld      a,low .data.end
    cp      l
    jr      nz,.loop
    ret
.data:
    MACRO   percentTestData value?, total?
        ASSERT 0 <= (total?) && (total?) <= 0x7FFF
        ASSERT 0 <= (value?) && (value?) <= (total?)
        DW      (total?), (value?)
        IF 0 < (total?)
            DB      (100 * (value?)) / (total?)
        ELSE
            DB      100
        ENDIF
    ENDM
    percentTestData 0, 0        ; 100%
    percentTestData 0, 1        ; 0%
    percentTestData 1, 1        ; 100%
    percentTestData 3, 4        ; 75%
    percentTestData 1000, 4000  ; 25%
    percentTestData 1000, 4001  ; 24%
    percentTestData 1000, 4002  ; 24%
    percentTestData 1000, 4003  ; 24%
    percentTestData 1000, 4004  ; 24%
    percentTestData 1001, 4000  ; 25%
    percentTestData 1001, 4001  ; 25%
    percentTestData 1001, 4002  ; 25%
    percentTestData 1001, 4003  ; 25%
    percentTestData 1001, 4004  ; 25%
    percentTestData 1001, 4005  ; 24%
    percentTestData 327, $7FFF  ; max total 0%
    percentTestData 328, $7FFF  ; max total 1%
    percentTestData 21626, $7FFF; max total 65%
    percentTestData 21627, $7FFF; max total 66%
    percentTestData $7FFF, $7FFF; max total 100%
.data.end

    ENDMODULE
