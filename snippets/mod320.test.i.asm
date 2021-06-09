    MODULE mod320

test:
    ld      hl,0        ; value to "mod 320"
    ld      de,0        ; expected result
.loop:
    push    hl
    push    de

    ;call    hlMod320

    ;call    hlMod320_unrolled

    ex      de,hl       ; LUT variant takes input in DE and returns result in DE
    call    deMod320_lut
    ex      de,hl

    pop     de          ; compare result with preserved DE
    or      a
    sbc     hl,de
    jr      z,.hlIsOk
    ; return signalling error
    pop     hl
    nextreg TILEMAP_DEFAULT_ATTR_NR_6C,2
    ret
.hlIsOk:
    inc     de          ; update DE to next result of mod320
    ld      hl,-320
    add     hl,de
    jr      nc,.deIsOk
    ex      de,hl
.deIsOk
    pop     hl          ; update HL for next test
    inc     hl
    ld      a,h
    or      l
    jr      nz,.loop    ; keep testing full 0..65535 range
    ret                 ; test finished

    ENDMODULE
