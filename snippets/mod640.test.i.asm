    MODULE mod640

test:
    ld      hl,0        ; value to "mod 640"
    ld      de,0        ; expected result
.loop:
    push    hl
    push    de

    call    hlMod640_lut
    ex      de,hl       ; result is in DE

    pop     de          ; compare result with preserved DE
    or      a
    sbc     hl,de
    jr      z,.hlIsOk
    ; return signalling error
    pop     hl
    nextreg TILEMAP_DEFAULT_ATTR_NR_6C,2
    ret
.hlIsOk:
    inc     de          ; update DE to next result of mod640
    ld      hl,-640
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
