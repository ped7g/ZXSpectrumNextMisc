    MODULE mod40

test:
    ld      hl,0        ; value to "mod 40"
    ld      de,0        ; expected result
.loop:
    push    hl

    call    hlMod40

    pop     hl
    cp      e
    jr      z,.resultIsOk
    ; return signalling error
    nextreg TILEMAP_DEFAULT_ATTR_NR_6C,2
    ret
.resultIsOk:
    inc     de          ; update DE to next result of mod40
    ld      a,-40
    add     a,e
    jr      nc,.deIsOk
    ld      e,a
.deIsOk
    inc     hl          ; update HL for next test
    ld      a,h
    or      l
    jr      nz,.loop    ; keep testing full 0..65535 range
    ret                 ; test finished

    ENDMODULE
