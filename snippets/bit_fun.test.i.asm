    MODULE bit_fun

test:
        ld      b,0             ; value to test and loop through
.loop:
        ld      a,b
        call    nibrrca_z80n    ; call my variant of the routine
        ld      e,a             ; preserve result in E
        ld      a,b
        call    twitter_post    ; call original twitter post
        cp      e
        jr      nz,.error       ; one of the two doesn't work correctly, if the result differs
        djnz    .loop           ; do all 256 possible inputs
        ret
.error:
        ; return signalling error
        nextreg TILEMAP_DEFAULT_ATTR_NR_6C,2
        ret

    ENDMODULE
