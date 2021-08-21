; examples of some fun with individual bits
; (at this moment only one `nibrrca` doing rrca-like rotate, but over two nibbles in register A)

    MODULE bit_fun

nibrrca_z80n:       ; ZX Next variant using Z80N TEST * instruction
        ; 7654'3210 -> 4765'0321 (bits 7654 and 3210 do RRCA-like rotation independently)
        rrca        ; works the same way as nibrrca, but exploits TEST instruction of Z80N
        test    $88 ; TEST does set same flags as AND, but discards result (preserves original A)
        ret     pe  ; so if the two bits have same value (P/V=1), no swap is needed, return
        xor     $88 ; otherwise "swap" them by flipping both bits to correct value
        ret         ; 8 bytes, 37T/26T

nibrrca:            ; Z80-only variant
        ; 7654'3210 -> 4765'0321 (7654 and 3210 do RRCA-like rotation independently)
        rrca        ; do regular RRCA, which makes all bits except b3/b7 final, b3/b7 are misplaced (swapped)
        ld      c,a
        and     $88 ; keep only b3 and b7, to set parity flag. 00 and 11 sets P/V=1 (PE), 01 and 10 sets P/V=0 (PO)
        ld      a,c ; restore the final value with misplaced bits
        ret     pe  ; but if P/V=1, then they are equal, so they don't need swap
        xor     $88 ; if P/V=0, they differ, and indeed are swapped. Using xor $88 will correct both of them.
        ret         ; 9 bytes, 41/30T

; original twitteer post making me curious about this topic
; https://twitter.com/nath_mojon/status/1429016130933379079
; posted by @nath_mojon, rotating right "nibbles"
; (code is modified to use D instead of B to have DJNZ available for test)
twitter_post:
        ld      d,a     ; was b,a
        rra
        and     $77
        ld      c,a
        ld      a,d     ; was a,b
        rla
        rla
        rla
        and     $88
        or      c
        ret             ; 13 bytes, 56T

    ENDMODULE
