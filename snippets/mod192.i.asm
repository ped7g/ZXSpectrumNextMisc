    MODULE mod192

;--------------------------------------------------------------------------------------------
; Calculates HL = HL % 192, for uin16_t values (0..65535)
; modifies: DE, BC, F
; code length: 18 bytes, 580+T total duration (for SUB_POWER 7)
; (for [incomplete] int16_t input you can do `add hl,32640` to get -32640..+32767 as 0..65407)

SUB_POWER   EQU     7       ; 7 is max, makes the loop to start with -128*192
                            ; you can use lower number if you know your max HL will be lower

hlMod192:
    ld      de,-192<<SUB_POWER      ; 10T
    ld      bc,$0100+SUB_POWER+1    ; 10T
.loop:
    add     hl,de                   ; 11T
    jr      c,.loop                 ; 12T /  7T
    sbc     hl,de                   ; 15T
    dec     c                       ;  4T
    ret     z                       ;  5T / 11T
    bsra    de,b                    ;  8T
    ; Z80 alternative: sra d : rr e (16T)
    jp      .loop                   ; 10T

;--------------------------------------------------------------------------------------------
; for top performance if you can afford 256-byte aligned lookup table (LUT)
; input: HL (depending on the LUT: input is treated either as unsigned or signed integer)
; modifies: HL, AF
; Best case is 55T, worst case 57T
;     DEFINE MOD192LUT_SIGNED ; uncomment for signed input (result is still 0..191)
hlMod192_lut:
    ld      a,l             ; original bottom 8 bits of value to mod192
    ; because (HL%192 == (HL%768)%192) && (HL%768 is H=H%3) (hint: 4*192 = 3*256 = 768)
    ; the value H%3 describes all possible different adjustments of L to become result
    ; the lookup table contains 256 adjust-values selected by H%3 value
    ld      l,h
    ld      h,high mod192lut
    sub     (hl)            ; adjust original L by value from table
    ld      h,0
    jr      nc,.hasResult   ; A = result if CF=0
    add     a,192           ; else L belongs to previous 0..191 range (+192 will fix it)
.hasResult:
    ld      l,a             ; HL = result
    ret

;--------------------------------------------------------------------------------------------
; same as previous, but takes input in HL and returns result in A (does modify HL and flags)
; Best case is 33T, worst case 44T, average ~39T
; uncomment/comment MOD192LUT_SIGNED define in variant "A" as you wish
hlMod192_lut_B:
    ld      a,l             ; original bottom 8 bits of value to mod192
    ld      l,h
    ld      h,high mod192lut
    sub     (hl)            ; adjust original L by value from table
    ret     nc
    add     a,192           ; else L belongs to previous 0..191 range (+192 will fix it)
    ret

    ALIGN 256
mod192lut:
    IFDEF MOD192LUT_SIGNED
        ; signed (manually tested)
        .(128/3)    DB 192,128,64               ; mod 3 based values for 0..127
                    DB 192,128
                    DB 128,64                   ; mod 3 based values for -128..-1
        .(128/3)    DB 192,128,64
    ELSE
        ; unsigned (mod192.test.i.asm expects unsigned case)
        .(256/3)    DB 192,128,64
                    DB 192
    ENDIF
    ASSERT 256 == $-mod192lut

    ENDMODULE
