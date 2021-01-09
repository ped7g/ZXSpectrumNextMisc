; returns ZF=1 when Z80N is detected, otherwise ZF=0
; modifies: AF always, H (!) on regular Z80
; could modify other registers/state on extended Z80 CPUs using ED24 for something else
detectZ80N:
    ld      a,$80
    DB $ED, $24, $00, $00   ; mirror a : nop : nop
    ; written as DB to be compilable also in Z80 mode or by non-Next assemblers
    ; the four byte ED240000 should be quite safe to execute at any Z80-like CPU
    ; but only Z80N will produce value $01 into A register (by MIRROR A instruction)
    dec     a
    ret     nz          ; NOT Z80N
    ; Z80N detected
    ret

    ; if you think you can optimize/shorten this, consider effects of your changes
    ; on non-Z80 and non-Z80N CPUs (this should be quite optimal test while keeping
    ; it at this level of robustness, optimizations may make it more fragile)
