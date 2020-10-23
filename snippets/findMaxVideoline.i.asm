; returns in A the max-videoline-LSB, for example A=0x37 for ZX128 mode at 50Hz
; => there are lines 0..0x137 => total lines is 312 (0x138)
; modifies: AF, BC, HL
; This algorithm works correctly only for modes with 258+ total lines (MSB=1, LSB=1+)
; which is currently (core 3.1.5) true for all VGA/HDMI modes in any variant
findMaxVideoline:
    ld      h,VIDEO_LINE_LSB_NR_1F
    ; wait for videoline LSB in 1..127 (128+ is 100% MSB=0, zero is not max in any mode)
.waitForPositiveLsbWithMsb:
    ld      bc,TBBLUE_REGISTER_SELECT_P_243B
    out     (c),h       ; select videoline LSB
    inc     b
.waitForPositiveLsb:
    in      l,(c)
    jp      m,.waitForPositiveLsb
    jr      z,.waitForPositiveLsb
    ; now check if MSB=1 (if not, restart the whole sequence from beginning)
    dec     b
    dec     h
    out     (c),h       ; select videoline MSB
    inc     h
    inc     b
    in      a,(c)
    rra
    jr      nc,.waitForPositiveLsbWithMsb
    ; now the L=LSB is 1..127 with MSB=1, just keep collecting LSB until it does reach zero
    dec     b
    out     (c),h       ; select videoline LSB
    inc     b
.collectMaxLsb:
    ld      a,l         ; A = current max LSB
    in      l,(c)
    jr      nz,.collectMaxLsb   ; until it wraps around to videoline 0
    ret
