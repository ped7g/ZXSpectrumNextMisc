; returns in HL the video-lines count for current mode (ie. 0x138 for VGA ZX128)
; modifies: AF, BC, HL
; This algorithm works correctly only for modes with 258..511 video lines
; current core 3.1.5 conforms to this for all VGA/HDMI modes in any variant (min 261?)
findVLinesCount:
    ld      bc,TBBLUE_REGISTER_SELECT_P_243B
    ld      hl,$0100 + VIDEO_LINE_LSB_NR_1F ; H = 1, L = VIDEO_LINE_LSB_NR_1F
    out     (c),l
    inc     b
.waitForNon255MaxLsb:
    ld      a,255       ; non-255 max not found yet
.waitForZeroLsb:
    ld      l,a
    in      a,(c)       ; L = last non-zero LSB, A = fresh LSB (may be zero upon wrap)
    jr      nz,.waitForZeroLsb
    inc     l           ; check if L is non-255 when line LSB wraps to 0 => max LSB found
    jr      z,.waitForNon255MaxLsb
    ret                 ; here HL is then equal to lines count (H=1 already, L=line.LSB+1)
