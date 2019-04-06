; hard-wired test harness for trying out routines drawing the progress bars
; to switch between modes, comment/uncomment all marked lines and change the screenBlock

;         CSPECT_BREAK
        ld      sp,charsetCopy+$300         ; put stack into this memory page
        ; switch gfx mode through regular "load screen block" handler code
        ld      a,1*8
        ld      (nexHeader.HIRESCOL),a      ; blue/yellow colour for HiRes mode
        ld      a,2
        ld      (nexHeader.BORDERCOL),a     ; red border in any case
        ld      a,201
        ld      (fread),a                   ; inhibit real fread
;         call    .debugClsHiCol              ; comment out for other modes
        ; 0 = L2, 1 = ULA, 2 = LoRes, 3 = HiRes, 4 = HiCol
        ld      hl,screenBlocksDefs + 0*SCREEN_BLOCK_DEF    ; set here the desired mode
        call    LoadScreenBlock
        xor     a                           ; go from "0 banks" to full NEXLOAD_MAX_BANK
.progressBankNumLoop:
        ; clear bottom two lines in Layer2  ; other modes are left to redraw themselves..
        ld      hl,$FD00
        ld      de,$FD01
        ld      (hl),l
        ld      bc,$300-1
        ldir
        ; setup progress bar
        ld      (nexHeader.NUMBANKS),a
        ld      b,a
        inc     b
        push    af
;         and     $0f                        ; uncomment for LoRes
        or      $80                        ; uncomment for Layer2
        ld      (nexHeader.LOADBARCOL),a    ; for ULA modes this doesn't affect anything
        ld      l,$10                       ; starting x-coordinate is 16
        ld      (drawProgressBar.x),hl
        ld      hl,-225                     ; current "D"
        ld      (drawProgressBar.D),hl
        call    setupProgressBar
        ; call progress A+1 times
.progressLoop:
        push    bc
        call    drawProgressBar
        pop     bc
        ei
        halt
        di
        djnz    .progressLoop
        pop     af
        inc     a
        cp      NEXLOAD_MAX_BANK+1
        jr      c,.progressBankNumLoop

        jr      $

.debugClsHiCol:             ; red paper, white ink for HiCol mode
        ld      hl,$6000
        ld      de,$6001
        ld      bc,$1800-1
        ld      (hl),$17
        ldir
        ret
