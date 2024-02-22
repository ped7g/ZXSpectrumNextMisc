;=====================================================================================
; ********* does NOT WORK as expected *******
;=====================================================================================
; explained by AA (ZX Next FPGA core maintainer):
; The copper is always running at 28 MHz.  The cpu change happens when the cpu control signals
; are idle :
; - on the rising edge of the cpu clock as seen in z80 timing diagrams with m1, mreq and iorq all high.
; The cpu speed is not changed until the cpu is in the idle state.  That's to avoid runt pulses
; in the timing.  But even after the cpu speed is change, the selection is through a hardware
; module BUFGMUX_1 which is a mux selecting the appropriate clock.  This is a special piece
; of hardware that makes sure the clock is changed in a way that runt pulses don't happen
; so there could be a cycle or two at the slower clock speed before that allows a change too.
; * So the change can happen on any rising edge of the cpu's clock but usually not
; in the middle of memory or i/o cycles.
; If the cpu is at 28 MHz, the BUFGMUX might postpone changing to a 3.5 MHz clock until the 3.5 MHz
; clock goes low then high.  And that's a contended clock so there are other reasons for that clock
; to be delayed.  IIRC the contended clock is held high while contended.
;
;=====================================================================================
; Ped7g: so what happens with this prototype - the short 8px spans of CPU throttled to 7 or 14MHz
; becomes usually longer than 8px spans (looks it's statistically easier to go from 3.5MHz
; to 7/14MHz than to go down to 3.5MHz from 7/14MHz), and instead of boosting 216T scanline to
; correct 224T, the timing does fluctuate and does average higher than 224T per line, making
; this impossible as general band-aid for HDMI mode vs general multicolor titles.
;
; There is hypothetical possibility to tune this precisely against robust and stable engine with
; some margins in required precisions, like maybe nirvana+ engine, customising every copper line
; boost to fit the running code, but it's not clear/guaranteed the fluctuations will be stable
; across frames even with fixed engine code, and would require tedious manual tuning re-running
; the code on real Next with HDMI output all the time.
;
; As I don't have working HDMI display myself, I'm abandoning this project here, posting the crude
; prototype code with these comments, just in case somebody finds this interesting or even wants
; to try to tune the code for particular multicolor game.
;

;=====================================================================================
; dot7gFX "HDMI timing change" v0.1 2023-04-27, copper code to throttle CPU
; © Peter Helcmanovsky 2023, license: https://opensource.org/licenses/MIT
; requires ZX Spectrum Next with core3.1.5+
;
; Assembles with sjasmplus - https://github.com/z00m128/sjasmplus (v1.20.2+)
; The Makefile has build commands, but it should be enough to just run:
;   sjasmplus hdmit.asm --raw=hdmit.dot
;
; TODO:
; - missing lot of parts as the main thing proved to not work, abandoning this
;

;=====================================================================================
; copper.txt timings:
;
;  Table 3.1: Vertical line counts and dot clock combinations
;
;            48K VGA 50Hz   312 lines   224.0 * 4 = 896 dot clocks
;           128K VGA 50Hz   311 lines   228.0 * 4 = 912 dot clocks
;       PENTAGON VGA 50Hz   320 lines   224.0 * 4 = 896 dot clocks
;
;            48K VGA 60Hz   262 lines   224.0 * 4 = 896 dot clocks
;           128K VGA 60Hz   261 lines   228.0 * 4 = 912 dot clocks
;
;               HDMI 50Hz   312 lines   216.0 * 4 = 864 dot clocks
;               HDMI 60Hz   262 lines   214.5 * 4 = 858 dot clocks
;
;  Table 3.2: Dot clocks per second
;
;            48K VGA 50Hz   312 lines   13,977,600 clocks   14.0Mhz (28Mhz)
;           128K VGA 50Hz   311 lines   14,181,600 clocks   14.2Mhz (28Mhz)
;       PENTAGON VGA 50Hz   320 lines   14,336,000 clocks   14.3Mhz (28Mhz)
;
;            48K VGA 60Hz   262 lines   14,085,120 clocks   14.1Mhz (28Mhz) (! obsolete !)
;           128K VGA 60Hz   261 lines   14,281,920 clocks   14.3Mhz (28Mhz) (! obsolete !)
;
;               HDMI 50Hz   312 lines   13,478,400 clocks   13.5Mhz (27Mhz)
;               HDMI 60Hz   262 lines   13,487,760 clocks   13.5Mhz (27Mhz)
;
;           16 dot clocks =  8 pixels in standard 256x192 resolution
;           16 dot clocks = 16 pixels in Timex HIRES 512x192 resolution
;
; Table 3.5: Slack dot clocks after maximum line compare
;
;        858 dot clocks per line   (52 * 16 = 832)   SLACK = 26 dot clocks
;        864 dot clocks per line   (52 * 16 = 832)   SLACK = 32 dot clocks
;        896 dot clocks per line   (54 * 16 = 864)   SLACK = 32 dot clocks
;        912 dot clocks per line   (55 * 16 = 880)   SLACK = 32 dot clocks

;=====================================================================================
; core 3.1.10 VHDL timings:
;            48K VGA 50Hz   312 lines   224.0 * 4 = 896 dot clocks                  int 248:0
;           128K VGA 50Hz   311 lines   228.0 * 4 = 912 dot clocks                  128 int 248:4 / +3 int 248:2
;       PENTAGON VGA 50Hz   320 lines   224.0 * 4 = 896 dot clocks                  int 239:323
;
;            48K VGA 60Hz   264 lines   224.0 * 4 = 896 dot clocks (! line diff !)  int 224:0
;           128K VGA 60Hz   264 lines   228.0 * 4 = 912 dot clocks (! line diff !)  128 int 224:4 / +3 int 224:2
;
;               HDMI 50Hz   312 lines   216.0 * 4 = 864 dot clocks                  int 256:4
;               HDMI 60Hz   262 lines   214.5 * 4 = 858 dot clocks                  int 235:4
;

;=====================================================================================
; designing timing fix
; HDMI 50Hz -> ZX48 50Hz
; - same lines
; - each line has 216T instead of 224T = -8T
; - total frame difference 67392 - 69888 = -2496T
; HDMI 50Hz -> ZX128 50Hz
; - +1 line
; - each line has 216T instead of 228T = -12T
; - total frame difference 67392 - 70908 = -3516T
;
;=====================================================================================
; HDMI 50Hz -> ZX48 50Hz
;           3.5       3.5
; at 3.5MHz 1T -> 1T, 4T ->  4T
; at 7MHz   1T -> 2T, 4T ->  8T
; at 14MHz  1T -> 4T, 4T -> 16T (dot clock)
; at 28MHz  1T -> 8T, 4T -> 32T
; copper wait is per 8pix = 16 dot clocks (16 hires pixels), 4T in 3.5MHz
; WAIT 1 clock = 0.5dot, MOVE is 2 clocks = 1dot, NOP is 1 clock = 0.5dot
;
; to meet zx48 do two 8px strides at 7MHz, that's 2x doing 8T instead of 2x 4T = 16-8 = +8T
; at each line .. *ouch*
;
; wait line,39  ; 312px (256 + 56)
; move 7,1      ; 7MHz
; wait line,41  ; 328px (256 + 72)
; move 7,0      ; 3.5MHz
; -- 4 instructions is slot-code
; -- 1024 instructions / 4 = 256 slots
; ->missing room for 312-256 = 56 lines
;
; new slot allocation layout approach dealing with interupt as well,
; reconfiguring it to line interrupt at 248:128T (0x23 = 249 !)
; lines:
; 248..279: 32 lines, adding 12T per line, per four lines, -128T by late interrupt
;           = 32 * (216 + 12) - 128 = 7168 T vs ZX48 32 * 224 = 7168 T
;           - 4 segments at 14MHz = 16*4-4*4 = +48T (48/4 = +12T) at lines 248, 252, 256, 260, 264, 268, 272, 276
; 280..291: 12 lines, adding 8T per line, per two lines
;           = 12 * (216 + 8) = 2688 T (same as ZX48)
;           - 4 segments at 7MHz = 8*4-4*4 = +16T (16/2 = +8T) at lines 280, 282, .., 290
; 292..311: 20 lines, adding 8T every line
;           = 20 * (216 + 8) = 4480 T (same as ZX48)
;           - 2 segments at 7MHz = 8*2-4*2 = +8T at each line
;   0..211: 192+20 lines, adding 8T every line
;           = 212 * (216 + 8) = 47488 T (same as ZX48)
;           - 2 segments at 7MHz = 8*2-4*2 = +8T at each line
; 212..223: 12 lines, adding 8T per line, per two lines
;           = 12 * (216 + 8) = 2688 T (same as ZX48)
;           - 4 segments at 7MHz = 8*4-4*4 = +16T (16/2 = +8T) at lines 212, 214, .., 222
; 224..231: 8 lines, adding 8T per line, per four lines
;           = 8 * (216 + 8) = 1792 T (same as ZX48)
;           - 8 segments at 7MHz = 8*8-4*8 = +32T (32/4 = +8T) at lines 224, 228
; 232..247: 16 lines per 216T, no copper code
;           = 16 * 216 = 3456 T vs ZX48 3584 T -> 128T missing
; 248     : extra +128T until line interrupt vs ZX48 0T -> fixing total frame time 69888 T
; total copper slots: = 8+6+20+192+20+6+2 = 254
; one free slot remaining in copper = 4 NOOP to deal with that
;

;=====================================================================================
; HDMI 50Hz -> ZX128 50Hz
;
; to meet zx128 do one 8px stride at 14MHz, that's 1x doing 16T instead of 1x 4T = 16-4 = +12T
;
; wait line,39  ; 312px (256 + 56)
; move 7,2      ; 14MHz
; wait line,40  ; 320px (256 + 64)
; move 7,0      ; 3.5MHz
; -- 4 instructions is slot-code
;
; possible layout:
; 192 pixel lines, top/bottom border: 20/21 lines per 1, 12 per 2
; =192+20+21+6+6 = 245 slots (11 to go), =192+20+21+12+12 = 257 lines (55 to go in HDMI, but 54 in zx128)
; 55 * 216T = 11880T
; 54 * 228T = 12312T
;           = 432T difference
; 36 lines can use throttle to keep 228T
; 19 lines per 216T will throw away 228T to fix total lines as if there were 311 lines
; ^ this must happen before interrupt at end
; 55 lines per 5 = 11 slots
; 293 lines to throttle by +12T
;

        OPT reset --zxnext --syntax=abf
        INCLUDE "constants.i.asm"
        ORG $2000

        STRUCT S_VARS
vLinesCount     WORD
intLine         WORD
        ENDS

;----------------------------------------------------------------------------------------------------------
startDot:
        ; parse command line options
            call    parseCommandLine
            ld      hl,txtCopy
            call    printMsg
            call    analyseCurrentMode
cfgAnalyse  or      a                                   ; to be modified to `scf` by parseCommandLine
            call    c,displayModeAnalysis               ; -a
cfgStopCu   or      a                                   ; to be modified to `scf` by parseCommandLine
            jp      c,stopCopper                        ; -s to stop copper and restore interrupts (exits with CF=0)
            ld      hl,txtHelp
cfgHelp     or      a                                   ; to be modified to `scf` by parseCommandLine
            jp      c,printMsg                          ; -h or invalid args (exits with CF=0)

        ; stop any copper code currently running and restore interrupts config
            call    stopCopper

        ; select copper code to generate (in this prototype only HDMI 50Hz -> ZX48 50Hz exists)
cfgMode     ld      a,0     ;FIXME all                  ; to be modified by parseCommandLine

        ; generate copper code to adjust timings
            ld      bc,TBBLUE_REGISTER_SELECT_P_243B
            ld      a,COPPER_DATA_16B_NR_63
            out     (c),a
            inc     b

getZx48:
            ;FIXME generate copper dynamically
; 248..279: 32 lines, adding 12T per line, per four lines, -128T by late interrupt
;           = 32 * (216 + 12) - 128 = 7168 T vs ZX48 32 * 224 = 7168 T
;           - 4 segments at 14MHz = 16*4-4*4 = +48T (48/4 = +12T) at lines 248, 252, 256, 260, 264, 268, 272, 276
            ld      a,$80|(39<<1)                       ; copper WAIT, h=39
            ld      de,$0700+248
            ld      hl,$0200|((39^43)<<1)               ; h=2 means 14MHz, throttle is 4 segments => +48T => +12T per line
            exa
            ld      a,8                                 ; line 248..279 (248, 252, 256, 260, 264, 268, 272, 276)
.gen_l1:
            exa
            out     (c),a,,(c),e                        ; CWAIT line, 39
            out     (c),d,,(c),h                        ; CMOVE 7, fast CPU
            xor     l
            out     (c),a,,(c),e                        ; CWAIT line, 43
            xor     l
            out     (c),d,,(c),0                        ; CMOVE 7, 3.5MHz ; out0-ok
            .4 inc     e
            jr nz,$+3
            inc     a                                   ; handle +256 overflow in line number
            exa
            dec     a
            jr      nz,.gen_l1

; 280..291: 12 lines, adding 8T per line, per two lines
;           = 12 * (216 + 8) = 2688 T (same as ZX48)
;           - 4 segments at 7MHz = 8*4-4*4 = +16T (16/2 = +8T) at lines 280, 282, .., 290
            ld      a,6                                 ; line 280..291 (+2)
            dec     h                                   ; h = 1 as 7MHz, throttle 4 segments => +16T => +8T per line
.gen_l2:
            exa
            out     (c),a,,(c),e                        ; CWAIT line, 39
            out     (c),d,,(c),h                        ; CMOVE 7, fast CPU
            xor     l
            out     (c),a,,(c),e                        ; CWAIT line, 43
            xor     l
            out     (c),d,,(c),0                        ; CMOVE 7, 3.5MHz ; out0-ok
            .2 inc     e
            exa
            dec     a
            jr      nz,.gen_l2

; 292..311: 20 lines, adding 8T every line
;           = 20 * (216 + 8) = 4480 T (same as ZX48)
;           - 2 segments at 7MHz = 8*2-4*2 = +8T at each line
            ld      a,20                                ; line 292..311
            ld      l,(39^41)<<1                        ; throttle 2 segments => +8T per line
.gen_l3:
            exa
            out     (c),a,,(c),e                        ; CWAIT line, 39
            out     (c),d,,(c),h                        ; CMOVE 7, fast CPU
            xor     l
            out     (c),a,,(c),e                        ; CWAIT line, 41
            xor     l
            out     (c),d,,(c),0                        ; CMOVE 7, 3.5MHz ; out0-ok
            inc     e
            exa
            dec     a
            jr      nz,.gen_l3

;   0..211: 192+20 lines, adding 8T every line
;           = 212 * (216 + 8) = 47488 T (same as ZX48)
;           - 2 segments at 7MHz = 8*2-4*2 = +8T at each line
            exa
            dec     a                                   ; restore MSB for line 0
            exa
            ld      e,a                                 ; line 0
            ld      a,192+20                            ; line 0..211
.gen_l4:
            exa
            out     (c),a,,(c),e                        ; CWAIT line, 39
            out     (c),d,,(c),h                        ; CMOVE 7, fast CPU
            xor     l
            out     (c),a,,(c),e                        ; CWAIT line, 41
            xor     l
            out     (c),d,,(c),0                        ; CMOVE 7, 3.5MHz ; out0-ok
            inc     e
            exa
            dec     a
            jr      nz,.gen_l4

; 212..223: 12 lines, adding 8T per line, per two lines
;           = 12 * (216 + 8) = 2688 T (same as ZX48)
;           - 4 segments at 7MHz = 8*4-4*4 = +16T (16/2 = +8T) at lines 212, 214, .., 222
            ld      a,6                                 ; line 212..223 (+2)
            ld      l,(39^43)<<1                        ; throttle 4 segments => +16T => +8T per line
.gen_l5:
            exa
            out     (c),a,,(c),e                        ; CWAIT line, 39
            out     (c),d,,(c),h                        ; CMOVE 7, fast CPU
            xor     l
            out     (c),a,,(c),e                        ; CWAIT line, 43
            xor     l
            out     (c),d,,(c),0                        ; CMOVE 7, 3.5MHz ; out0-ok
            .2 inc     e
            exa
            dec     a
            jr      nz,.gen_l5

; 224..231: 8 lines, adding 8T per line, per four lines
;           = 8 * (216 + 8) = 1792 T (same as ZX48)
;           - 8 segments at 7MHz = 8*8-4*8 = +32T (32/4 = +8T) at lines 224, 228
            ld      a,2                                 ; line 224..231 (+4) (224, 228)
            ld      l,(39^47)<<1                        ; throttle 8 segments => +32T => +8T per line
.gen_l6:
            exa
            out     (c),a,,(c),e                        ; CWAIT line, 39
            out     (c),d,,(c),h                        ; CMOVE 7, fast CPU
            xor     l
            out     (c),a,,(c),e                        ; CWAIT line, 47
            xor     l
            out     (c),d,,(c),0                        ; CMOVE 7, 3.5MHz ; out0-ok
            .4 inc     e
            exa
            dec     a
            jr      nz,.gen_l6

; 232..247: 16 lines per 216T, no copper code
;           = 16 * 216 = 3456 T vs ZX48 3584 T -> 128T missing
; 248     : extra +128T until line interrupt vs ZX48 0T -> fixing total frame time 69888 T
; total copper slots: = 8+6+20+192+20+6+2 = 254
; two free slots remaining in copper = 2x4 NOOP to deal with that
            ld      a,2*4*2
.gen_l7:    out     (c),0                               ; CNOOP ; out0-ok
            dec     a
            jr      nz,.gen_l7

        ; setup line interrupt
            nextreg VIDEO_INTERUPT_VALUE_NR_23,249      ; line 249 to trigger at end of line 248:128T
            nextreg VIDEO_INTERUPT_CONTROL_NR_22,%110   ; disable ULA int, enabled line int, MSB=0

        ; start the copper code
            nextreg COPPER_CONTROL_HI_NR_62,%01'00'0000 ; reset CPC to zero, start copper

        ; exit back to NextZXOS
exit:
            or      a                                   ; clear CF to not signal error to NextZXOS
            ret

;----------------------------------------------------------------------------------------------------------
ReadNextReg:
        ; reads nextreg in A into A (does modify currently selected NextReg on I/O port)
            ld      bc,TBBLUE_REGISTER_SELECT_P_243B
            out     (c),a
            inc     b                                   ; bc = TBBLUE_REGISTER_ACCESS_P_253B
            in      a,(c)                               ; read desired NextReg state
            ret

;----------------------------------------------------------------------------------------------------------
stopCopper:
        ; stop any copper code currently running and restore interrupts configuration to ULA int
            xor     a                                   ; A = 0, CF = 0
            nextreg COPPER_CONTROL_LO_NR_61,a
            nextreg COPPER_CONTROL_HI_NR_62,a           ; stop copper and set write-index to 0
            nextreg VIDEO_INTERUPT_CONTROL_NR_22,a      ; restore interrupts back to ULA int
            ret

;----------------------------------------------------------------------------------------------------------
analyseCurrentMode:
        ; read current video mode properties
            call    findVLinesCount
            ld      (vars.vLinesCount),hl               ; total lines of video mode
        ; check interrupt line, assume the dot command is run with IM1 mode with enabled OS interrupts
        ; (other option is to read nextreg 0x22 in tight loop, which will most likely freeze emus)
            di
            ld      hl,im2tab
            ld      de,im2tab+1
            ld      bc,im2isr-im2tab
            ld      a,high(im2tab)
            ld      i,a
            im      2
            inc     a                                   ; A = high(im2isr)
            ld      (hl),a
            ldir
            ; build: `jp im2real_isr` at im2isr address
            ld      (hl),$C3
            inc     l
            ld      (hl),low(.isr_get_line)
            inc     l
            ld      (hl),high(.isr_get_line)
            ei
            halt                                        ; jumps to following instruction with DI
.isr_get_line:
            pop     hl                                  ; throw away return address
            ld      a,VIDEO_LINE_LSB_NR_1F
            call    ReadNextReg
            ld      l,a
            ld      a,VIDEO_LINE_MSB_NR_1E
            call    ReadNextReg
            ld      h,a
            ld      (vars.intLine),hl
        ; check line duration
            ;FIXME all, seems one will have to sample reading video line to not tamper with line interrupts

        ; restore IM 1 OS interrupts
            im      1
            ld      a,$3F
            ld      i,a
            ei

        ;FIXME all
            ret

; returns in HL the video-lines count for current mode (ie. 0x137 for VGA ZX128)
; modifies: AF, BC, HL
; This algorithm works correctly only for modes with 258..511 video lines
; core 3.1.5 and 3.1.10 conforms to this for all VGA/HDMI modes in any variant (min 261)
findVLinesCount:
            ld      bc,TBBLUE_REGISTER_SELECT_P_243B
            ld      hl,$0100 + VIDEO_LINE_LSB_NR_1F     ; H = 1, L = VIDEO_LINE_LSB_NR_1F
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

;----------------------------------------------------------------------------------------------------------
displayModeAnalysis:
        ; display analysis of video mode properties
        ;FIXME all
            ret

;----------------------------------------------------------------------------------------------------------
printMsg:
        ; print zero terminated string from HL by using `rst $10` (to be compatible with any user mode)
            ld      a,(hl)
            inc     hl
            and     $7F                     ; clear 7th bit
            ret     z                       ; exit if terminator ($00 or $80)
            rst     $10
            jr      printMsg

;----------------------------------------------------------------------------------------------------------
isWhiteOrEol:
        ; returns:
        ;   ZF=1, CF=0 : the char in A is space or EOL-like
        ;   ZF=0, CF=0 : other char
            cp      ' '
            ret     z
.eolOnly:
            cp      ':'
            ret     z
            cp      13
            ret     z
            cp      10
            ret     z
            or      a
            ret

;----------------------------------------------------------------------------------------------------------
skipWhite:
        ; returns:
        ;   CF=0 : A = non-whitespace char, HL points after it
        ;   CF=1 : end of line was reached without any non-whitespace char
.loop:  ; skip through spaces
            ld      a,(hl)
            inc     hl
            cp      ' '
            jr      z,.loop
        ; check for EOLs: 0, 10, 13, ':'
            call    isWhiteOrEol.eolOnly
            ret     nz                                  ; non-white char, return with CF=0 and char in A
            scf
            ret                                         ; signal EOL by CF=1

letterPal:  DB      "ulst"

;----------------------------------------------------------------------------------------------------------
parseCommandLine:
        ; FIXME all
            or      a
            ret
/*
            ld      a,h
            or      l
            jr      z,displayHelp                       ; 0 == HL -> empty command line, display help
        ; some command line is available, try to parse it
            call    skipWhite
            jr      c,displayHelp                       ; encountering end of line too soon
        ; select palette by the letter
            or      $20                                 ; lowercase it
            ex      de,hl
            ld      hl,letterPal
            ld      bc,5                                ; +1 length to run beyond the buffer in case of mismatch
            cpir
            ld      a,l
            sub     1 + low letterPal                   ; A = 0,1,2,3,4 for [u,l,s,t,<other>]
            cp      4
            jr      nc,displayHelp                      ; invalid palette letter, display help
            ex      de,hl
            ld      c,a                                 ; C = 0,1,2,3 -> palette select (will become bits 5-4 for NR $43)
        ; check if there is optional digit 0/1 to select first/second palette
            ld      a,(hl)
            sub     '0'
            jr      c,.not_a_01digit
            cp      2
            jr      nc,.not_a_01digit
        ; A = 0/1 depending on the digit 0/1 in command line
            inc     hl                                  ; '0'/'1' char accepted
        .2  add     a,a                                 ; A<<2
            or      c
            ld      c,a                                 ; C = palette select with future bit 6 (first/second palette)
.not_a_01digit:
        ; check if there is whitespace or EOL after palette option (otherwise display help)
            ld      a,(hl)
            call    isWhiteOrEol
            jr      nz,displayHelp
        ; convert the palette select value to final form and store it in variable
            ld      a,c
            swapnib
            ld      (palette),a
        ; check if there is optional <delay> argument
            call    skipWhite
            ccf
            ret     nc                                  ; that was all, done, return with CF=0 signalling OK
        ; non-white char in A, parse it as integer
            ld      e,0
.parseDelay:
            sub     '0'
            jr      c,displayHelp                       ; non-digit char, display help
            ld      d,10
            cp      d
            jr      nc,displayHelp                      ; non-digit char, display help
            mul     de                                  ; E *= 10
            add     de,a                                ; DE += digit
            ld      a,d
            or      a
            jr      nz,displayHelp                      ; integer overflow, display help
        ; parse next char, should be digit or white/eol
            ld      a,(hl)
            inc     hl
            call    isWhiteOrEol
            jr      nz,.parseDelay                      ; some char, could be digit, check it
            dec     hl                                  ; space or EOL, revert ++HL for final check
        ; <delay> parsed, store it
            ld      a,e
            or      a
            jr      z,displayHelp                       ; only values 1..255 are valid
            ld      (delay),a
        ; check if EOL can be reached
            call    skipWhite
            jr      nc,displayHelp                      ; something unexpected on the remaining line, display help
        ; all parsed, all OK, clear CF -> run the effect
            or      a
            ret

*/

;----------------------------------------------------------------------------------------------------------

                    ;12345678901234567890123456789012; 32chars width
txtCopy:    DB      "dot7gFX HDMI timing change v0.2\r"
            DB      "by Ped7g, installs copper code\r\r"
            DB      0
txtHelp:    DB      "TODO SYNOPSIS:\r"
            DB      " ../HDMIT.DOT <?> [<?>]\r"
            DB      "TODO ARGUMENTS:\r"
            DB      " ? = <type u|l|s|t>[0|1]\r"
            DB      " ? = <type 1..255>\r\r"
            DB      "TODO EXAMPLE:\r"
            DB      " ../HDMIT.DOT ? ?\r"
            DB      "     blah blah\r"
            DB      0

;----------------------------------------------------------------------------------------------------------
; initialised data (with default values)

;----------------------------------------------------------------------------------------------------------
; uninitialised data -> not part of the binary

vars        S_VARS = $

im2tab      equ     (vars + S_VARS + 255) & -256
im2isr      equ     im2tab + high(im2tab) + $0101

; for debugging in CSpect (with full card image)
        DEVICE ZXSPECTRUM48 : CSPECTMAP "hdmit.map"
