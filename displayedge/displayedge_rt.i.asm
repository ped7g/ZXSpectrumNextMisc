;-------------------------------
; DISPLAYEDGE runtime library
; © Peter Helcmanovsky 2020, license: https://opensource.org/licenses/MIT
;
; Runtime functions to read config file from system, detect video mode and figure out
; user's settings for current mode (or when mode did change)
;
; Assembles with sjasmplus - https://github.com/z00m128/sjasmplus (v1.14.0+)
;
; Define USE_TO_READ_NEXT_REG to your function to read NextReg (IN/OUT in A, preserve BC)
; or keep undefined to let this source add `dspedge.ReadNextReg` function.
;
; Changelist:
; v1    18/01/2020 P7G    Initial version (unfinished)
;-------------------------------
; # API (list of functions):
; DetectMode                   - returns dspedge.MODE_* value (current display mode)

    MODULE dspedge

MODE_HDMI_50        EQU         0
MODE_ZX48_50        EQU         1
MODE_ZX128_50       EQU         2
MODE_ZX128P3_50     EQU         3
MODE_HDMI_60        EQU         4
MODE_ZX48_60        EQU         5
MODE_ZX128_60       EQU         6
MODE_ZX128P3_60     EQU         7
MODE_PENTAGON       EQU         8   ; the board ignores 50/60Hz bit for Pentagon timing
MODE_COUNT          EQU         9

    ; if no "USE_TO_READ_NEXT_REG" function was provided by source including this file, define own
    IFNDEF USE_TO_READ_NEXT_REG
        DEFINE USE_TO_READ_NEXT_REG @dspedge.ReadNextReg

ReadNextReg:
        ; reads nextreg in A into A
        ; Input
        ;       A = nextreg to read
        ; Output:
        ;       A = value in nextreg
        ; Uses:
        ;       A, [currently selected NextReg on I/O port $243B]
                push    bc
                ld      bc, $243B   ; TBBLUE_REGISTER_SELECT_P_243B
                out     (c),a
                inc     b       ; bc = TBBLUE_REGISTER_ACCESS_P_253B
                in      a,(c)   ; read desired NextReg state
                pop     bc
                ret
    ENDIF

;;----------------------------------------------------------------------------------------
;; Detect current video mode:
;; 0, 1, 2, 3 = HDMI, ZX48, ZX128, ZX128+3 (all 50Hz), add +4 for 60Hz modes
;; 8 = Pentagon (board generates identical ~49Hz signal for 50/60Hz setting)

DetectMode:
        ; Output:
        ;       A = current mode 0..8
        ; Uses:
        ;       B, side effect: selects NextReg $11 or $03 on I/O port

                ;; read current configuration from NextRegs and convert it to 0..8 value
                ; read 50Hz/60Hz info
                ld      a,$05 ; PERIPHERAL_1_NR_05
                call    USE_TO_READ_NEXT_REG
                and     $04             ; bit 2 = 50Hz/60Hz configuration
                ld      b,a             ; remember the 50/60 as +0/+4 value in B
                ; read HDMI vs VGA info
                ld      a,$11 ; VIDEO_TIMING_NR_11
                call    USE_TO_READ_NEXT_REG
                inc     a               ; HDMI is value %111 in bits 2-0 -> zero it
                and     $07
                jr      z,.hdmiDetected
                ; if VGA mode, read particular zx48/zx128/pentagon setting
                ld      a,$03 ; MACHINE_TYPE_NR_03
                call    USE_TO_READ_NEXT_REG
                ; a = bits 6-4: %00x zx48, %010 zx128/+2, %011 zx128+2A/B,+3,Next, %100 pentagon
                bit     6,a
                jr      nz,.pentagonDetected
                swapnib a
                and     $03             ; A = (0|1)/2/3 for zx48/zx128/pentagon
                jr      nz,.hdmiDetected    ; 1/2/3 -> just add 50/60Hz and return
                inc     a               ; 0->1 to end as ZX48
.hdmiDetected:  add     a,b             ; add 50/60Hz value to final result
                ret
.pentagonDetected:
                ld      a,MODE_PENTAGON
                ret

    ENDMODULE
