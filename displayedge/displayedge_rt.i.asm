;-------------------------------
; DISPLAYEDGE runtime library
; © Peter Helcmanovsky 2020, license: https://opensource.org/licenses/MIT
;
; Runtime functions to read config file from system, detect video mode and figure out
; user's settings for current mode
;
; Assembles with sjasmplus - https://github.com/z00m128/sjasmplus (v1.14.0+)
;
; Define USE_TO_READ_NEXT_REG to your function to read NextReg (IN/OUT in A, preserve BC)
; or keep undefined to let this source add `dspedge.ReadNextReg` function.
;
; You can define DISPLAYEDGE_ORG_ADR to assemble this file in stand-alone way re-located
; to the defined address, and produce the binary "displayedge_rt.bin", or you can include
; this source file into your own source and deal with location/binary saving on your own
; (see the .displayedge tool project (displayedge.asm+Makefile) for examples)
; The standalone "displayedge_rt.bin" can be included to your project and use labels from
; the export file (--exp sjasmplus option) to call the binary functions.
;
; Changelist:
; v1.2  28/01/2020 P7G    Incorporating the feedback from discord:
;                           keywords prefix "edge_", the default cfg: /sys/env.cfg
; v1.1  27/01/2020 P7G    Adding GetMargins API and improving docs about usage
; v1    25/01/2020 P7G    First working version, before public test
; v0    18/01/2020 P7G    Initial version (unfinished)
;
;-------------------------------
; # API (list of functions), all symbols inside "dspedge" module:
; ReadNextReg           - reads nextreg A into A (`-DUSE_TO_READ_NEXT_REG=<yours>` if you have own)
; DetectMode            - returns dspedge.MODE_* value (current display mode)
; GetMargins            - returns already parsed margins for desired mode
; SanitizeMarginValue   - A value clamped to 0..31 (32..127 -> 31, 128..255 -> 0)
; ParseCfgFile          - will parse values from provided CFG file into memory
; defaultCfgFileName    - default CFG filename: "/sys/env.cfg",0
;-------------------------------

    ; switch sjasmplus to correct syntax variant
    OPT push reset --zxnext --syntax=abfw

    MODULE dspedge

    IFDEF DISPLAYEDGE_ORG_ADR
        ORG DISPLAYEDGE_ORG_ADR
        OUTPUT "displayedge_rt.bin"
    ENDIF

Begin:

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

    STRUCT S_MARGINS        ; pixels of margin 0..31 (-1 = undefined margin)
L           BYTE    -1      ; left
R           BYTE    -1      ; right
T           BYTE    -1      ; top
B           BYTE    -1      ; bottom
    ENDS

    ; if no "USE_TO_READ_NEXT_REG" function was provided by source including this file, define own
    IFNDEF USE_TO_READ_NEXT_REG
        DEFINE USE_TO_READ_NEXT_REG @dspedge.ReadNextReg

ReadNextReg:
        ; reads nextreg in A into A
        ; Input:
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

            ; read current configuration from NextRegs and convert it to 0..8 value
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

;;----------------------------------------------------------------------------------------
;; Retrieve parsed margin values for particular mode

GetMargins:
        ; Input:
        ;       A = dspedge.MODE_* value, which mode margins should be returned
        ;       DE = S_MARGINS[MODE_COUNT] array (4 * 9 = 36 bytes) - parsed data by ParseCfgFile
        ; Output:
        ;       BCDE = left, right, top, bottom margin values
        ;              (255, 255, 255, 255 = "not in file" or "invalid mode index")
        ; Uses:
        ;       A

                cp      MODE_COUNT
                jr      nc,.invalidModeIndex
            ; valid mode index, fetch the data from array (not sanitizing them *again*)
                rlca
                rlca
                add     de,a            ; DE = DE + 4*mode_index
                push    hl
                ex      de,hl
                ldi     b,(hl)          ; fake: ld b,(hl) : inc hl  ; LEFT margin
                ldi     c,(hl)          ; fake: ld c,(hl) : inc hl  ; RIGHT margin
                ldi     d,(hl)          ; fake: ld d,(hl) : inc hl  ; TOP margin
                ld      e,(hl)                                      ; BOTTOM margin
                pop     hl
                ret
            ; for invalid index return 4x 255
.invalidModeIndex:
                ld      bc,$FFFF
                push    bc
                pop     de
                ret

;;----------------------------------------------------------------------------------------
;; Sanitize margin value in A, turning it into 0..31 value.
;; Values 32..127 will become 31, 128..255 (-128..-1) will become 0.

SanitizeMarginValue:
        ; Input:
        ;       A = margin value to sanitize
        ; Output:
        ;       A = sanitized margin value

            ; sanitize the value in A
                test    -32
                ret     z               ; value OK
            ; value is 32..255 .. the 32..127 will become 31, 128..255 will become 0
                ld      a,31
                ret     p
                xor     a
                ret

;;----------------------------------------------------------------------------------------
;; Read and parse cfg file - use it before using API calls to get margin values for mode

ParseCfgFile:
        ; Input:
        ;       HL = filename of CFG file (zero terminated string for OS call)
        ;       DE = S_MARGINS[MODE_COUNT] array (4 * 9 = 36 bytes of memory to store results)
        ;       BC = aligned 256 byte buffer for reading file content ($xx00 address)
        ; Output:
        ;       when Fc = 1
        ;       A = esxDOS error (fopen, fread or fclose failed)
        ;       The S_MARGINS array is always already initialized to [-1,-1,-1,-1]
        ;       when Fc = 0
        ;       The S_MARGINS array contains parsed values (-1 for missing mode/value)
        ;       (values are sanitized to valid [-1, 0..31] even if file says other)
        ; Uses:
        ;       AF, BC, DE, HL, IX

                ld      (.oldSP),sp
                ld      (.MarginsPtr),de
                push    bc
                push    hl
                push    hl
            ; initialize margins array to all -1
                ld      h,d
                ld      l,e
                ld      (hl),-1
                inc     de
                ld      bc,S_MARGINS * MODE_COUNT - 1
                ldir
            ; open the file - use both HL + IX for filename, to work as dot command or app
                pop     hl
                pop     ix
                ld      a,'$'           ; system drive (if not overriden in by fname)
                ld      b,$01           ; read-only
                rst     $08 : DB $9A    ; F_OPEN
                pop     hl              ; HL = buffer pointer
                ret     c               ; F_OPEN failed, return with carry set + A=error
                ld      (.Fhandle),a
            ; load full buffer in two 128B steps (to make zero-terminator logic work!)
                ld      l,$80           ; L=$80 to load first half of buffer
                call    .readBuffer
                ld      l,b             ; L=0 to load second half of buffer
                call    .readBuffer
                call    .parseNewLineLoop
            ; F_CLOSE the file
                ld      a,(.Fhandle)
                rst     $08 : DB $9B    ; F_CLOSE
.esxError:  ; throw away all stack values to preserve A + Fc + HL, and return up
.oldSP=$+1:     ld      sp,0            ; self-modify storage
                ret

.getCh:
                ld      a,(hl)
                inc     l
                jp      pe,.readBuffer  ; $xx7F -> $xx80, load first 128B of buffer
                ret     nz              ; $xxFF -> $xx00 is Fz=1 -> load second 128B
.readBuffer:
            ; buffer is half-empty, read further 128 bytes
                ld      bc,$80
                push    af              ; char read
                push    hl              ; address of next char
                ; advance HL by $80, to read one buffer ahead
                ld      a,l
                add     a,c
                ld      l,a
                push    hl
                pop     ix
.Fhandle=$+1:   ld      a,low .Fhandle  ; self-modify storage for handle
                push    de              ; preserve DE (is working register for parser)
                rst     $08 : DB $9D    ; F_READ: A = file handle, HL+IX = address, BC = bytes to read
                jr      c,.esxError
                ; BC=DE=bytes read, HL+=BC
                ; CSpect 2.12.5 w/o full NextZXOS returns always HL + original_BC (emu bug)
                pop     de
                bit     7,c
                jr      nz,.full128BytesRead
                ld      (hl),0          ; add null terminator after last read byte
.full128BytesRead:
                pop     hl              ; restore current address
                pop     af              ; restore the char read
                ret

.parseNewLineLoop:
            ; HL = current buffer
                call    skipWhiteSpace
                ; check for known keywords 'hdmi, zx48, zx128, zx128p3, pentagon', else skipToEol
                call    isKeyword       ; ZF=0 no match, ZF=1 match, HL points after, A=0..8 match number
                jr      nz,.skipToEol
                ld      e,a
                ; look for "assign" character
                call    skipWhiteSpace
                cp      '='
                jr      nz,.skipToEol
                call    .getCh          ; eat the '=' char
            ; prepare to parse values
                ld      d,S_MARGINS
                mul     de
.MarginsPtr=$+2:add     de,.MarginsPtr   ; self-modify storage for value
                        ; DE = address into margins array
                call    ParseFourValuesToDe
                ;  |
                ; fallthrough to .skipToEol (to skip rest of line)
                ;  |
.skipToEol:
                call    .getCh
                or      a
                ret     z               ; null-terminated file or EOF
                cp      10
                jr      z,.skipEolItself
                cp      13
                jr      nz,.skipToEol
.skipEolItself:
            ; check if next char is also EOL, then keep reading+skipping
                ld      a,(hl)
                cp      13
                jr      z,.skipToEol
                cp      10
                jr      z,.skipToEol
                jr      .parseNewLineLoop

skipWhiteSpace_doSkip:
                call    ParseCfgFile.getCh  ; eat the whitespace
skipWhiteSpace:
                ld      a,(hl)
                cp      ' '         ; space char
                jr      z,skipWhiteSpace_doSkip
                cp      9           ; tab char
                jr      z,skipWhiteSpace_doSkip
                ret

ParseFourValuesToDe:
                call    .parseTwoValuesToDe
.parseTwoValuesToDe:
                call    .parseValueToDe
.parseValueToDe:
            ; skip any whitespace and single comma within it
                call    skipWhiteSpace
                cp      ','
                call    z,skipWhiteSpace_doSkip ; eats comma first :)
                or      a
                ret     z           ; null terminator hit, abort everything
            ; parse the decimal digits into value
                push    de
                ld      e,0
                call    .parseDigits
                ld      a,e
                call    SanitizeMarginValue
                pop     de
                ld      (de),a
                inc     de
                ret
.parseDigits:
            ; check if next char is digit
                ld      a,(hl)
                sub     '0'
                ret     c
                cp      10
                ret     nc
            ; next char is digit, add it to total value (already converted by check above)
                ld      d,10
                mul     de
                add     de,a
            ; eat the digit char from input stream
                call    ParseCfgFile.getCh
                jr      .parseDigits

isKeyword:
    ; Fz=0 no match, keeps HL
    ; Fz=1 match, HL points after, A=match index (0..N)
                ld      de,keywordsModes
                ld      bc,$0100    ; match flag + match index
                push    hl
.matchLoop:
                ld      a,(de)
                inc     de
                or      a
                jr      nz,.keywordContinues
                djnz    .wordMismatch
            ; keyword match, check end word boundary, can be anything <= 32 or '=' char
                ld      a,(hl)
                cp      ' '+1
                jr      c,.wordMatch
                cp      '='
                jr      nz,.wordMismatch
.wordMatch:
            ; keyword did match, return HL advanced, but by "getCh" to keep
            ; the buffer preload working too
                pop     de
                ex      de,hl       ; HL = old HL, DE = target HL, HL != DE (keyword.length != 0)
                ld      d,c         ; D = match index
.AdvanceHlLoop:
                call    ParseCfgFile.getCh
                ld      a,e
                cp      l
                jr      nz,.AdvanceHlLoop   ; also sets Fz=1 to signal match
                ld      a,d         ; A = match index
                ret
.wordMismatch:
            ; keyword mismatch, reset matching and start comparing with next keyword
                pop     hl          ; restore original HL
                ld      b,1         ; reset match flag
                ld      a,(de)
                cp      b
                ret     c           ; if A == 0, return with Fz=0
            ; new keyword starts here (A = first char already)
                push    hl
                inc     de
                inc     c           ; keyword index
.keywordContinues:
                cp      (hl)
                jr      z,.charDoesMatch
                inc     b           ; will be in 2..strlen(keyword)+1 range = ok
.charDoesMatch:
                inc     l
                jr      .matchLoop

defaultCfgFileName:
                DZ      "/sys/env.cfg"  ; zero terminated for esxDOS
                DB      32|128          ; bit7 terminated for UI of .displayedge tool

keywordsModes:                      ; (less than 128 chars per keyword)
.h_5            DZ      'edge_hdmi_50'
.z4_5           DZ      'edge_zx48_50'
.z1_5           DZ      'edge_zx128_50'
.z3_5           DZ      'edge_zx128p3_50'
.h_6            DZ      'edge_hdmi_60'
.z4_6           DZ      'edge_zx48_60'
.z1_6           DZ      'edge_zx128_60'
.z3_6           DZ      'edge_zx128p3_60'
.p              DZ      'edge_pentagon'
                DB      0           ; end of keywords

End:
    ENDMODULE

    IFDEF DISPLAYEDGE_ORG_ADR
        OUTEND

        ; when assembling with DISPLAYEDGE_ORG_ADR defined, produce also export file
        ; use "--exp=displayedge_rt.exp" to define the export name on command line
        EXPORT dspedge.Begin
        IFNDEF USE_TO_READ_NEXT_REG
            EXPORT dspedge.ReadNextReg
        ENDIF
        EXPORT dspedge.DetectMode
        EXPORT dspedge.defaultCfgFileName
        EXPORT dspedge.SanitizeMarginValue
        EXPORT dspedge.ParseCfgFile
        EXPORT dspedge.End
        ; video modes constants
        EXPORT dspedge.MODE_HDMI_50
        EXPORT dspedge.MODE_ZX48_50
        EXPORT dspedge.MODE_ZX128_50
        EXPORT dspedge.MODE_ZX128P3_50
        EXPORT dspedge.MODE_HDMI_60
        EXPORT dspedge.MODE_ZX48_60
        EXPORT dspedge.MODE_ZX128_60
        EXPORT dspedge.MODE_ZX128P3_60
        EXPORT dspedge.MODE_PENTAGON
        EXPORT dspedge.MODE_COUNT       ; number of different modes
        ; S_MARGINS structure
        EXPORT dspedge.S_MARGINS        ; length of structure
        EXPORT dspedge.S_MARGINS.L      ; offset Left-margin pixels
        EXPORT dspedge.S_MARGINS.R      ; offset Right-margin pixels
        EXPORT dspedge.S_MARGINS.T      ; offset Top-margin pixels
        EXPORT dspedge.S_MARGINS.B      ; offset Bottom-margin pixels
    ENDIF

    OPT pop     ; restore original configuration of sjasmplus syntax
