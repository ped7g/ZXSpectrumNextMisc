;-------------------------------
; DISPLAYEDGE runtime library
; © Peter Helcmanovsky 2020, license: https://opensource.org/licenses/MIT
; project repository: https://github.com/ped7g/ZXSpectrumNextMisc (report issues here)
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
; v2    23/01/2021 P7G    Refactored to make the runtime smaller (-36 bytes => 434)
; v1.2  28/01/2020 P7G    Incorporating the feedback from discord:
;                           keywords prefix "edge_", the default cfg: /sys/env.cfg
; v1.1  27/01/2020 P7G    Adding GetMargins API and improving docs about usage
; v1    25/01/2020 P7G    First working version, before public test
; v0    18/01/2020 P7G    Initial version (unfinished)
;
;-------------------------------
; # API (list of functions), all symbols are inside "dspedge" module:
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
                cp      1               ; turn 0 into 1 (config mode is treated as ZX48)
.hdmiDetected:  adc     a,b             ; add 50/60Hz value to final result
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
                ld      b,$FF
                ld      c,b
                ld      d,b
                ld      e,b
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

defaultCfgFileName:
                DZ      "/sys/env.cfg"  ; zero terminated for esxDOS
                DB      32|128          ; bit7 terminated for UI of .displayedge tool

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
            ; initialize margins array to all -1
                ld      a,-1
                ld      b,S_MARGINS * MODE_COUNT
.fillMarginArray:
                ldi     (de),a          ; fake: ld (de),a : inc de
                djnz    .fillMarginArray
            ; open the file - use both HL + IX for filename, to work as dot command or app
                push    hl
                pop     ix
                ld      a,'$'           ; system drive (if not overridden in fname)
                inc     b               ; B = $01 (read-only)
                rst     $08 : DB $9A    ; F_OPEN
                pop     hl              ; HL = buffer pointer
                ret     c               ; F_OPEN failed, return with carry set + A=error
                ld      (.Fhandle),a
            ; load full buffer in two 128B steps (to make zero-terminator logic work!)
                call    .readBuffer     ; read the *other* half of buffer at $xx80 (!)
                ld      l,$80           ; L=$80 to load $xx00 half of buffer
                call    .readBuffer     ; and start parsing from the $xx80
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
                jr      z,.readBuffer   ; $xxFF -> $xx00 is Fz=1 -> load second half of buffer
                ret     po              ; not $xx7F -> $xx80, don't load first half of buffer
.readBuffer:
            ; buffer is half-empty, read further 128 bytes
                push    af              ; already read char
                push    hl              ; address of next char
                push    de              ; preserve DE (is working register for parser)
                ld      bc,$80
                ; flip L by $80, to read other buffer half
                ld      a,l
                xor     c
                ld      l,a
                push    hl
                pop     ix
.Fhandle=$+1:   ld      a,low .Fhandle  ; self-modify storage for handle
                rst     $08 : DB $9D    ; F_READ: A = file handle, HL+IX = address, BC = bytes to read
                jr      c,.esxError
                ; BC=DE=bytes read, HL+=BC
                ; CSpect 2.12.5 w/o full NextZXOS returns always HL + original_BC (emu bug)
                bit     7,c
                jr      nz,.full128BytesRead
                ld      (hl),b          ; add null terminator after last read byte
.full128BytesRead:
                pop     de
                pop     hl              ; restore current address
                pop     af              ; restore the char read
                ret

.parseNewLineLoop:
            ; HL = current buffer
                call    skipWhiteSpace
                ; check for known keywords 'hdmi, zx48, zx128, zx128p3, pentagon', else skipToEol
                call    matchKeyword    ; ZF=1 no match, ZF=0 match, HL points after, A=0..8 match number
                jr      z,.skipToEol
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

; ! WARNING, ABI change ! in previous version Fz=1 was "match"
; (this routine is considered internal, not part of public API, see EXPORT statements
; near end of file to see what is part of public API, and unlikely to change ever)
matchKeyword:
    ; In:   HL=input stream, Modifies: AF, BC (in .getCh), DE, HL
    ; Out:  Fz=1 no match, preserves HL, A=0
    ;       Fz=0 match, stream+HL is advanced after keyword, A=keyword index (0,1,...,N)
                push    hl
                ex      de,hl

            ; search for "edge_" string in input buffer (not advancing stream)
                ld      hl,.keywordEdge
                call    .compareKeyword
                jr      z,.didNotMatchInPrefix

            ; "edge_" did match, now try to match all different video-mode keywords
                inc     hl          ; HL = keywordsModes ptr (video mode keyword list)
.tryNextKeywordSuffix:              ; DE = input buffer (after "_"), HL = keyword to try
                push    de
                call    .compareKeyword
                jr      z,.tryNextKeyword   ; did not match, try one more
            ; keyword did match, check if it ends with "word boundary" character
                cp      ' '+1       ; anything less/equal to space char is ok
                jr      c,.keywordFound
                cp      '='         ; and "assign" directly after mode is ok
                jr      z,.keywordFound

            ; keyword-suffix didn't match, try another suffix (starting after "edge_")
                inc     hl          ; start of next keyword
.tryNextKeyword:
                pop     de          ; restore buffer pointer
                bit     7,(hl)      ; check if there is another keyword in list
                jr      z,.tryNextKeywordSuffix
            ; all keywords failed, report "no match" result: Fz=1 (!)
.didNotMatchInPrefix:
                pop     hl          ; restore original input buffer pointer
                xor     a
                ret                 ; ZF=1, HL = original buffer (no keyword match)

            ; keyword was found, advance the input stream beyond it, return its index
.keywordFound:                      ; E is here: low(end-of-keyword-ptr)
                ld      d,(hl)      ; terminator of keyword (encodes keyword index)
                pop     hl          ; throw away ptr after "edge_" keyword
                pop     hl          ; original buffer, E will be used to advance it
.advanceStreamLoop:                 ; keep reading input stream until L == E
                call    ParseCfgFile.getCh
                ld      a,e
                sub     l
                jr      nz,.advanceStreamLoop
            ; HL = next char after keyword, D = terminator (encoded index), A = 0, ZF=1
                or      d           ; ZF=0 (!), A = encoded index (value 128..255)
                cpl                 ; decode index from terminator value into 0..N value
                ret                 ; ZF=0, A = keyword index

.compareKeyword:
                ld      a,(de)      ; character in input buffer
                bit     7,(hl)
            ; DE = next char ptr, A = next char, HL = keyword char/terminator ptr
                ret     nz          ; indicate match with ZF=0
            ; regular char in keyword, do the compare
                inc     e           ; ++DE to next character in [circular] input buffer
                cp      (hl)
                inc     hl          ; compare A with [HL++]
                jr      z,.compareKeyword
            ; regular letter mismatch - skip rest of current keyword until terminator
.letterMismatch:
                bit     7,(hl)
                inc     hl          ; HL++ until it points beyond the terminator
                jr      z,.letterMismatch
            ; return mismatch: ZF=1, DE = mismatch char ptr, A = 0, HL = next keyword ptr
                xor     a
                ret

.keywordEdge    DB      'edge_',128
.h_5            DB      'hdmi_50',~0    ; the other keywords must follow right after "edge_"
.z4_5           DB      'zx48_50',~1
.z1_5           DB      'zx128_50',~2
.z3_5           DB      'zx128p3_50',~3
.h_6            DB      'hdmi_60',~4
.z4_6           DB      'zx48_60',~5
.z1_6           DB      'zx128_60',~6
.z3_6           DB      'zx128p3_60',~7
.p              DB      'pentagon',~8
                DB      128             ; end of keywords

End:
    DISPLAY "dspedge module (runtime part) machine code size: ",/D,End-Begin
    ENDMODULE

    IFDEF DISPLAYEDGE_ORG_ADR
        OUTEND

        ; when assembling with DISPLAYEDGE_ORG_ADR defined, produce also export file
        ; use "--exp=displayedge_rt.exp" to define the export name on command line
        EXPORT dspedge.Begin
        IFUSED dspedge.ReadNextReg
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
