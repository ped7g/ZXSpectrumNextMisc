;; function to calculate size of Z80N opcode
;;
;; © Peter Helcmanovsky 2020, license: https://opensource.org/licenses/MIT
;;
;; this is meant to be reasonably fast while also small+simple (size 176 bytes)

    ; switch sjasmplus to correct syntax variant
    OPT push reset --zxnext --syntax=abfw

;; returns size of opcode in memory in bytes
;; In:  HL = address of opcode
;; Out: A = opcode size
;; Modifies: DE
GetZ80NOpcodeSize:
    ; if (0xCD == (b[0] & 0xCF))
        ld      e,(hl)
        ld      a,$CF
        and     e
        cp      $CD
        jr      nz,.NotPrefixLikeInstruction
    ; call ** / ED / IXY happened, filter it further
        sub     e                   ; A = $CD-b[0]
        jr      z,.return3          ; if (0xCD == b[0]) -> "call **" return 3
        cp      $CD-$ED             ; ZF = (0xED == b[0])
        inc     hl
        ld      a,(hl)              ; b[1] into A
        jr      z,.ExtendedInstructions ; if (0xED == b[0]) -> extended instructions
    ; IXY instructions
    ; prevent recursion for DD DD FD FD DD FD arrays (return 1)
        cp      $DD
        jr      z,.return1andDecHl
        cp      $FD
        jr      z,.return1andDecHl
    ; ED prefix invalidates previous DD/FD prefix (return 1)
        cp      $ED
        jr      z,.return1andDecHl
    ; IXY bit-prefix instruction (return 4)
        cp      $CB
        jr      z,.return4andDecHl
    ; IXY prefixed `halt` (return 2)
        cp      $76
        jr      z,.return2andDecHl
    ; calculate "extraL" for +2 when the instruction has "(hl)" memory access (else +1)
        sub     $34                 ; A = b[1]-$34
        cp      $36-$34
        jr      c,.return3andDecHl  ; $34, $35 -> "inc/dec (ixy+*)"
        jr      z,.return4andDecHl  ; $36 "ld (ixy+*),*"
        sub     $70-$34             ; A = b[1]-$70
        cp      $77-$70+1
        jr      c,.return3andDecHl  ; ($70 <= b[1] <= $77) -> "ld (ixy+*),r8"
        add     $70-$46             ; A = b[1]-$46
        cp      $BE-$46+1
        jr      nc,.hasNotHlMemPtr  ; (b[1] < $40+6 || $BE < b[1]) -> surely no "(ixy+*)"
        and     7
        jr      z,.return3andDecHl  ; "ld r8,(ixy+*)" or "and/or/xor/... (ixy+*)"
.hasNotHlMemPtr:
        call    GetZ80NOpcodeSize   ; get length of regular instruction formed by rest of opcode
        dec     hl                  ; restore HL
        inc     a                   ; add extra +1 length to regular length
        ret

.return1andDecHl:
            dec     hl
.return1:   ld      a,1
            ret
.return3andDecHl:
            dec     hl
.return3:   ld      a,3
            ret
.return4andDecHl:
            dec     hl
            ld      a,4
            ret

.ExtendedInstructions:
        cp      $27
        jr      z,.return3andDecHl  ; Z80N test imm8
        cp      $92
        jr      z,.return3andDecHl  ; Z80N nextreg imm8,A
        cp      $8A
        jr      z,.return4andDecHl  ; Z80N push imm16
        cp      $91
        jr      z,.return4andDecHl  ; Z80N nextreg imm8,imm8
        sub     $34
        cp      $36-$34+1
        jr      c,.return4andDecHl  ; Z80N add r16,imm16 ($34 .. $36)
        ld      a,$C7
        and     (hl)
        cp      $43                 ; $43, $4B, $53, $5B, $63, $6B, $73, $7B
        jr      z,.return4andDecHl  ; ld (**),r16, ld r16,(**)
    ; everything else from extended is length 2
.return2andDecHl:
            dec     hl
.return2:   ld      a,2
            ret

.NotPrefixLikeInstruction:
    ; E = b[0]
        ld      a,$40
        add     a,e
        jp      m,.return1          ; all 0x40 .. 0xBF instructions (return 1)
        ld      e,a                 ; E = b[0] + $40
        and     7                   ; A = "idx7" (b[0] & 0x07)
        cp      4
        jr      z,.Idx7IsFour
        jr      c,.Idx7Is0to3
    ; for 5..7 idx7 the pattern is 1,2,1 (CD/DD/ED/FD are already processed)
        and     1
        jr      z,.return2
        ret
    ; for 4 == idx7: Cx..Fx have 3B (call cc), 0x..3x have 1B (inc/dec)
.Idx7IsFour:
        bit     6,e                 ; return (x0 < 0x40) ? 3 : 1;
        jr      z,.return3          ; "call cc"
        ld      a,1                 ; "inc/dec"
        ret
    ; for 0..3 idx7 the pattern differs between many octets, so just use table
    ; 1133 .... | 1132 ....    ; C0..CF
    ; 1132 .... | 1132 ....    ; D0..DF
    ; 1131 .... | 1131 ....    ; E0..EF
    ; 1131 .... | 1131 ....    ; F0..FF
    ; 1311 .... | 1111 ....    ; 00..0F
    ; 2311 .... | 2111 ....    ; 10..1F
    ; 2331 .... | 2131 ....    ; 20..2F
    ; 2331 .... | 2131 ....    ; 30..3F
.Idx7Is0to3:
        push    af
        xor     e                   ; A = %0bbbb000 (octet number in b6..b3)
        rrca
        rrca
        rrca                        ; A = octet = x0>>3
        ld      de,quartetsPatterns
        add     de,a
        ld      a,(de)              ; A = quartetData = quartetsPatterns[octet]
        pop     de                  ; D = idx7 (0..3)
    ; return two bits of quartetData selected by idx7
        inc     d
.prepareQuartetData:
        rrca
        rrca
        dec     d
        jr      nz,.prepareQuartetData
        and     $03
        ret
        
    MACRO PACK_OPCODE_PATTERN_DATA idx0, idx1, idx2, idx3
        DB  ((idx0<<2) | ((idx1)<<4) | ((idx2)<<6) | ((idx3)<<0))
    ENDM

quartetsPatterns:
    PACK_OPCODE_PATTERN_DATA 1,1,3,3 : PACK_OPCODE_PATTERN_DATA 1,1,3,2     ; C0..CF  idx7 0..3)
    PACK_OPCODE_PATTERN_DATA 1,1,3,2 : PACK_OPCODE_PATTERN_DATA 1,1,3,2     ; D0..DF  idx7 0..3)
    PACK_OPCODE_PATTERN_DATA 1,1,3,1 : PACK_OPCODE_PATTERN_DATA 1,1,3,1     ; E0..EF  idx7 0..3)
    PACK_OPCODE_PATTERN_DATA 1,1,3,1 : PACK_OPCODE_PATTERN_DATA 1,1,3,1     ; F0..FF  idx7 0..3)
    PACK_OPCODE_PATTERN_DATA 1,3,1,1 : PACK_OPCODE_PATTERN_DATA 1,1,1,1     ; 00..0F  idx7 0..3)
    PACK_OPCODE_PATTERN_DATA 2,3,1,1 : PACK_OPCODE_PATTERN_DATA 2,1,1,1     ; 10..1F  idx7 0..3)
    PACK_OPCODE_PATTERN_DATA 2,3,3,1 : PACK_OPCODE_PATTERN_DATA 2,1,3,1     ; 20..2F  idx7 0..3)
    PACK_OPCODE_PATTERN_DATA 2,3,3,1 : PACK_OPCODE_PATTERN_DATA 2,1,3,1     ; 30..3F  idx7 0..3)

    OPT pop     ; restore original configuration of sjasmplus syntax
