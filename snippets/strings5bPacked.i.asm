    MODULE str5b

;--------------------------------------------------------------------------------------------
; Decodes 5-bit+null-terminated encoded string from address DE to address HL
; modifies: AF, C, DE, HL
; code length: 19 bytes
; ! remember the decoded values are 0..31, not ASCII.
; ! for the price of +2B and 0 becoming '@' you can convert to ASCII with added `set 6,c`
; Authors of code: Busy, Baze, Zilog (Ped - reviews + sjasmplus encoder)

decode      ld      a,%1000'0000
.readChar   ld      c,%0000'1000
.bitLoop    add     a,a
            jr      nz,.readBit
            ld      a,(de)
            inc     de
            adc     a,a
.readBit    rl      c
            jr      nc,.bitLoop
        ;   set     6,c     ; +2B to convert 0 -> '@', 1..26 -> 'A'..'Z', 27..31 -> "[\]^_"
            ld      (hl),c
            inc     hl
            jr      nz,.readChar
            ret

;--------------------------------------------------------------------------------------------
; Macro to encode string literal to 5b null terminated string, requires DEVICE memory!
; valid chars:  ABC..XYZ[\]^_
; become value: 1 .. 31  (encoded to 5-bit chunks, starting at top bit of first byte)
; invalid chars will become some 0..31 value by truncating ASCII code (@ becomes zero, etc)
; ! The macro will destroy content of 1+strlen(literal?) bytes of DEVICE memory !

        MACRO str5b_encode literal?
            OPT push listmc
            DEFINE cur_packed_bits ((.inPtr-.str)*5 - ($-.str)*8)
.str:       DZ literal?     ; parse + store into memory regular ASCII encoded string + \0
.packedEnd  EQU     .str + (($ - .str) * 5 + 7) / 8 ; expected end address after packing
.inPtr = .str
.packed = 0
            ORG     .str
            WHILE $ < .packedEnd
.packed = (.packed<<5) | ({b .inPtr}&$1F)
.inPtr = .inPtr + 1
                IF 8 <= cur_packed_bits
                    DB  (.packed >>> (cur_packed_bits - 8)) & $FF
                ENDIF
            ENDW
            UNDEFINE cur_packed_bits
            OPT pop
        ENDM

;--------------------------------------------------------------------------------------------

numOfExampleStrings     EQU     15

example_Packed:

.s1:        str5b_encode 'ABCD[\]^_'

.s2:        DB          %00000'111  ; just zero terminator (unpacks as empty string)

.s3:        str5b_encode 'A'

.s4:        str5b_encode 'AB'

.s5:        str5b_encode 'ABC'

.s6:        str5b_encode 'ABCD'

.s7:        str5b_encode 'ABCDE'

.s8:        str5b_encode 'ABCDEF'

.s9:        str5b_encode 'ABCDEFG'

.sA:        str5b_encode 'ABCDEFGH'

.sB:        str5b_encode 'ABCDEFGHI'

.sC:        str5b_encode 'ABCDEFGHIJ'

.sD:        str5b_encode 'ABCDEFGHIJK'

.sE:        str5b_encode 'ABCDEFGHIJKL'

.sF:        str5b_encode 'ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_'  ; all 31 valid chars

            ; warning - the DEVICE memory beyond this packed area is modified
            ; by the encoding macro during assembling -> following machine code
            ; need to be defined *after* in source to be not overwritten by encoding.

    DISPLAY "str5b: 133 bytes worth of strings encoded into ",/D,$-.s1," bytes (",/D,($-.s1)*100/133,"%)"
            ; 90 bytes, ~67.7% of original size

example_Unpacked:
            DS  134,$AA  ; $AA is debug marker, to see how the values are unpacked
            ; after all test strings are unpacked, single $AA should be left intact

    ENDMODULE
