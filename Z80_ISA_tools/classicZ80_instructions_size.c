// prototype of function to calculate size of Z80 opcode
//
// © Peter Helcmanovsky 2020, license: https://opensource.org/licenses/MIT
//
// this is meant to be reasonably fast while as small+simple as possible to make
// it possible in future to rewrite it easily in full assembly

#include <stdint.h>

typedef uint8_t byte;

#define PACK_PATTERN_DATA(idx0, idx1, idx2, idx3) ((idx0) | ((idx1)<<2) | ((idx2)<<4) | ((idx3)<<6))

const byte quartetsPatterns[16] = {
    PACK_PATTERN_DATA(1,1,3,3), PACK_PATTERN_DATA(1,1,3,2),     // C0..CF (idx7 0..3)
    PACK_PATTERN_DATA(1,1,3,2), PACK_PATTERN_DATA(1,1,3,2),     // D0..DF (idx7 0..3)
    PACK_PATTERN_DATA(1,1,3,1), PACK_PATTERN_DATA(1,1,3,1),     // E0..EF (idx7 0..3)
    PACK_PATTERN_DATA(1,1,3,1), PACK_PATTERN_DATA(1,1,3,1),     // F0..FF (idx7 0..3)
    PACK_PATTERN_DATA(1,3,1,1), PACK_PATTERN_DATA(1,1,1,1),     // 00..0F (idx7 0..3)
    PACK_PATTERN_DATA(2,3,1,1), PACK_PATTERN_DATA(2,1,1,1),     // 10..1F (idx7 0..3)
    PACK_PATTERN_DATA(2,3,3,1), PACK_PATTERN_DATA(2,1,3,1),     // 20..2F (idx7 0..3)
    PACK_PATTERN_DATA(2,3,3,1), PACK_PATTERN_DATA(2,1,3,1)      // 30..3F (idx7 0..3)
};

byte opcodeSz(byte *b) {
    if (0xCD == (b[0] & 0xCF)) {                // call ** / ED / IXY
        if (0xCD == b[0]) return 3;             // call **
        if (0xED == b[0]) {                     // extended instructions
            if (0x43 == (b[1] & 0xC7)) return 4;// ld (**),r16, ld r16,(**)
            return 2;                           // everything else
        }
        // IXY instructions
        if (0xDD == (b[1] & 0xDF)) return 1;    // prevent recursion for DD DD FD FD DD FD arrays
        if (0xED == b[1]) return 1;             // ED prefix invalidates DD/FD prefix
        if (0xCB == b[1]) return 4;             // bit-prefix instruction
        if (0x76 == b[1]) return 2;             // IXY prefixed `halt`
        byte extraL =   (0x34 <= b[1] && b[1] <= 0x36) ||   // inc/dec/ld "(hl)"
                        (0x70 <= b[1] && b[1] <= 0x77) ||   // ld (hl),r8
                        (0x40 <= b[1] && b[1] <= 0xBF && 6 == (b[1] & 0x07));   // ld r8,(hl), and/or/xor... (hl)
        return 1 + extraL + opcodeSz(b+1);      // +1 or +2 prefix of regular instruction
    }

    const byte x0 = b[0] + 0x40;
    if (0x80 <= x0) return 1;                   // all 0x40 .. 0xBF instructions
    const byte idx7 = x0 & 0x07;
    // for 4 == idx7: Cx..Fx have 3B (call cc), 0x..3x have 1B (inc/dec)
    if (0x04 == idx7) return (x0 < 0x40) ? 3 : 1;
    // for 5..7 idx7 the pattern is 1,2,1 (CD/DD/ED/FD are already processed)
    if (0x05 <= idx7) return 2 - (idx7&1);
    // for 0..3 idx7 the pattern differs between many octets, so just use table
    // 1133 .... | 1132 ....    ; C0..CF
    // 1132 .... | 1132 ....    ; D0..DF
    // 1131 .... | 1131 ....    ; E0..EF
    // 1131 .... | 1131 ....    ; F0..FF
    // 1311 .... | 1111 ....    ; 00..0F
    // 2311 .... | 2111 ....    ; 10..1F
    // 2331 .... | 2131 ....    ; 20..2F
    // 2331 .... | 2131 ....    ; 30..3F
    const byte octet = x0>>3;
    const byte quartetData = quartetsPatterns[octet];
    return (quartetData>>(idx7*2)) & 0x03;
}
