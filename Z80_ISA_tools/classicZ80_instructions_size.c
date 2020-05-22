// prototype of function to calculate size of Z80 opcode
//
// © Peter Helcmanovsky 2020, license: https://opensource.org/licenses/MIT
//
// this is meant to be reasonably fast while as small+simple as possible to make
// it possible in future to rewrite it easily in full assembly

#include <stdint.h>

typedef uint8_t byte;

const byte patternDataA[] = {
    2, 1, 1, 1, 1, 3, 1, 1, 3, 3
};

#define pattern1111 (patternDataA + 1)
#define pattern1131 (patternDataA + 3)
#define pattern1311 (patternDataA + 4)
#define pattern2111 (patternDataA + 0)
#define pattern1133 (patternDataA + 6)
const byte pattern1132[4] = { 1, 1, 3, 2 };
const byte pattern2131[4] = { 2, 1, 3, 1 };
const byte pattern2311[4] = { 2, 3, 1, 1 };
const byte pattern2331[4] = { 2, 3, 3, 1 };

const byte* quartetsPatterns[16] = {
    pattern1133, pattern1132,   // C0..CF (idx7 0..3)
    pattern1132, pattern1132,   // D0..DF (idx7 0..3)
    pattern1131, pattern1131,   // E0..EF (idx7 0..3)
    pattern1131, pattern1131,   // F0..FF (idx7 0..3)
    pattern1311, pattern1111,   // 00..0F (idx7 0..3)
    pattern2311, pattern2111,   // 10..1F (idx7 0..3)
    pattern2331, pattern2131,   // 20..2F (idx7 0..3)
    pattern2331, pattern2131    // 30..3F (idx7 0..3)
};

byte opcodeSz(byte *b) {
    if (0xCD == (b[0] & 0xCF)) {                // call ** / ED / IXY
        if (0xCD == b[0]) return 3;             // call **
        if (0xED == b[0]) {                     // extended instructions
            if (0x43 == (b[1] & 0xC7)) return 4;// ld (**),r16, ld r16,(**)
            //TODO Z80N extras not covered here
            return 2;                           // everything else
        }
        // IXY instructions
        if (0xDD == (b[1] & 0xDF)) return 1;    // prevent recursion for DD DD FD FD DD FD arrays
        if (0xCB == b[1]) return 4;             // bit-prefix instruction
        byte extraL =   (0x34 <= b[1] && b[1] <= 0x36) ||   // inc/dec/ld "(hl)"
                        (0x70 <= b[1] && b[1] <= 0x77) ||   // ld (hl),r8
                        (0x40 <= b[1] && b[1] <= 0xBF && 6 == (b[1] & 0x07));   // (hl)
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
    return quartetsPatterns[octet][idx7];
}
