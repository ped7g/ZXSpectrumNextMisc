#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cassert>

/*
 * signed div50 "v1" routine
 * simulating routine (fixed point 8.8 x 3.5):
    bit     7,d
    ld      h,d
    ld      l,%101'00100            ; 5.125 in fixed point 3:5 form
    ld      d,l
    mul     de                      ; x0*y -> top byte is added to x1*y
    ld      a,d
    ex      de,hl
    mul     de                      ; x1*y
    add     de,a                    ; DE = top 16 bit of (x * 5.125) in fixed point 11:13
    jr      z,.x_pos
    ld      a,d
    sub     %101'00100              ; adjust result to be correct for negative x
    ld      d,a
.x_pos:
    ld      b,5
    bsra    de,b                    ; shift DE to become (x * 5.125 / 256) in fixed point 8:8
    ret
*/

static constexpr uint16_t divs50_v1_mul = 0b101'00100;  // value 5.125 encoded in fp3.5

int16_t divs50_fp88_fp35_v1(const int16_t in) {
    const uint8_t in_l = in & 0xFF;
    const uint8_t in_h = in >> 8;
    const uint16_t m1 = in_l * divs50_v1_mul;
    const uint16_t m2 = in_h * divs50_v1_mul;
    int32_t r = m2 + (m1 >> 8);             // in * 5.125 >> 3
    assert(0 <= r && r <= 0xFFFF);          // this must not overflow, the asm code assumes the result will fit into 16b
    if (in < 0) r -= (divs50_v1_mul<<8);    // fix unsigned multiplication result to signed
    r = int16_t(r) >> 5;                    // (in * 5.125) >> 8 == in / 49.95121951219512
    // the result for negative input values is almost always -1 to expected (0xFFFF -> 0xFFFF instead of 0x0000, etc)
    return r;
}

/*
 * signed div50 "v2" routine (after finding better adjust constant which makes whole routine only +1T slower)
 * simulating routine (fixed point 8.8 x 3.5):
    bit     7,d
    ld      h,d
    ld      l,%101'00100            ; 5.125 in fixed point 3:5 form
    ld      d,l
    mul     de                      ; x0*y -> top byte is added to x1*y
    ld      a,d
    ex      de,hl
    mul     de                      ; x1*y
    add     de,a                    ; DE = top 16 bit of (x * 5.125) in fixed point 11:13
    jr      z,.x_pos
    add     de,-(%101'00100<<8)+32  ; adjust result to be correct for negative x and improve rounding
.x_pos:
    ld      b,5
    bsra    de,b                    ; shift DE to become (x * 5.125 / 256) in fixed point 8:8
    ret
*/

int16_t divs50_fp88_fp35_v2(const int16_t in) {
    const uint8_t in_l = in & 0xFF;
    const uint8_t in_h = in >> 8;
    const uint16_t m1 = in_l * divs50_v1_mul;
    const uint16_t m2 = in_h * divs50_v1_mul;
    int32_t r = m2 + (m1 >> 8);             // in * 5.125 >> 3
    assert(0 <= r && r <= 0xFFFF);          // this must not overflow, the asm code assumes the result will fit into 16b
    if (in < 0) r += -(divs50_v1_mul<<8)+32;// fix unsigned multiplication result to signed and adjust inaccuracy
    r = int16_t(r) >> 5;                    // (in * 5.125) >> 8 == in / 49.95121951219512
    // the result is almost exact, except 16 input values (from negative side) are +1 closer to zero
    // when compared to "/ 50" the result is +-1, total absolute error 20305
    return r;
}

int main() {
    int16_t i = 0;
    int sumerr = 0;
    // table header if the output is used as CSV file
    printf("input i,as fp8.8,divs50_v2,as fp8.8,e,r,r,o,r,,i,n,,r,e,s,u,l,t");
    do {
        const int16_t expected = i / 50;
        const int16_t divs50 = divs50_fp88_fp35_v2(i);
        // output new-line, current `i` and div50 result every 64 values
        if (0 == (i&0x3F)) printf("\n%d,%3.2f,%d,%1.5f,", i, i / 256.0, divs50, divs50 / 256.0);
        const int err = int(divs50) - int(expected);
        printf("%d,", err);
        sumerr += abs(err);                 // update cumulative error sum with absolute error
    } while (++i);  // do all 64ki inputs
    printf("\nabserror,%d,avg. err,%1.5f\n", sumerr, sumerr / 65536.0);
}
