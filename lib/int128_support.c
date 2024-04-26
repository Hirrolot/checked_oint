// GCC's support for 128-bit integers [1] is quite castrated. This file defines
// some more constants and operations.
//
// [1]: https://gcc.gnu.org/onlinedocs/gcc/_005f_005fint128.html

#include <assert.h>
#include <stddef.h>

#define U128_MIN ((unsigned __int128)0)
#define U128_MAX (~U128_MIN)

#define I128_MIN (~I128_MAX)
#define I128_MAX ((__int128)(U128_MAX >> 1))

// The minimum size of a `char` array that is able to contain an arbitrary
// integer in decimal notation. Currently, it is equal to the number of
// characters in the minimum 128-bit signed number,
// `-170141183460469231731687303715884105728`, plus the null character.
#define MAX_INT_PRINT_SIZE (40 + /* the null character */ 1)

static const char digits[] = "0123456789";

static int map_digit(const char c) {
    switch (c) {
    case '0': return 0;
    case '1': return 1;
    case '2': return 2;
    case '3': return 3;
    case '4': return 4;
    case '5': return 5;
    case '6': return 6;
    case '7': return 7;
    case '8': return 8;
    case '9': return 9;
    default: return -1;
    }
}

static int print_u128_aux(
    unsigned __int128 n,
    char buffer[const restrict static MAX_INT_PRINT_SIZE]) {
    int i = 0;
    if (n >= 10) {
        i = print_u128_aux(n / 10, buffer);
    }

    buffer[i] = digits[n % 10];
    i++;

    return i;
}

static int print_u128(
    unsigned __int128 n,
    char buffer[const restrict static MAX_INT_PRINT_SIZE]) {
    const int i = print_u128_aux(n, buffer);
    buffer[i] = '\0';
    return i;
}

static int
print_i128(__int128 n, char buffer[const restrict static MAX_INT_PRINT_SIZE]) {
    int offset = 0;
    if (n < 0) {
        buffer[0] = '-';
        offset = 1;
    }

    unsigned __int128 unsigned_n = 0;
    if (I128_MIN == n) {
        // `I128_MIN` is irrepresentable as a positive integer.
        unsigned_n = (unsigned __int128)I128_MAX + 1;
    } else if (n < 0) {
        unsigned_n = (unsigned __int128)-n;
    } else {
        unsigned_n = (unsigned __int128)n;
    }

    return offset + print_u128(unsigned_n, buffer + offset);
}

#define FAIL_IF(cond)                                                          \
    if (cond) {                                                                \
        return -1;                                                             \
    }

static int
scan_u128(const char s[const restrict], unsigned __int128 *const restrict x) {
    assert(s);
    assert(x);

    FAIL_IF('\0' == s[0]);

    unsigned __int128 result = 0;
    for (size_t i = 0; s[i] != '\0'; i++) {
        // Overflow.
        FAIL_IF(result > U128_MAX / 10);
        result *= 10;

        const int digit = map_digit(s[i]);
        // An invalid character.
        FAIL_IF(-1 == digit);

        // Overflow.
        FAIL_IF(result > U128_MAX - (unsigned __int128)digit);

        result += (unsigned __int128)digit;
    }

    *x = result;
    return 0;
}

static int scan_i128(const char s[const restrict], __int128 *const restrict x) {
    assert(s);
    assert(x);

    FAIL_IF('\0' == s[0]);

    int offset = 0;
    int sign = 1;
    if ('-' == s[0]) {
        sign = -1;
        offset = 1;
    }

    unsigned __int128 result = 0;
    FAIL_IF(-1 == scan_u128(s + offset, &result));

    // Overflow.
    FAIL_IF(1 == sign && result > (unsigned __int128)I128_MAX);

    // Underflow.
    FAIL_IF(-1 == sign && result > (unsigned __int128)I128_MAX + 1);

    if (-1 == sign && (unsigned __int128)I128_MAX + 1 == result) {
        // `I128_MIN` is irrepresentable as a positive integer.
        *x = I128_MIN;
        return 0;
    }

    *x = sign * (__int128)result;
    return 0;
}

#undef FAIL_IF
