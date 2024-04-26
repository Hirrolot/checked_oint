#include "int128_support.c"

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

// This is a workaround to force dynamic linking, see
// <https://github.com/ocaml/dune/issues/10461#issuecomment-2082149852>.
void checked_oint_force_link(void) {}

#define X(bitness, ty)                                                         \
    int checked_oint_bit_not_i##bitness(const int x) {                         \
        return (int)(~(ty)x);                                                  \
    }

X(8, int8_t)
X(16, int16_t)

#undef X

#define X(name, bitness, ty, op)                                               \
    int checked_oint_##name##_i##bitness(const int x, const int y) {           \
        return (int)((ty)x op y);                                              \
    }

X(shift_left, 8, int8_t, <<)
X(shift_right, 8, int8_t, >>)
X(shift_left, 16, int16_t, <<)
X(shift_right, 16, int16_t, >>)

#undef X

#define X(signedness, bitness, ty)                                             \
    int checked_oint_print_##signedness##bitness(                              \
        const int##bitness##_t x,                                              \
        char buffer[const restrict static MAX_INT_PRINT_SIZE]) {               \
        return sprintf(buffer, "%" PRI##signedness##bitness, (ty)x);           \
    }

X(u, 32, uint32_t)
X(u, 64, uint64_t)

#undef X

#define X(postfix, int_ty, f, max_int_ty, int_min, int_max)                    \
    int checked_oint_scan_##postfix(                                           \
        const char s[const restrict], int_ty *const restrict x,                \
        const int base) {                                                      \
        assert(s);                                                             \
        assert(x);                                                             \
        assert(2 == base || 8 == base || 10 == base || 16 == base);            \
                                                                               \
        if (isspace(s[0]) || '\0' == s[0]) {                                   \
            return -1;                                                         \
        }                                                                      \
                                                                               \
        errno = 0;                                                             \
        char *end = NULL;                                                      \
        const max_int_ty result = f(s, &end, base);                            \
        if (end[0] != '\0') {                                                  \
            /* There are some uninterpreted characters left. */                \
            return -1;                                                         \
        }                                                                      \
        _Pragma("GCC diagnostic push");                                        \
        _Pragma("GCC diagnostic ignored \"-Wtype-limits\"");                   \
        if (ERANGE == errno || result < int_min || result > int_max) {         \
            return -1;                                                         \
        }                                                                      \
        _Pragma("GCC diagnostic pop");                                         \
                                                                               \
        *x = result;                                                           \
        return 0;                                                              \
    }

X(u32, int32_t, strtoumax, uintmax_t, 0, UINT32_MAX)
X(u64, int64_t, strtoumax, uintmax_t, 0, UINT64_MAX)
X(i32, int32_t, strtoimax, intmax_t, INT32_MIN, INT32_MAX)
X(i64, int64_t, strtoimax, intmax_t, INT64_MIN, INT64_MAX)
X(int, int, strtoimax, intmax_t, INT_MIN, INT_MAX)

#undef X

#define X(signedness, gcc_ty, min_int, max_int)                                \
    struct signedness##128 {                                                   \
        uint64_t high, low;                                                    \
    };                                                                         \
                                                                               \
    static struct signedness##128 wrap_##signedness##128(const gcc_ty x) {     \
        return (struct signedness##128){                                       \
            .high = (uint64_t)(x >> 64), .low = (uint64_t)x};                  \
    }                                                                          \
                                                                               \
    static gcc_ty unwrap_##signedness##128(const struct signedness##128 x) {   \
        return (gcc_ty)((gcc_ty)x.high << 64 | (gcc_ty)x.low);                 \
    }                                                                          \
                                                                               \
    X_AUX(                                                                     \
        signedness, gcc_ty, min_int, max_int, wrap_##signedness##128,          \
        unwrap_##signedness##128)

#define X_AUX(signedness, gcc_ty, min_int, max_int, wrap, unwrap)              \
    bool checked_oint_equal_##signedness##128(                                 \
        const struct signedness##128 x, const struct signedness##128 y) {      \
        return unwrap(x) == unwrap(y);                                         \
    }                                                                          \
                                                                               \
    int checked_oint_compare_##signedness##128(                                \
        const struct signedness##128 x, const struct signedness##128 y) {      \
        if (unwrap(x) < unwrap(y)) {                                           \
            return -1;                                                         \
        } else if (unwrap(x) > unwrap(y)) {                                    \
            return 1;                                                          \
        } else {                                                               \
            return 0;                                                          \
        }                                                                      \
    }                                                                          \
                                                                               \
    struct signedness##128 checked_oint_min_##signedness##128(void) {          \
        return wrap(min_int);                                                  \
    }                                                                          \
                                                                               \
    struct signedness##128 checked_oint_max_##signedness##128(void) {          \
        return wrap(max_int);                                                  \
    }                                                                          \
                                                                               \
    struct signedness##128 checked_oint_of_int_unchecked_##signedness##128(    \
        const int x) {                                                         \
        return wrap((gcc_ty)x);                                                \
    }                                                                          \
                                                                               \
    int checked_oint_print_##signedness##128(                                  \
        const struct signedness##128 x,                                        \
        char buffer[const restrict static MAX_INT_PRINT_SIZE]) {               \
        return print_##signedness##128(unwrap(x), buffer);                     \
    }                                                                          \
                                                                               \
    /* TODO: support the `base` argument. */                                   \
    int checked_oint_scan_##signedness##128(                                   \
        const char s[const restrict],                                          \
        struct signedness##128 *const restrict x, const int base) {            \
        assert(s);                                                             \
        assert(x);                                                             \
        assert(2 == base || 8 == base || 10 == base || 16 == base);            \
                                                                               \
        _Pragma("GCC diagnostic push");                                        \
        _Pragma("GCC diagnostic ignored \"-Wunused-value\"");                  \
        assert(                                                                \
            ("Binary, octal, and hexadecimal 128-bit numbers are not "         \
             "currently supported",                                            \
             10 == base));                                                     \
        _Pragma("GCC diagnostic pop");                                         \
                                                                               \
        gcc_ty result = 0;                                                     \
        if (0 == scan_##signedness##128(s, &result)) {                         \
            *x = wrap(result);                                                 \
            return 0;                                                          \
        }                                                                      \
                                                                               \
        return -1;                                                             \
    }                                                                          \
                                                                               \
    X_OP2(signedness, add, +, wrap, unwrap)                                    \
    X_OP2(signedness, sub, -, wrap, unwrap)                                    \
    X_OP2(signedness, mul, *, wrap, unwrap)                                    \
    X_OP2(signedness, div, /, wrap, unwrap)                                    \
    X_OP2(signedness, rem, %, wrap, unwrap)                                    \
    X_OP1(signedness, bit_not, ~, wrap, unwrap)                                \
    X_OP2(signedness, bit_or, |, wrap, unwrap)                                 \
    X_OP2(signedness, bit_and, &, wrap, unwrap)                                \
    X_OP2(signedness, bit_xor, ^, wrap, unwrap)                                \
    X_OP2(signedness, shift_left, <<, wrap, unwrap)                            \
    X_OP2(signedness, shift_right, >>, wrap, unwrap)

#define X_OP1(signedness, name, op, wrap, unwrap)                              \
    struct signedness##128 checked_oint_##name##_##signedness##128(            \
        const struct signedness##128 x) {                                      \
        return wrap(op unwrap(x));                                             \
    }
#define X_OP2(signedness, name, op, wrap, unwrap)                              \
    struct signedness##128 checked_oint_##name##_##signedness##128(            \
        const struct signedness##128 x, const struct signedness##128 y) {      \
        return wrap(unwrap(x) op unwrap(y));                                   \
    }

X(u, unsigned __int128, U128_MIN, U128_MAX)
X(i, __int128, I128_MIN, I128_MAX)

#undef X
#undef X_AUX
#undef X_OP1
#undef X_OP2
