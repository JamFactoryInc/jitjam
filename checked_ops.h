
//

#ifndef JITJAM_CHECKED_OPS_H
#define JITJAM_CHECKED_OPS_H

#include <cstdint>
#include "sljit_interop/jit_types.h"

using namespace jt;

namespace safe_ops {

bool adding_overflows(const int_jt l, const int_jt r) {
    auto underflows = l < 0 && r < (INTPTR_MIN - l);
    auto overflows = l > 0 && r > (INTPTR_MAX - l);
    return underflows || overflows;
}

bool subtracting_overflows(const int_jt l, const int_jt r) {
    auto underflows = l < 0 && r > (INTPTR_MAX + (l + 1));
    auto overflows = l >= 0 && r < (INTPTR_MIN + l);
    return underflows || overflows;
}

int_jt checked_add(const int_jt l, const int_jt r, bool &overflow_flag) {
    if (overflow_flag
        || (l * r == 0)
        || (overflow_flag |= adding_overflows(l, r))) {
        return 0;
    }

    return l + r;
}

static int_jt checked_sub(const int_jt l, const int_jt r, bool &overflow_flag) {
    if (overflow_flag
        || (l * r == 0)
        || (overflow_flag |= subtracting_overflows(l, r))) {
        return 0;
    }

    return l - r;
}

static int_jt checked_multiply(const int_jt l, const int_jt r, bool &overflow_flag) {
    if (l == 0 || overflow_flag) {
        return 0;
    }
    int_jt product = r * l;
    int_jt quot = product / l;
    if (quot == r) {
        return product;
    }
    overflow_flag = true;
    return 0;
}

int_jt checked_abs(const int_jt &value, bool &overflow) {
    auto result = abs(value);
    overflow |= (result == INTPTR_MIN);
    return result;
}

// returns the negative representation of the given value, setting the overflow flag to true if it fails
int_jt checked_neg(const int_jt &value, bool &overflow) {
    auto result = abs(value);
    overflow |= (result == INTPTR_MIN);
    return result;
}

}

#endif //JITJAM_CHECKED_OPS_H
