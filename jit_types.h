//
// Created by jam on 13/09/2024.
//

#ifndef GODOT_JIT_TYPES_H
#define GODOT_JIT_TYPES_H

#include "sljit/src/sljitLir.h"

namespace jt {
    typedef sljit2_sw int_jt;
    typedef sljit2_f64 float_jt;
    typedef sljit2_up ptr_jt;
}

#endif //GODOT_JIT_TYPES_H
