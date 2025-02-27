
#include "../utils.h"
#include "jit_types.h"
#include "../sljit/src/sljitLir.h"

#ifndef JITJAM_ARG_TYPE_UTILS_H
#define JITJAM_ARG_TYPE_UTILS_H

using namespace jt;



enum ABIArgType {
    TYPE_VOID = SLJIT2_ARG_TYPE_RET_VOID,
    TYPE_WORD = SLJIT2_ARG_TO_TYPE(W),
    TYPE_POINTER = SLJIT2_ARG_TO_TYPE(P),
    TYPE_FLOAT = SLJIT2_ARG_TO_TYPE(F64),
};

class FunctionSignature {
    const sljit2_s32 ARRAY_ARGS = -1;

    sljit2_s32 packed_signature = 0;

    explicit FunctionSignature(sljit2_s32 value) {
        this->packed_signature = value;
    }

public:

    /**
     * A signature representing
     * @code void (*)()
     */
    FunctionSignature() = default;

    /**
     * A signature representing
     * @code returnType (*)()
     */
    explicit FunctionSignature(ABIArgType return_type): FunctionSignature((sljit2_s32) return_type) {

    }

    /**
     * A signature representing
     * @code returnType (*)(arg1)
     */
    explicit FunctionSignature(ABIArgType return_type, ABIArgType arg1) {
        int value = return_type;
        value |= arg1 << 4;
        this->packed_signature = value;
    }

    /**
     * A signature representing
     * @code returnType (*)(arg1, arg2)
     */
    explicit FunctionSignature(ABIArgType return_type, ABIArgType arg1, ABIArgType arg2) {
        int value = return_type;
        value |= arg1 << 4;
        value |= arg2 << 8;
        this->packed_signature = value;
    }

    /**
     * A signature representing
     * @code returnType (*)(arg1, arg2, arg3)
     */
    explicit FunctionSignature(ABIArgType return_type, ABIArgType arg1, ABIArgType arg2, ABIArgType arg3) {
        int value = return_type;
        value |= arg1 << 4;
        value |= arg2 << 8;
        value |= arg3 << 12;
        this->packed_signature = value;
    }

    int get_reserved_int_safe_registers() {
        int reserved_count = 0;

        int value = this->packed_signature;
        // pop off the return type, since it uses a scratch register
        value >>= 4;

        while (value != 0) {
            int type = value & 0b1111;
            if (type == ABIArgType::TYPE_FLOAT) {
                ++reserved_count;
            }
            value >>= 4;
        }
        return reserved_count;
    }

    int get_reserved_float_safe_registers() {
        int reserved_count = 0;
        int value = this->packed_signature;
        // pop off the return type, since it uses a scratch register
        value >>= 4;

        while (value != 0) {
            int type = value & 0b1111;
            if (type == ABIArgType::TYPE_WORD || type == ABIArgType::TYPE_POINTER) {
                ++reserved_count;
            }
            value >>= 4;
        }
        return reserved_count;
    }

    sljit2_s32 as_sljit_args() const {
        return this->packed_signature;
    }
};

template<typename T>
struct ArgTypeOf {
    static const ABIArgType value;
    static int uses_float_reg();
    static int uses_int_reg();
};

template<>
struct ArgTypeOf<void> {
    static const ABIArgType value = ABIArgType::TYPE_VOID;
    static int uses_float_reg() { return false; }
    static int uses_int_reg() { return false; }
};
template<>
struct ArgTypeOf<int_jt> {
    static const ABIArgType value = ABIArgType::TYPE_WORD;
    static int uses_float_reg() { return false; }
    static int uses_int_reg() { return true; }
};
template<typename T>
struct ArgTypeOf<T*> {
    static const ABIArgType value = ABIArgType::TYPE_POINTER;
    static int uses_float_reg() { return false; }
    static int uses_int_reg() { return true; }
};
template<>
struct ArgTypeOf<float_jt> {
    static const ABIArgType value = ABIArgType::TYPE_FLOAT;
    static int uses_float_reg() { return true; }
    static int uses_int_reg() { return false; }
};

#endif //JITJAM_ARG_TYPE_UTILS_H
