
//

#ifndef JITJAM_RUNTIME_VALUES_H
#define JITJAM_RUNTIME_VALUES_H

#include "../../../sljit_interop/jit_types.h"
#include "../../../handles/handle_impls.h"

using namespace jt;

enum RuntimeValueType {
    Int,
    Float
};

union RuntimeValue {
    int_jt int_value;
    float_jt float_value;
    VariableId referenced_variable;

    RuntimeValue(): int_value(0l) {}
    RuntimeValue(int_jt int_value): int_value(int_value) {}
    RuntimeValue(float_jt float_value): float_value(float_value) {}
    RuntimeValue(VariableId referenced_variable): referenced_variable(referenced_variable) {}
};

#endif //JITJAM_RUNTIME_VALUES_H
