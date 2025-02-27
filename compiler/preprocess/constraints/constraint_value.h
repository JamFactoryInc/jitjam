
//

#ifndef JITJAM_CONSTRAINT_VALUE_H
#define JITJAM_CONSTRAINT_VALUE_H

#include "../../../sljit_interop/jit_types.h"
#include "../../../handles/handle_impls.h"
#include "../instruction.h"

using namespace jt;

struct ConstraintValue {
    enum ValueType {
        // a value is not present yet
        UnInit,
        // a value is present, but not known at compile-time
        Unknown,
        IntValue,
        FloatValue,
        VariableReference,
    } value_type;
    union RuntimeValue {
        int_jt int_value;
        float_jt float_value;
        VariableId referenced_variable;

        RuntimeValue(int_jt int_value): int_value(int_value) {}
        RuntimeValue(float_jt float_value): float_value(float_value) {}
        RuntimeValue(VariableId referenced_variable): referenced_variable(referenced_variable) {}
    } value;

    ConstraintValue(): value_type(ValueType::UnInit), value(0l) {}
    ConstraintValue(ValueType value_type, RuntimeValue value): value_type(value_type), value(value) {}
    ConstraintValue(int_jt int_value): value_type(ValueType::IntValue), value(int_value) {}
    ConstraintValue(float_jt float_value): value_type(ValueType::FloatValue), value(float_value) {}
    ConstraintValue(VariableId referenced_variable): value_type(ValueType::VariableReference), value(referenced_variable) {}

    bool is_compatible(const ConstraintValue &other) const {
        return this->value_type == other.value_type;
    }

    bool is_uninit() const {
        return this->value_type == ValueType::UnInit;
    }

    bool is_int() const {
        return this->value_type == ValueType::IntValue;
    }

    bool is_float() const {
        return this->value_type == ValueType::FloatValue;
    }

    bool is_constant() const {
        return this->value_type == ValueType::IntValue || this->value_type == ValueType::FloatValue;
    }

    bool is_zero() const {
        if (this->is_int()) {
            return this->value.int_value == 0;
        }
        if (this->is_float()) {
            return this->value.float_value == 0.0;
        }
        return false;
    }

    static ConstraintValue uninit() {
        return { };
    }

    static ConstraintValue unknown() {
        return { ValueType::Unknown, 0l };
    }

    static ConstraintValue from_const_instr(const Instruction &instr) {
        if (instr.code == Op::Code::SetConstInt) {
            return { instr.variant.const_int.value };
        } else if (instr.code == Op::Code::SetConstFloat) {
            return { instr.variant.const_float.value };
        } else {
            return ConstraintValue::uninit();
        }
    }

    ConstraintValue try_add(const ConstraintValue &other) const {
        if (this->is_int() && other.is_int()) {
            return this->value.int_value + other.value.int_value;
        }

        if (this->is_float() && other.is_float()) {
            return this->value.float_value + other.value.float_value;
        }

        return ConstraintValue::unknown();
    }

    ConstraintValue try_sub(const ConstraintValue &other) const {
        if (this->is_int() && other.is_int()) {
            return this->value.int_value - other.value.int_value;
        }

        if (this->is_float() && other.is_float()) {
            return this->value.float_value - other.value.float_value;
        }

        return ConstraintValue::unknown();
    }

    ConstraintValue try_mul(const ConstraintValue &other) const {
        if (this->is_int() && other.is_int()) {
            return this->value.int_value * other.value.int_value;
        }
        if (this->is_int() && this->value.int_value == 0) {
            // 0 * x = 0
            return 0l;
        }
        if (other.is_int() && other.value.int_value == 0) {
            // 0 * x = 0
            return 0l;
        }

        if (this->is_float() && other.is_float()) {
            return this->value.float_value * other.value.float_value;
        }
        if (this->is_float() && this->value.float_value == 0) {
            // 0 * x = 0
            return 0.0;
        }
        if (other.is_float() && other.value.float_value == 0) {
            // 0 * x = 0
            return 0.0;
        }

        return ConstraintValue::unknown();
    }

    ConstraintValue try_div(const ConstraintValue &other) const {
        if (this->is_int()) {
            if (this->value.int_value == 0) {
                // 0 / x = 0
                return 0l;
            }
            if (other.is_int()) {
                return this->value.int_value / other.value.int_value;
            }
        }

        if (this->is_float() && other.is_float()) {
            return this->value.float_value * other.value.float_value;
        }
        if (this->is_float() && this->value.float_value == 0) {
            return 0.0;
        }
        if (other.is_float() && other.value.float_value == 0) {
            return 0.0;
        }

        return ConstraintValue::unknown();
    }

    ConstraintValue try_mod(const ConstraintValue &other) const {
        if (this->is_int()) {
            if (this->value.int_value == 0) {
                // 0 % x = 0
                return 0l;
            }
            if (other.is_int()) {
                return this->value.int_value % other.value.int_value;
            }
        }

        return ConstraintValue::unknown();
    }

    ConstraintValue try_less_than(const ConstraintValue &other) const {
        return ConstraintValue::unknown();
    }

    ConstraintValue try_less_than_or_equal(const ConstraintValue &other) const {
        return ConstraintValue::unknown();
    }

    ConstraintValue try_greater_than(const ConstraintValue &other) const {
        return ConstraintValue::unknown();
    }

    ConstraintValue try_greater_than_or_equal(const ConstraintValue &other) const {
        return ConstraintValue::unknown();
    }

    ConstraintValue try_equal(const ConstraintValue &other) const {
        return ConstraintValue::unknown();
    }

    ConstraintValue try_not_equal(const ConstraintValue &other) const {
        return ConstraintValue::unknown();
    }
};

#endif //JITJAM_CONSTRAINT_VALUE_H
