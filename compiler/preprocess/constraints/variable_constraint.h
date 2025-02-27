
//

#ifndef JITJAM_VARIABLE_CONSTRAINT_H
#define JITJAM_VARIABLE_CONSTRAINT_H

#include "../../../sljit_interop/jit_types.h"
#include "../../../handles/handles.h"
#include "constraint_value.h"

using namespace jt;

struct VariableConstraint {
    enum ConstraintType: uint32_t {
        UnInit                          = 0b0000000000000001,
        ConstantValue                   = 0b0000000000000010,
        EqualTo                         = 0b0000000000000100,
        NotEqualTo                      = 0b0000000000001000,
        LessThan                        = 0b0000000000010000,
        LessThanOrEqual                 = 0b0000000000100000,
        GreaterThan                     = 0b0000000001000000,
        GreaterThanOrEqual              = 0b0000000010000000,
        DivisibleBy                     = 0b0000000100000000,
        NotDivisibleBy                  = 0b0000001000000000,
    } constraint_type;

    ConstraintValue constraint_value;

    VariableConstraint(): constraint_type(ConstraintType::UnInit), constraint_value(ConstraintValue(0l)) { }
    VariableConstraint(ConstraintType constraint_type, ConstraintValue value):
        constraint_type(constraint_type), constraint_value(value) { }

    static VariableConstraint constant_value(const ConstraintValue &constraint_value) {
        return { ConstraintType::ConstantValue, constraint_value };
    }

    static VariableConstraint less_than(const ConstraintValue &constraint_value) {
        return { ConstraintType::LessThan, constraint_value };
    }

    static VariableConstraint less_than_or_eq(const ConstraintValue &constraint_value) {
        return { ConstraintType::LessThanOrEqual, constraint_value };
    }

    static VariableConstraint greater_than(const ConstraintValue &constraint_value) {
        return { ConstraintType::GreaterThan, constraint_value };
    }

    static VariableConstraint greater_than_or_eq(const ConstraintValue &constraint_value) {
        return { ConstraintType::GreaterThanOrEqual, constraint_value };
    }

    static VariableConstraint equal_to(const ConstraintValue &constraint_value) {
        if (constraint_value.is_constant()) {
            return { ConstraintType::ConstantValue, ConstraintValue(constraint_value) };
        }
        return { ConstraintType::EqualTo, ConstraintValue(constraint_value) };
    }

    static VariableConstraint not_equal_to(const ConstraintValue &constraint_value) {
        return { ConstraintType::NotEqualTo, constraint_value };
    }

    static VariableConstraint divisible_by(const ConstraintValue &constraint_value) {
        return { ConstraintType::DivisibleBy, ConstraintValue(constraint_value) };
    }

    static VariableConstraint not_divisible_by(const ConstraintValue &constraint_value) {
        return { ConstraintType::NotDivisibleBy, ConstraintValue(constraint_value) };
    }
};

#endif //JITJAM_VARIABLE_CONSTRAINT_H
