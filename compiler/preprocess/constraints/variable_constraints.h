
//

#ifndef JITJAM_VARIABLE_CONSTRAINTS_H
#define JITJAM_VARIABLE_CONSTRAINTS_H

#include "../../../sljit_interop/jit_types.h"
#include "../../../handles/handles.h"
#include "constraint_value.h"
#include "variable_constraint.h"
#include "variable_bounds.h"

using namespace jt;

struct VariableConstraints {
    std::multimap<VariableConstraint::ConstraintType, VariableConstraint> constraints = {};
    VariableBounds bounds;
    VariableId constrained_variable;
    uint32_t constraint_flags = 0;

    void add_flag(const VariableConstraint::ConstraintType &type) {
        this->constraint_flags |= type;
    }

    void remove_flag(const VariableConstraint::ConstraintType &type) {
        this->constraint_flags &= (~type);
    }

    bool has_flag(const VariableConstraint::ConstraintType &type) const {
        return (this->constraint_flags & type) != 0;
    }

    void add_constraint(const VariableConstraint &constraint) {
        this->add_flag(constraint.constraint_type);
    }

    void set_constant(const ConstraintValue &constant_value) {
        // don't set changed_since_last_update since that's just used to try to narrow to a const value anyway
        if (!constant_value.is_constant()) {
            return;
        }
        this->constraint_flags = VariableConstraint::ConstraintType::ConstantValue;
        this->constraints.clear();
        this->constraints.insert(std::make_pair(
            VariableConstraint::ConstraintType::ConstantValue,
            VariableConstraint::constant_value(constant_value)
        ));
    }

    const ConstraintValue &get_constant() const {
        return this->constraints.find(VariableConstraint::ConstraintType::ConstantValue)->second.constraint_value;
    }

    bool is_constant() const {
        return this->has_flag(VariableConstraint::ConstraintType::ConstantValue);
    }

    static void try_add(
        VariableConstraints &lhs,
        VariableConstraints &rhs,
        VariableConstraints &dst
    ) {
        if (dst.is_constant()) {
            return;
        }

        if (lhs.is_constant() && rhs.is_constant()) {
            auto &lhs_value = lhs.get_constant();
            auto &rhs_value = rhs.get_constant();

            dst.set_constant(lhs_value.try_add(rhs_value));
            return;
        }

        if (!lhs.is_constant() && !rhs.is_constant()) {
            // we can't determine much about the sum if neither are constant and overflow is permitted
            return;
        }

        const auto &constant_arg = lhs.is_constant()
                                   ? lhs : rhs;
        const auto &non_constant_arg = lhs.is_constant()
                                       ? rhs : lhs;

        if (constant_arg.get_constant().is_zero()) {
            // adding 0 means the rhs is the same value as the dst
            dst.add_constraint(VariableConstraint::equal_to(non_constant_arg.constrained_variable));
            return;
        }



        // TODO: add a config to treat overflow as an error
        // this would allow us to assume x + 1 > x and x + -1 < x
    }
};

#endif //JITJAM_VARIABLE_CONSTRAINTS_H
