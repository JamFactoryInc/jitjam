
#include "bytecode.h"
#include "runtime_values.h"
#include "variable_bounds.h"
#include <map>


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

BlockId InstructionProcessor::add_block(const Block &block) {
    this->blocks[block.id.id] = block;
    return block.id;
}

Block &InstructionProcessor::borrow_block(const BlockId &block_id) {
    return blocks[block_id.id];
}

void InstructionProcessor::compile() {
    for (auto &block: this->blocks) {
        block.recompute_captured_aliases();
    }

    this->fold_consts();

    for (auto &block: this->blocks) {
        this->compile_block(block);
    }
}

void const_eval(const std::vector<ConstraintValue> &values, const Instruction &instr) {

    if (is_binary_operation(instr.code)) {
        auto &variant = instr.variant.binary;
        auto &lhs_value = values[variant.lhs.id];
        auto &rhs_value = values[variant.rhs.id];
        auto &dst_value = values[variant.dst.id];

        switch (instr.code) {
            case Op::Code::LessThan:
                break;
            case Op::Code::LessThanEqual:
                break;
            case Op::Code::GreaterThan:
                break;
            case Op::Code::GreaterThanEqual:
                break;
            case Op::Code::Equal:
                break;
            case Op::Code::NotEqual:
                break;
            case Op::Code::Add:
                break;
            case Op::Code::Subtract:
                break;
            case Op::Code::Multiply:
                break;
            case Op::Code::Divide:
                break;
            case Op::Code::Mod:
                break;
            case Op::Code::BitXor:
                break;
            case Op::Code::BitOr:
                break;
            case Op::Code::BitAnd:
                break;
        }
    }

    if (is_unary_operation(instr.code)) {
        auto &variant = instr.variant.unary;
        auto &src_value = values[variant.src.id];
        auto &dst_value = values[variant.dst.id];

        switch (instr.code) {
            case Op::Code::BitCompliment:
                break;
            case Op::Code::Negate:
                break;
            case Op::Code::Return:
                break;
            case Op::Code::CastToInt:
                break;
            case Op::Code::CastToFloat:
                break;
            case Op::Code::Move:
                break;
        }
    }
}

void InstructionProcessor::fold_consts() {
    // can be indexed-into via VariableId
    auto variable_values = std::vector<ConstraintValue>(this->variables.size());

    // map of variables that are required to evaluate instructions -> the instructions that requires them
    auto variable_definition_dependencies = std::multimap<VariableId, Instruction>();

    for (auto &block: this->blocks) {
        for (auto &item: block.variable_instructions) {
            auto &var_id = item.first;
            auto &instructions = item.second;

            auto assignment_instr = instructions[0];
            if (!is_const(assignment_instr.code)) {

                // if this var depends on others, add them to the dependency map under this variable id
                for (auto &var_dependency: assignment_instr.read_variables()) {
                    variable_definition_dependencies.insert(std::make_pair(var_dependency, assignment_instr));
                }

                variable_values[var_id.id] = ConstraintValue::uninit();

                continue;
            }

            // the assignment is a constant, so add it to our map of constants along with its value
            variable_values[var_id.id] = ConstraintValue::from_const_instr(assignment_instr);
        }
    }

    for (auto &entry: variable_definition_dependencies) {
        auto &var_id = entry.first;
        auto &instr = entry.second;
        auto &variable_value = variable_values[var_id.id];

        if (variable_value.is_constant()) {

        }
    }
}

void InstructionProcessor::compile_block(Block &block) {

}

















