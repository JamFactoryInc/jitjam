
#include "preprocess_engine.h"

PreProcEngine::PreProcEngine(JitJamConfig *config) {
    this->config = config;
}

VariableId PreProcEngine::unique_var_id() {
    return var_id_counter++;
}

AliasId PreProcEngine::unique_alias_id() {
    return alias_id_counter++;
}

BlockId PreProcEngine::unique_block_id() {

    BlockId new_id = block_id_counter++;
    const size_t capacity = blocks.capacity();
    if (capacity <= new_id.id) {
        blocks.reserve(capacity);
    }

    return new_id;
}

Var PreProcEngine::declare_variable(const VariableTypeId &var_type_id) {
    auto new_var_id = unique_var_id();
    auto new_alias_id = unique_alias_id();

    Var new_var = { new_var_id, new_alias_id, var_type_id };
    variables.emplace_back(new_var);
    return new_var;
}

Var PreProcEngine::shadow_alias(const Var &shadowed_var) {
    auto new_var_id = unique_var_id();
    auto shadowed_alias_id = shadowed_var.alias_id;

    Var new_var = { new_var_id, shadowed_alias_id, shadowed_var.type_id };
    variables.emplace_back(new_var);
    return new_var;
}

BlockId PreProcEngine::add_block(const Block &block) {
    this->blocks[block.id.id] = block;
    return block.id;
}

Block &PreProcEngine::borrow_block(const BlockId &block_id) {
    return blocks[block_id.id];
}

void PreProcEngine::compile() {
    for (auto &block: this->blocks) {
        block.recompute_captured_aliases();
    }

    this->fold_consts();

    for (auto &block: this->blocks) {
        this->compile_block(block);
    }
}

void PreProcEngine::fold_consts() {
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

void PreProcEngine::compile_block(Block &block) {

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
