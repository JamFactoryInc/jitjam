
#include "block.h"



Block::Block(PreProcEngine *processor) {
    this->instruction_processor = processor;
    this->config = processor->config;
    this->id = processor->unique_block_id();
}

// captures the alias if it is not declared by this block
void Block::require_var(const Var &referenced_var) {
    if (!this->declares_alias(referenced_var.alias_id)) {
        this->captured_aliases.insert(referenced_var.alias_id);
    }
}

bool Block::declares_alias(const AliasId &alias_id) {
    return this->declared_aliases.find(alias_id) != this->declared_aliases.end();
}

// creates an initial instruction record for this variable,
// with the output of the given instruction as this variable's one assignment
void Block::define(Var &var, Instruction &instr) {
    this->variable_instructions[var.id] = std::vector<Instruction>({ instr });
}

// declares a new Var with a unique alias, parented to the current block
Var Block::declare(const VariableTypeId &type_id) {
    return this->instruction_processor->declare_variable(type_id);
}

// creates a new Var that overrides the alias definition for this block
// the Var is initialized with the given instruction
Var Block::shadow_alias(Var &old_var) {
    Var new_var = this->instruction_processor->shadow_alias(old_var);
    this->alias_overrides[new_var.alias_id] = new_var.id;
    return new_var;
}

// records the instruction all non-modified associated variables
void Block::record_instruction(const Instruction &instr) {
    auto &variables = instr.read_variables();

    for (auto &var_id: variables) {
        this->variable_instructions[var_id].emplace_back(instr);
    }

    if (Op::is_any_jump_operation(instr.code)) {
        if (instr.code == Op::Code::CondJump) {
            auto &variant = instr.variant.cond_jump;
            this->variable_instructions[variant.condition_var.id].emplace_back(instr);
        }
        this->terminating_instruction = instr;
    }
}

bool Block::is_assignable(const VariableTypeId &src, const VariableTypeId &dst) {
    return src == dst;
}

void Block::assert_assignable(const VariableTypeId &src, const VariableTypeId &dst) {
    if (!Block::is_assignable(src, dst)) {
        utils::raise(
            "Cannot assign value of type '{}' to a variable of type '{}'.\n"
            "If this behaviour is desired, ensure an explicit cast is registered via the JitJamConfig, and perform the explicit cast before this assignment.",
            { src.type_name(), dst.type_name() }
        );
    }
}

bool Block::is_block_writable() const {
    return this->terminating_instruction.code == Op::Code::Noop;
}

void Block::assert_writable() {
    if (!this->is_block_writable()) {
        utils::raise("Block may no longer be written-to after a jump, conditional jump, or return", {});
    }
}

// gets the resulting type of a binary operation, according to the registered operations in the JitJamConfig
VariableTypeId Block::get_output_type(const Op::Code &operation, const VariableTypeId &lhs, const VariableTypeId &rhs) {
    auto operator_key = OperatorOverload { .opcode=operation, .lhs_type=lhs, .rhs_type=rhs };
    auto result = this->config->operator_definitions.find(operator_key);
    if (result != this->config->operator_definitions.end()) {
        return result->second;
    }

    auto symbol = Op::op_symbol(operation);
    utils::raise("Unregistered operation '{}' between types {} and {}", { symbol, lhs.type_name(), rhs.type_name() });
    // unreachable
    return VariableTypeId::NIL;
}

// copies captured aliases from the child's alias captures, unless this block declares the alias
void Block::bubble_aliases(const std::set<AliasId> &child_aliases) {
    for (const AliasId &alias: child_aliases) {
        this->captured_aliases.insert(alias);
        if (!this->declares_alias(alias)) {
            this->captured_aliases.insert(alias);
        }
    }
}

// computes (and caches as the captured_aliases value) the required aliases for this block and its following blocks
std::set<AliasId> &Block::recompute_captured_aliases() {
    if (this->is_finalised) {
        return this->captured_aliases;
    }
    this->is_finalised = true;

    auto &ip = this->instruction_processor;
    auto &instr = this->terminating_instruction;
    auto &code = instr.code;

    if (is_block_writable()) {
        utils::raise("Unterminated block cannot be compiled", {});
    } else if (is_cond_jump_operation(code)) {
        std::set<AliasId> &truthy_aliases = ip->borrow_block(instr.variant.cond_jump.truthy_target).recompute_captured_aliases();
        std::set<AliasId> &falsy_aliases = ip->borrow_block(instr.variant.cond_jump.falsy_target).recompute_captured_aliases();

        this->bubble_aliases(truthy_aliases);
        this->bubble_aliases(falsy_aliases);
    } else if (is_uncond_jump_operation(code)) {
        std::set<AliasId> &aliases = ip->borrow_block(instr.variant.cond_jump.falsy_target).recompute_captured_aliases();
        this->bubble_aliases(aliases);
    } else if (is_return(code)) {
        // do nothing
    } else {
        utils::raise("Illegal terminating instruction {}. This is likely a bug in JitJam, unless you're doing something funky", { op_symbol(instr.code) });
    }

    return this->captured_aliases;
}

void Block::jump(BlockId &target) {
    this->assert_writable();

    Block &target_block = this->instruction_processor->borrow_block(target);
    target_block.preceding_blocks.insert(this->id);

    auto instruction = Instruction::jump(target);
    this->record_instruction(instruction);
}

void Block::jump_if(Var &condition, BlockId &if_true_block, BlockId &if_false_block) {
    this->assert_writable();

    Block &truthy_block = this->instruction_processor->borrow_block(if_true_block);
    truthy_block.preceding_blocks.insert(this->id);

    Block &falsy_block = this->instruction_processor->borrow_block(if_false_block);
    falsy_block.preceding_blocks.insert(this->id);

    auto instruction = Instruction::cond_jump(condition.id, truthy_block.id, falsy_block.id);
    this->record_instruction(instruction);
}

// returns a new variable containing the sum of the runtime values of 'lhs' and 'rhs'
// if the associated types of 'lhs' and 'rhs' do not have a registered Add operation, this function aborts
Var Block::add(const Var &lhs, const Var &rhs) {
    this->assert_writable();

    this->require_var(lhs);
    this->require_var(rhs);

    VariableTypeId resulting_type = this->get_output_type(Op::Code::Add, lhs.type_id, rhs.type_id);

    Var result = declare(resulting_type);
    auto instruction = Instruction::binary(Op::Code::Add, lhs, rhs, result);

    this->define(result, instruction);
    this->record_instruction(instruction);

    return result;
}

// Creates a new variable that shadows the existing 'dst' variable, with this new variable being a copy of the runtime value of 'src'.
// This is the SSA version of variable reassignment.
//
// For convenience, this function also reassigns the passed '&dst' reference to the new variable to parallel the shadowing of the runtime 'dst' variable
//
// Keep in mind, though, that reassigning a `Var&` itself does not modify the resulting runtime behaviour
void Block::set(const Var &src, Var &dst) {
    this->assert_writable();
    Block::assert_assignable(src.type_id, dst.type_id);

    this->require_var(dst);
    this->require_var(src);

    Var alias = this->shadow_alias(dst);
    auto instruction = Instruction::set(src, alias);
    this->define(alias, instruction);
    this->record_instruction(instruction);

    dst = alias;
}

// Creates a new runtime constant with the const_value provided.
//
// For convenience, this function also reassigns the passed '&dst' reference to the new variable to parallel the shadowing of the runtime 'dst' variable
void Block::set(int_jt const_value, Var &dst) {
    this->assert_writable();
    Block::assert_assignable(VariableTypeId::ISIZE, dst.type_id);

    this->require_var(dst);

    Var new_var = this->shadow_alias(dst);
    auto instruction = Instruction::set_const(const_value, new_var);

    this->define(new_var, instruction);
    this->record_instruction(instruction);

    dst = new_var;
}

// Creates a new runtime constant with the const_value provided.
//
// For convenience, this function also reassigns the passed '&dst' reference to the new variable to parallel the shadowing of the runtime 'dst' variable
void Block::set(float_jt const_value, Var &dst) {
    this->assert_writable();
    Block::assert_assignable(VariableTypeId::FSIZE, dst.type_id);

    this->require_var(dst);


    Var new_var = this->shadow_alias(dst);
    auto instruction = Instruction::set_const(const_value, new_var);

    this->define(new_var, instruction);
    this->record_instruction(instruction);

    dst = new_var;
}

// declares a new variable with a given type and a value copied from a given variable
Var Block::declare(const Var &src_value, const VariableType &new_var_type) {
    this->assert_writable();
    Block::assert_assignable(src_value.type_id, new_var_type.type_id);

    this->require_var(src_value);

    Var new_var = this->declare(new_var_type.type_id);

    auto instruction = Instruction::set(src_value, new_var);
    this->define(new_var, instruction);
    this->record_instruction(instruction);

    return new_var;
}