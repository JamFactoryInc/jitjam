
//

#ifndef JITJAM_BLOCK_H
#define JITJAM_BLOCK_H

#include "engine/preprocess_engine.h"
#include "../../sljit_interop/jit_types.h"
#include "../../handles/handle_impls.h"
#include "instruction.h"
#include "var.h"
#include "set"
#include "variable_type.h"

using namespace jt;

class Block {
    friend class PreProcEngine;

    PreProcEngine *instruction_processor;
    JitJamConfig *config;
    BlockId id;

    // aliases that are required by this block, but not declared in it
    // once finalised, this set contains any child dependencies as well
    std::set<AliasId> captured_aliases = {};

    // aliases that are declared in this block
    std::set<AliasId> declared_aliases = {};

    // aliases that override a captured alias
    // these values are used when a subsequent block fetches its captures
    std::unordered_map<AliasId, VariableId> alias_overrides = {};

    std::unordered_map<VariableId, std::vector<Instruction>> variable_instructions = {};

    std::set<BlockId> preceding_blocks = {};

    Instruction terminating_instruction = Instruction::noop();

    bool is_finalised = false;

    Block(PreProcEngine *processor);

    static bool is_assignable(const VariableTypeId &src, const VariableTypeId &dst);
    static void assert_assignable(const VariableTypeId &src, const VariableTypeId &dst);

    // captures the alias if it is not declared by this block
    void require_var(const Var &referenced_var);
    bool declares_alias(const AliasId &alias_id);

    // creates an initial instruction record for this variable,
    // with the output of the given instruction as this variable's one assignment
    void define(Var &var, Instruction &instr);

    // declares a new Var with a unique alias, parented to the current block
    Var declare(const VariableTypeId &type_id);

    // creates a new Var that overrides the alias definition for this block
    // the Var is initialized with the given instruction
    Var shadow_alias(Var &old_var);

    // records the instruction all non-modified associated variables
    void record_instruction(const Instruction &instr);
    bool is_block_writable() const;
    void assert_writable();

    // gets the resulting type of a binary operation, according to the registered operations in the JitJamConfig
    VariableTypeId get_output_type(const Op::Code &operation, const VariableTypeId &lhs, const VariableTypeId &rhs);

    // copies captured aliases from the child's alias captures, unless this block declares the alias
    void bubble_aliases(const std::set<AliasId> &child_aliases);

    // computes (and caches as the captured_aliases value) the required aliases for this block and its following blocks
    std::set<AliasId> &recompute_captured_aliases();

public:
    void jump(BlockId &target);
    void jump_if(Var &condition, BlockId &if_true_block, BlockId &if_false_block);

    // returns a new variable containing the sum of the runtime values of 'lhs' and 'rhs'
    // if the associated types of 'lhs' and 'rhs' do not have a registered Add operation, this function aborts
    Var add(const Var &lhs, const Var &rhs);

    // Creates a new variable that shadows the existing 'dst' variable, with this new variable being a copy of the runtime value of 'src'.
    // This is the SSA version of variable reassignment.
    //
    // For convenience, this function also reassigns the passed '&dst' reference to the new variable to parallel the shadowing of the runtime 'dst' variable
    //
    // Keep in mind, though, that reassigning a `Var&` itself does not modify the resulting runtime behaviour
    void set(const Var &src, Var &dst);

    // Creates a new runtime constant with the const_value provided.
    //
    // For convenience, this function also reassigns the passed '&dst' reference to the new variable to parallel the shadowing of the runtime 'dst' variable
    void set(int_jt const_value, Var &dst);

    // Creates a new runtime constant with the const_value provided.
    //
    // For convenience, this function also reassigns the passed '&dst' reference to the new variable to parallel the shadowing of the runtime 'dst' variable
    void set(float_jt const_value, Var &dst);

    // declares a new variable with a given type and a value copied from a given variable
    Var declare(const Var &src_value, const VariableType &new_var_type);
};

#endif //JITJAM_BLOCK_H
