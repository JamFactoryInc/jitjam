//
// Created by jam on 16/09/2024.
//

#ifndef JITJAM_BYTECODE_H
#define JITJAM_BYTECODE_H

#include <cstdint>
#include <utility>
#include <typeindex>
#include "handles.h"
#include "../codegen/mem.h"

using namespace jt;

namespace Op {

    enum Code {
        Noop,

        // ops that require 2 args and 1 dst
        BINARY_OPS,

        LessThan,
        LessThanEqual,
        GreaterThan,
        GreaterThanEqual,
        Equal,
        NotEqual,

        Add,
        Subtract,
        Multiply,
        Divide,
        Mod,
        BitXor,
        BitOr,
        BitAnd,

        END_BINARY_OPS,

        // ops that require 1 arg and 1 dst
        UNARY_OPS,

        BitCompliment,
        Negate,
        Return,

        CastToInt,
        CastToFloat,

        Move,

        END_UNARY_OPS,

        SetConstInt,
        SetConstFloat,

        Jump,
        CondJump,

    };

    // ops that require 2 args and 1 dst
    bool is_binary_operation(Code code) {
        return code > BINARY_OPS && code < END_BINARY_OPS;
    }

    // ops that require 1 arg and 1 dst
    bool is_unary_operation(Code code) {
        return code > UNARY_OPS && code < END_UNARY_OPS;
    }

    bool is_any_jump_operation(Code code) {
        return code == Code::Jump || code == Code::CondJump;
    }

    bool is_cond_jump_operation(Code code) {
        return code == Code::CondJump;
    }

    bool is_uncond_jump_operation(Code code) {
        return code == Code::Jump;
    }

    bool is_return(Code code) {
        return code == Code::Return;
    }

    bool is_noop(Code code) {
        return code == Code::Noop;
    }

    bool is_const(Code code) {
        return code == Code::SetConstFloat || code == Code::SetConstInt;
    }

    const char* op_symbol(Code code) {
        switch (code)
        {
            case BINARY_OPS:
            case END_BINARY_OPS:
            case UNARY_OPS:
            case END_UNARY_OPS:
                return "<Control Op>";
            case LessThan: return "<";
            case LessThanEqual: return "<=";
            case GreaterThan: return ">";
            case GreaterThanEqual: return ">=";
            case Equal: return "==";
            case NotEqual: return "!=";
            case Add: return "+";
            case Subtract: return "-";
            case Multiply: return "*";
            case Divide: return "/";
            case Mod: return "%";
            case BitXor: return "^";
            case BitOr: return "|";
            case BitAnd: return "&";
            case BitCompliment: return "~";
            case Negate: return "-";
            case Return: return "return";
            case Move: return "move";
            case CastToInt: return "(int)";
            case CastToFloat: return "(float)";
            case Noop: return "<noop>";
            case Jump: return "<jump>";
            case CondJump: return "<cond jump>";
            case SetConstInt: return "<set const int>";
            case SetConstFloat: return "<set const float>";
        }
    }
}

struct VariableType {
    VariableTypeId type_id;
    enum Type: uint16_t {
        Integer,
        Float,
        Struct,
    } type;
    // size of the type in bytes
    uint16_t size;
};

struct Instruction;
struct InstructionProcessor;
class Var {
    friend class InstructionProcessor;
    friend class Block;

    AliasId alias_id;
    VariableTypeId type_id;

public:
    VariableId id;

public:
    Var(VariableId &id, AliasId &alias_id, const VariableTypeId &type_id):
        id(id),
        alias_id(alias_id),
        type_id(type_id) { }
};

struct ConstValue;
typedef Id<ConstValue> ConstValueId;
struct ConstValue {
    enum ValueType {
        ConstInt,
        ConstFloat,
    } value_type;
    union {
        int_jt int_const;
        float_jt float_const;
    } value;
};

struct OperatorOverload {
    Op::Code opcode;
    VariableTypeId lhs_type;
    VariableTypeId rhs_type;

    bool operator==(const OperatorOverload &other) const {
        return opcode == other.opcode
            && lhs_type == other.lhs_type
            && rhs_type == other.rhs_type;
    }
};
template<>
struct std::hash<OperatorOverload> {
    size_t operator()(const OperatorOverload& operator_key) const
    {
        return operator_key.opcode | (
            (
                operator_key.rhs_type.encoded_type.hash_code()
                 ^ (operator_key.lhs_type.encoded_type.hash_code() << 1)
            ) << 5
        );
    }
};

struct CastOverload {
    VariableTypeId src_type;
    VariableTypeId dst_type;
};
template<>
struct std::hash<CastOverload> {
    size_t operator()(const CastOverload& cast_key) const
    {
        return cast_key.src_type.encoded_type.hash_code() ^ (cast_key.dst_type.encoded_type.hash_code() << 1);
    }
};

/**
 * Welcome to JitJam! \n
 * \n
 * This lib will provide you with the basics to compile a reasonably performant native binary on the fly, using minimal basic components: @c Vars and @c Blocks \n
 * \n
 *
 * Vars are references to runtime values. Each Var refers primarily to a unique value, but also to the sequence of values associated with a variable name. \n
 * When receiving a Var from a function, assume that it represents is a unique & constant value. \n 
 * Reassignment is done by shadowing aliases. \n 
 * \n 
 *
 * To demonstrate, consider this example:
 * @code
 * {
 *     int x;
 *     x = 1;
 *     x = 2;
 * }
 * @endcode
 * 
 * Here, @c x is an alias: a unique label, but one with multiple values. \n 
 * Vars only represent one unique runtime value of a semantic variable, so to achieve this in JitJam, one would do:
 * @code
 * Var x = block.declare();
 * block.set(1, x);
 * block.set(2, x);
 * @endcode
 * Here, @c block.set() mutates the passed @c x reference to be a new Var that represents the constant @c 1 \n
 * \n 
 * However, when we declare @c x , it is given a unique alias. This alias is maintained when we mutate the @c x variable here. \n 
 * \n 
 * Effectively all we did is override the value of @c x for when the block completes. \n 
 * \n 
 * This underlying behaviour -- that a @c Var is a single value overriding an alias in a local context -- is important to understand and keep in mind when working with JitJam
 */
class JitJamConfig
{
    friend struct Block;

    std::unordered_map<OperatorOverload, VariableTypeId> operator_definitions;
    std::unordered_map<CastOverload, VariableTypeId> type_conversions;

    void register_binary_operator(
        Op::Code op, const VariableTypeId &lhs_type, const VariableTypeId &rhs_type,
        const VariableTypeId &resulting_type
    )
    {
        auto overload = OperatorOverload{.opcode=op, .lhs_type=lhs_type, .rhs_type=rhs_type,};
        this->operator_definitions[overload] = resulting_type;
    }

    JitJamConfig()
    {
        // ints

        register_binary_operator(Op::Code::Add, VariableTypeId::ISIZE, VariableTypeId::ISIZE, VariableTypeId::ISIZE);
        register_binary_operator(Op::Code::Subtract, VariableTypeId::ISIZE, VariableTypeId::ISIZE, VariableTypeId::ISIZE);
        register_binary_operator(Op::Code::Multiply, VariableTypeId::ISIZE, VariableTypeId::ISIZE, VariableTypeId::ISIZE);
        register_binary_operator(Op::Code::Divide, VariableTypeId::ISIZE, VariableTypeId::ISIZE, VariableTypeId::ISIZE);
        register_binary_operator(Op::Code::Mod, VariableTypeId::ISIZE, VariableTypeId::ISIZE, VariableTypeId::ISIZE);

        register_binary_operator(Op::Code::BitAnd, VariableTypeId::ISIZE, VariableTypeId::ISIZE, VariableTypeId::ISIZE);
        register_binary_operator(Op::Code::BitOr, VariableTypeId::ISIZE, VariableTypeId::ISIZE, VariableTypeId::ISIZE);
        register_binary_operator(Op::Code::BitXor, VariableTypeId::ISIZE, VariableTypeId::ISIZE, VariableTypeId::ISIZE);

        register_binary_operator(Op::Code::Equal, VariableTypeId::ISIZE, VariableTypeId::ISIZE, VariableTypeId::BOOL);
        register_binary_operator(Op::Code::NotEqual, VariableTypeId::ISIZE, VariableTypeId::ISIZE, VariableTypeId::BOOL);
        register_binary_operator(Op::Code::GreaterThan, VariableTypeId::ISIZE, VariableTypeId::ISIZE, VariableTypeId::BOOL);
        register_binary_operator(Op::Code::GreaterThanEqual, VariableTypeId::ISIZE, VariableTypeId::ISIZE, VariableTypeId::BOOL);
        register_binary_operator(Op::Code::LessThan, VariableTypeId::ISIZE, VariableTypeId::ISIZE, VariableTypeId::BOOL);
        register_binary_operator(Op::Code::LessThanEqual, VariableTypeId::ISIZE, VariableTypeId::ISIZE, VariableTypeId::BOOL);

        // floats

        register_binary_operator(Op::Code::Add, VariableTypeId::FSIZE, VariableTypeId::FSIZE, VariableTypeId::FSIZE);
        register_binary_operator(Op::Code::Subtract, VariableTypeId::FSIZE, VariableTypeId::FSIZE, VariableTypeId::FSIZE);
        register_binary_operator(Op::Code::Multiply, VariableTypeId::FSIZE, VariableTypeId::FSIZE, VariableTypeId::FSIZE);
        register_binary_operator(Op::Code::Divide, VariableTypeId::FSIZE, VariableTypeId::FSIZE, VariableTypeId::FSIZE);

        register_binary_operator(Op::Code::Equal, VariableTypeId::FSIZE, VariableTypeId::FSIZE, VariableTypeId::BOOL);
        register_binary_operator(Op::Code::NotEqual, VariableTypeId::FSIZE, VariableTypeId::FSIZE, VariableTypeId::BOOL);
        register_binary_operator(Op::Code::GreaterThan, VariableTypeId::FSIZE, VariableTypeId::FSIZE, VariableTypeId::BOOL);
        register_binary_operator(Op::Code::GreaterThanEqual, VariableTypeId::FSIZE, VariableTypeId::FSIZE, VariableTypeId::BOOL);
        register_binary_operator(Op::Code::LessThan, VariableTypeId::FSIZE, VariableTypeId::FSIZE, VariableTypeId::BOOL);
        register_binary_operator(Op::Code::LessThanEqual, VariableTypeId::FSIZE, VariableTypeId::FSIZE, VariableTypeId::BOOL);
    }
};

union InstructionVariant {
    struct SetConstInt {
        VariableId variable_id;
        int_jt value;
    } const_int;

    struct SetConstFloat {
        VariableId variable_id;
        float_jt value;
    } const_float;

    struct Unary {
        VariableId src;
        VariableId dst;
    } unary;

    // used immediately after a declaration to initialize the value
    struct Binary {
        VariableId lhs;
        VariableId rhs;
        VariableId dst;
    } binary;

    struct Jump {
        BlockId target;
    } jump;

    struct ConditionalJump {
        VariableId condition_var;
        BlockId truthy_target;
        BlockId falsy_target;
    } cond_jump;

    struct BlockStart {

    } block_start;

    InstructionVariant() {}
    InstructionVariant(Binary binary): binary(binary) {}
    InstructionVariant(Unary move): unary(move) {}
    InstructionVariant(Jump jump): jump(jump) {}
    InstructionVariant(ConditionalJump cond_jump): cond_jump(cond_jump) {}
    InstructionVariant(SetConstInt const_int): const_int(const_int) {}
    InstructionVariant(SetConstFloat const_float): const_float(const_float) {}

};

struct Instruction {
    static thread_local std::vector<VariableId> temp_buffer;

    Op::Code code;
    InstructionVariant variant;

    Instruction(Op::Code opcode, InstructionVariant variant):
        code(opcode),
        variant(variant) { }

    static Instruction noop() {
        return {
            Op::Code::Noop,
            InstructionVariant()
        };
    }

    static Instruction binary(Op::Code opcode, const Var &lhs, const Var &rhs, const Var &dst) {
        return {
            opcode,
            InstructionVariant::Binary { .lhs=lhs.id, .rhs=rhs.id, .dst=dst.id }
        };
    }

    static Instruction unary(Op::Code opcode, const Var &src, const Var &dst) {
        return {
            opcode,
            InstructionVariant::Unary { .src=src.id, .dst=dst.id }
        };
    }

    static Instruction jump(const BlockId &target_block) {
        return {
            Op::Code::Jump,
            InstructionVariant::Jump { .target=target_block }
        };
    }

    static Instruction cond_jump(const VariableId &condition, const BlockId &truthy_block, const BlockId &falsy_block) {
        return {
            Op::Code::CondJump,
            InstructionVariant::ConditionalJump { .condition_var=condition, .truthy_target=truthy_block, .falsy_target=falsy_block }
        };
    }

    static Instruction set(const Var &src, const Var &dst) {
        return unary(Op::Code::Move, src, dst);
    }

    static Instruction set_const(const int_jt int_value, const Var &dst) {
        return {
            Op::Code::SetConstInt,
            InstructionVariant::SetConstInt { .variable_id=dst.id, .value=int_value }
        };
    }

    static Instruction set_const(const float_jt float_value, const Var &dst) {
        return {
            Op::Code::SetConstFloat,
            InstructionVariant::SetConstFloat { .variable_id=dst.id, .value=float_value }
        };
    }

    VariableId get_written_variable_opt() const {
        if (is_binary_operation(this->code)) {
            return this->variant.binary.dst;
        }

        if (is_unary_operation(this->code)) {
            return this->variant.unary.dst;
        }

        if (this->code == Op::Code::SetConstInt) {
            return this->variant.const_int.variable_id;
        }

        if (this->code == Op::Code::SetConstFloat) {
            return this->variant.const_float.variable_id;
        }

        return VariableId::invalid();
    }

    // returns a reference to a vector containing all the read-only variable ids required for this operation
    const std::vector<VariableId> &read_variables() const {
        temp_buffer.clear();

        if (is_binary_operation(this->code)) {
            temp_buffer.emplace_back(this->variant.binary.lhs);
            if (this->variant.binary.lhs != this->variant.binary.rhs) {
                temp_buffer.emplace_back(this->variant.binary.rhs);
            }
            return temp_buffer;
        }

        if (is_unary_operation(this->code)) {
            temp_buffer.emplace_back(this->variant.unary.src);
            return temp_buffer;
        }

        if (this->code == Op::Code::CondJump) {
            temp_buffer.emplace_back(this->variant.cond_jump.condition_var);
            return temp_buffer;
        }

        return temp_buffer;
    }
};
thread_local std::vector<VariableId> Instruction::temp_buffer = {};

class InstructionProcessor {
    friend class Block;

    JitJamConfig *config;
    // indexed-into by BlockId
    std::vector<Block> blocks = {};
    std::vector<Var> variables = {};

    int var_id_counter = 0;
    int alias_id_counter = 0;
    int block_id_counter = 0;

    InstructionProcessor(JitJamConfig *config) {
        this->config = config;
    }

    VariableId unique_var_id() {
        return var_id_counter++;
    }

    AliasId unique_alias_id() {
        return alias_id_counter++;
    }

    BlockId unique_block_id() {

        BlockId new_id = block_id_counter++;
        const size_t capacity = blocks.capacity();
        if (capacity <= new_id.id) {
            blocks.reserve(capacity);
        }

        return new_id;
    }

    Var declare_variable(const VariableTypeId &var_type_id) {
        auto new_var_id = unique_var_id();
        auto new_alias_id = unique_alias_id();

        Var new_var = { new_var_id, new_alias_id, var_type_id };
        variables.emplace_back(new_var);
        return new_var;
    }

    Var shadow_alias(const Var &shadowed_var) {
        auto new_var_id = unique_var_id();
        auto shadowed_alias_id = shadowed_var.alias_id;

        Var new_var = { new_var_id, shadowed_alias_id, shadowed_var.type_id };
        variables.emplace_back(new_var);
        return new_var;
    }

public:

    BlockId add_block(const Block &block);
    Block &borrow_block(const BlockId &block_id);
    void compile();
    void compile_block(Block &block);
    void fold_consts();
};

class Block {
    friend class InstructionProcessor;

    InstructionProcessor *instruction_processor;
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

    Block(InstructionProcessor *processor) {
        this->instruction_processor = processor;
        this->config = processor->config;
        this->id = processor->unique_block_id();
    }

    // captures the alias if it is not declared by this block
    void require_var(const Var &referenced_var) {
        if (!this->declares_alias(referenced_var.alias_id)) {
            this->captured_aliases.insert(referenced_var.alias_id);
        }
    }

    bool declares_alias(const AliasId &alias_id) {
        return this->declared_aliases.find(alias_id) != this->declared_aliases.end();
    }

    // creates an initial instruction record for this variable,
    // with the output of the given instruction as this variable's one assignment
    void define(Var &var, Instruction &instr) {
        this->variable_instructions[var.id] = std::vector<Instruction>({ instr });
    }

    // declares a new Var with a unique alias, parented to the current block
    Var declare(const VariableTypeId &type_id) {
        return this->instruction_processor->declare_variable(type_id);
    }

    // creates a new Var that overrides the alias definition for this block
    // the Var is initialized with the given instruction
    Var shadow_alias(Var &old_var) {
        Var new_var = this->instruction_processor->shadow_alias(old_var);
        this->alias_overrides[new_var.alias_id] = new_var.id;
        return new_var;
    }

    // records the instruction all non-modified associated variables
    void record_instruction(const Instruction &instr) {
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

    static bool is_assignable(const VariableTypeId &src, const VariableTypeId &dst) {
        return src == dst;
    }

    static void assert_assignable(const VariableTypeId &src, const VariableTypeId &dst) {
        if (!Block::is_assignable(src, dst)) {
            utils::raise(
                "Cannot assign value of type '{}' to a variable of type '{}'.\n"
                         "If this behaviour is desired, ensure an explicit cast is registered via the JitJamConfig, and perform the explicit cast before this assignment.",
            { src.type_name(), dst.type_name() }
            );
        }
    }

    bool is_block_writable() const {
        return this->terminating_instruction.code == Op::Code::Noop;
    }

    void assert_writable() {
        if (!this->is_block_writable()) {
            utils::raise("Block may no longer be written-to after a jump, conditional jump, or return", {});
        }
    }

    // gets the resulting type of a binary operation, according to the registered operations in the JitJamConfig
    VariableTypeId get_output_type(const Op::Code &operation, const VariableTypeId &lhs, const VariableTypeId &rhs) {
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
    void bubble_aliases(const std::set<AliasId> &child_aliases) {
        for (const AliasId &alias: child_aliases) {
            this->captured_aliases.insert(alias);
            if (!this->declares_alias(alias)) {
                this->captured_aliases.insert(alias);
            }
        }
    }

    // computes (and caches as the captured_aliases value) the required aliases for this block and its following blocks
    std::set<AliasId> &recompute_captured_aliases() {
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

public:
    void jump(BlockId &target) {
        this->assert_writable();

        Block &target_block = this->instruction_processor->borrow_block(target);
        target_block.preceding_blocks.insert(this->id);

        auto instruction = Instruction::jump(target);
        this->record_instruction(instruction);
    }

    void jump_if(Var &condition, BlockId &if_true_block, BlockId &if_false_block) {
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
    Var add(const Var &lhs, const Var &rhs) {
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
    void set(const Var &src, Var &dst) {
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
    void set(int_jt const_value, Var &dst) {
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
    void set(float_jt const_value, Var &dst) {
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
    Var declare(const Var &src_value, const VariableType &new_var_type) {
        this->assert_writable();
        Block::assert_assignable(src_value.type_id, new_var_type.type_id);

        this->require_var(src_value);

        Var new_var = this->declare(new_var_type.type_id);

        auto instruction = Instruction::set(src_value, new_var);
        this->define(new_var, instruction);
        this->record_instruction(instruction);

        return new_var;
    }
};


#endif //JITJAM_BYTECODE_H
