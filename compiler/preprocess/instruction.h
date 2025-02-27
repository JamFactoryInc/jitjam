
//

#ifndef JITJAM_INSTRUCTION_H
#define JITJAM_INSTRUCTION_H

#include "../../sljit_interop/jit_types.h"
#include "../../handles/handle_impls.h"
#include "../opcodes.h"
#include "var.h"

using namespace jt;

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

#endif //JITJAM_INSTRUCTION_H
