//
// Created by jam on 16/09/2024.
//

#ifndef JITJAM_OPTIMIZER_H
#define JITJAM_OPTIMIZER_H

#include <unordered_map>
#include <vector>
#include "mem.h"
#include "bytecode.h"

struct GraphNode {

};

struct CodeGraph {

};

struct InstrData {
    int reads_since_write;
    int last_write;
    int last_read;
    union {
        int_jt _int;
        float_jt _float;
    } consteval_value;
    bool is_float;
    bool is_const = false;
};

class Optimizer {
    std::unordered_map<Mem, InstrData> instr_data;
    std::vector<Instr> instr_buffer;

    void write_jump(JumpLabel label) {
        instr_buffer.push_back(Instr(Opcode::JUMP));
        instr_buffer.push_back(Instr(label));
    }

    void write_jump_cnd(Opcode op, JumpLabel label, Mem cond) {
        instr_buffer.push_back(Instr(op));
        instr_buffer.push_back(Instr(label));
        instr_buffer.push_back(Instr(cond));
    }

    void write_op1(Opcode op, Instr src, Mem dst) {
        instr_buffer.push_back(Instr(op));
        instr_buffer.push_back(src);
        instr_buffer.push_back(Instr(dst));
    }

    void write_op2(Opcode op, Mem src1, Mem src2, Mem dst) {
        instr_buffer.push_back(Instr(op));
        instr_buffer.push_back(Instr(src1));
        instr_buffer.push_back(Instr(src2));
        instr_buffer.push_back(Instr(dst));
    }

    std::vector<Instr> optimize(std::vector<Instr> instrs) {
        const auto *first_instr = instrs.data();
        auto *instr = instrs.data();

        while (true) {
            int op_arg_count = instr->args();
            int instr_idx = instr - first_instr;
            if (instr->is_jump()) {

            }

            switch (instr->_op) {
                case Opcode::SET_CONST: {
                    auto &data = instr_data[instr->dst()];
                    data.consteval_value._int = instr->src1().arg_2;
                    data.is_float = false;
                    data.is_const = true;
                    data.last_write = instr_idx;
                    break;
                }
                case Opcode::SET_CONST_F: {
                    auto &data = instr_data[instr->dst()];
                    data.consteval_value._float = instr->src1().arg_2;
                    data.is_float = true;
                    data.is_const = true;
                    data.last_write = instr_idx;
                    break;
                }
                case Opcode::MOVE: {
                    auto &src_data = instr_data[instr->src1()];
                    auto &dst_data = instr_data[instr->dst()];
                    src_data.last_read = instr_idx;
                    dst_data.last_write = instr_idx;
                    if (src_data.is_const) {
                        dst_data.is_const = true;
                        dst_data.is_float = false;
                        dst_data.consteval_value = src_data.consteval_value;
                        write_op1(Opcode::SET_CONST, Mem::integral_const(src_data.consteval_value._int), instr->dst());
                        continue;
                    }
                    break;
                }
                case Opcode::MOVE_F: {
                    auto &src_data = instr_data[instr->src1()];
                    auto &dst_data = instr_data[instr->dst()];
                    src_data.last_read = instr_idx;
                    dst_data.last_write = instr_idx;
                    if (src_data.is_const) {
                        dst_data.is_const = true;
                        dst_data.is_float = true;
                        dst_data.consteval_value = src_data.consteval_value;
                        write_op1(Opcode::SET_CONST_F, Instr(src_data.consteval_value._float), instr->dst());
                        continue;
                    }
                    break;
                }
                default: switch (op_arg_count) {
                    case 3: {
                        auto src = instr->src2();
                        auto &data = instr_data[src];
                        data.last_read = instr_idx;
                    }
                    case 2: {
                        auto src = instr->src1();
                        auto &src_data = instr_data[src];
                        src_data.last_read = instr_idx;

                        auto dst = instr->dst();
                        auto &dst_data = instr_data[dst];
                        src_data.last_write = instr_idx;
                    }
                }
            }

            instr = instr->next();
        }

    }
};

#endif //JITJAM_OPTIMIZER_H
