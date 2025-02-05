//
// Created by jam on 16/09/2024.
//

#ifndef JITJAM_BYTECODE_H
#define JITJAM_BYTECODE_H

#include <cstdint>
#include "mem.h"

enum Opcode {
    ADD,
    SUB,
    MUL,
    DIV,
    MOD,

    SHL,
    SHR,

    NEG_F,

    ADD_F,
    SUB_F,
    MUL_F,
    DIV_F,

    BITOR,
    BITAND,
    BITXOR,

    OR,
    AND,
    XOR,
    NOT,

    LT,
    LTE,
    GT,
    GTE,
    EQ,
    NEQ,

    INT_TO_FLOAT,
    FLOAT_TO_INT,

    MOVE,
    MOVE_F,

    SET_CONST,
    SET_CONST_F,

    JUMP,
    JUMP_IF_EQ,
    JUMP_IF_NEQ,
    JUMP_IF_LT,
    JUMP_IF_LTE,
    JUMP_IF_GT,
    JUMP_IF_GTE,

    RET,
    RET_F,
    RET_V,
};

#define ARGS(op, w) case op: return w;

static int args(Opcode op) {
    switch (op) {
        ARGS(ADD, 3)
        ARGS(SUB, 3)
        ARGS(MUL, 3)
        ARGS(DIV, 3)
        ARGS(MOD, 3)

        ARGS(SHL, 3)
        ARGS(SHR, 3)

        ARGS(NEG_F, 2)

        ARGS(ADD_F, 3)
        ARGS(SUB_F, 3)
        ARGS(MUL_F, 3)
        ARGS(DIV_F, 3)

        ARGS(BITOR, 3)
        ARGS(BITAND, 3)
        ARGS(BITXOR, 3)

        ARGS(OR, 3)
        ARGS(AND, 3)
        ARGS(XOR, 3)
        ARGS(NOT, 2)

        ARGS(LT, 3)
        ARGS(LTE, 3)
        ARGS(GT, 3)
        ARGS(GTE, 3)
        ARGS(EQ, 3)
        ARGS(NEQ, 3)

        ARGS(INT_TO_FLOAT, 2)
        ARGS(FLOAT_TO_INT, 2)

        ARGS(SET_CONST, 2)
        ARGS(SET_CONST_F, 2)

        ARGS(MOVE, 2)
        ARGS(MOVE_F, 2)

        ARGS(JUMP, 1)
        ARGS(JUMP_IF_EQ, 2)
        ARGS(JUMP_IF_NEQ, 2)
        ARGS(JUMP_IF_LT, 2)
        ARGS(JUMP_IF_LTE, 2)
        ARGS(JUMP_IF_GT, 2)
        ARGS(JUMP_IF_GTE, 2)

        ARGS(RET, 1)
        ARGS(RET_F, 1)
        ARGS(RET_V, 0)
    }
}

struct JumpLabel {
    int32_t id;
};

union Instr {
public:
    Opcode _op;
    Mem _mem;
    JumpLabel _jump;
    float_jt _float;

    Instr() { };

    Instr(Opcode op): _op(op) { };
    Instr(Mem mem): _mem(mem) { };
    Instr(JumpLabel jump): _jump(jump) { };
    Instr(float_jt float_val): _float(float_val) { };

    bool is_jump() {
        return _op >= JUMP & _op <= JUMP_IF_GTE;
    }

    int args() {
        return ::args(_op);
    }

    Instr *next() {
        return this + (args() + 1) * sizeof(Instr);
    }

    float_jt float_const() {
        // only SET_CONST_F uses float const, and it is always the first arg
        return (this + sizeof(Instr))->_float;
    }

    Mem src1() {
        return (this + sizeof(Instr))->_mem;
    }

    Mem src2() {
        return (this + 2 * sizeof(Instr))->_mem;
    }

    JumpLabel jump() {
        // jump is always the last arg (mutually exclusive with dst)
        return (this + args() * sizeof(Instr))->_jump;
    }

    Mem dst() {
        // dst is always the last arg (mutually exclusive with jump)
        return (this + args() * sizeof(Instr))->_mem;
    }
};

#endif //JITJAM_BYTECODE_H
