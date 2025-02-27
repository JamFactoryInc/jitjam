
//

#ifndef JITJAM_OPCODES_H
#define JITJAM_OPCODES_H

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

#endif //JITJAM_OPCODES_H
