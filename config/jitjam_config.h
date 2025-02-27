
//

#ifndef JITJAM_JITJAM_CONFIG_H
#define JITJAM_JITJAM_CONFIG_H

#include <map>
#include "operator_overloads.h"
#include "type_conversions.h"
#include "../compiler/opcodes.h"
#include "../handles/handle_impls.h"

using namespace jt;

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
    std::unordered_map<TypeConversion, VariableTypeId> type_conversions;

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

#endif //JITJAM_JITJAM_CONFIG_H
