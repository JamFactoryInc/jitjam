
//

#ifndef JITJAM_OPERATOR_OVERLOADS_H
#define JITJAM_OPERATOR_OVERLOADS_H

#include "../handles/handle_impls.h"
#include "../compiler/opcodes.h"

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

#endif //JITJAM_OPERATOR_OVERLOADS_H
