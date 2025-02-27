
//

#ifndef JITJAM_TYPE_CONVERSIONS_H
#define JITJAM_TYPE_CONVERSIONS_H

#include "../handles/handle_impls.h"
#include <map>

struct TypeConversion {
    VariableTypeId src_type;
    VariableTypeId dst_type;
};
template<>
struct std::hash<TypeConversion> {
    size_t operator()(const TypeConversion& cast_key) const
    {
        return cast_key.src_type.encoded_type.hash_code() ^ (cast_key.dst_type.encoded_type.hash_code() << 1);
    }
};

#endif //JITJAM_TYPE_CONVERSIONS_H
