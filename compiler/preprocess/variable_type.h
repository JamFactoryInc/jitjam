
//

#ifndef JITJAM_VARIABLE_TYPE_H
#define JITJAM_VARIABLE_TYPE_H

#include "../../sljit_interop/jit_types.h"
#include "../../handles/handle_impls.h"

using namespace jt;

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

#endif //JITJAM_VARIABLE_TYPE_H
