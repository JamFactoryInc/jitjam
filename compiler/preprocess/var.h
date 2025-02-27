
//

#ifndef JITJAM_VAR_H
#define JITJAM_VAR_H

#include "../../sljit_interop/jit_types.h"
#include "../../handles/handle_impls.h"

using namespace jt;

struct Instruction;
struct PreProcEngine;
class Var {
    friend class PreProcEngine;
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

#endif //JITJAM_VAR_H
