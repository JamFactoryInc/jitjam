
//

#ifndef JITJAM_HANDLES_H
#define JITJAM_HANDLES_H

#include <utility>
#include <typeindex>
#include <map>
#include "../../utils.h"
#include "../../sljit_interop/jit_types.h"

using namespace jt;

template <typename T>
struct Id {
    int id;

    Id(): id(-1) { }

    Id(int id): id(id) { }

    static Id<T> create(int id) {
        return Id(id);
    }

    bool operator==(const Id &other) const {
        return other.id == id;
    }

    bool operator!=(const Id &other) const {
        return other.id != id;
    }

    bool operator<(const Id &other) const {
        return id < other.id;
    }

    bool operator<=(const Id &other) const {
        return id <= other.id;
    }

    bool is_valid() {
        return this->id >= 0;
    }

    static Id<T> invalid() {
        return { };
    }
};

template<typename T>
struct std::hash<Id<T>> {
    size_t operator()(const Id<T>& id) const
    {
        return std::hash<int>()(id.id);
    }
};


struct Block;
typedef Id<Block> BlockId;

// a literal variable name in the source
typedef int AliasId;


struct Variable;
typedef Id<Variable> VariableId;
struct Variable {
    VariableId id;
};


struct VariableTypeId {
    std::type_index encoded_type;

    // register-compatible types
    static const VariableTypeId BOOL;
    static const VariableTypeId ISIZE;
    static const VariableTypeId FSIZE;
    static const VariableTypeId NIL;

    VariableTypeId(): VariableTypeId(VariableTypeId::NIL) { }

    VariableTypeId(std::type_index type_index): encoded_type(type_index) { }

    VariableTypeId(const VariableTypeId &other) = default;

    template<typename T>
    VariableTypeId(): VariableTypeId(std::type_index(typeid(T))) { }

    bool operator ==(const VariableTypeId &other) const {
        return encoded_type == other.encoded_type;
    }

    std::string type_name() const {
        return utils::demangle_type_name(this->encoded_type.name());
    }
};
const VariableTypeId VariableTypeId::BOOL = std::type_index(typeid(bool));
const VariableTypeId VariableTypeId::ISIZE = std::type_index(typeid(int_jt));
const VariableTypeId VariableTypeId::FSIZE = std::type_index(typeid(int_jt));
const VariableTypeId VariableTypeId::NIL = std::type_index(typeid(nullptr));

template<>
struct std::hash<VariableTypeId> {
    size_t operator()(const VariableTypeId& id) const
    {
        return id.encoded_type.hash_code();
    }
};

#endif //JITJAM_HANDLES_H
