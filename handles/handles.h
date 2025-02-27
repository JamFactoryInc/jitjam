
//

#ifndef JITJAM_HANDLES_H
#define JITJAM_HANDLES_H

#include <utility>
#include <typeindex>
#include <map>
#include "../utils.h"
#include "../sljit_interop/jit_types.h"

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

#endif //JITJAM_HANDLES_H
