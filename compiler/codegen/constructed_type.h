//
// Created by jam on 13/09/2024.
//

#ifndef GODOT_CONSTRUCTED_TYPE_H
#define GODOT_CONSTRUCTED_TYPE_H

#include <unordered_map>

struct FieldReference {
    int stack_offset;
    int struct_offset = 0;
    bool is_float = false;
    FieldReference() = default;
    FieldReference(int _stack_offset, int _struct_offset, bool _is_float = false):
        stack_offset(_stack_offset),
        struct_offset(_struct_offset),
        is_float(_is_float) { }
};

/// a representation of a struct at runtime
template<typename T>
class ConstructedType {
public:
    // we don't need to assign this since it's just a means of obtaining the address of a field
    const T data = 0;

    ConstructedType() = default;
};



#endif //GODOT_CONSTRUCTED_TYPE_H
