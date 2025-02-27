//
// Created by jam on 11/09/2024.
//

#ifndef SLJIT2_MEM_H
#define SLJIT2_MEM_H

#include "../../sljit/src/sljitLir.h"

#include "../../sljit_interop/jit_types.h"
#include "constructed_type.h"
#include "../mempack/memory_prealloc.h"
#include "../../utils.h"
#include <memory>
#include <unordered_map>
#include <memory>
#include <list>

using namespace jt;

struct Mem;
struct Asm;

struct Jump {
    std::shared_ptr<sljit2_jump*> jmp;

public:
    Jump() {
        this->jmp = std::make_shared<sljit2_jump*>();
    }
};

struct Label {
    std::shared_ptr<sljit2_label*> label;

public:

    Label() {
        this->label = std::make_shared<sljit2_label*>();
    }
};

struct _Mem;
struct Register {
    sljit2_s32 arg_1;

    Register(sljit2_s32 _arg_1): arg_1(_arg_1) { }

    operator sljit2_s32() const {
        return arg_1;
    }
};
template<>
struct std::hash<Register> {
    size_t operator()(Register reg) {
        return reg.arg_1;
    }
};

template<typename F>
FieldReference _field(int size, int stack_offset, const void *struct_addr, const void *field_addr, bool _is_float) {
    static_assert(false, "Field type not supported");
    return FieldReference();
}

template<typename T>
struct LocalVar {
    // shared dummy instance just used to get the address of fields
    static const T instance;
    int stack_offset;

    LocalVar(int _stack_offset): stack_offset(_stack_offset) { }

    T const &operator *() const {
        return instance;
    }

    template<typename F>
    FieldReference field(const F &field) const {
        return _field<F>();
    }
};

template<>
struct LocalVar<float_jt> {
    int stack_offset;
    LocalVar(int _stack_offset): stack_offset(_stack_offset) { }
    operator FieldReference() { return {stack_offset, 0, true}; }
};

template<>
struct LocalVar<int_jt> {
    int stack_offset;
    LocalVar(int _stack_offset): stack_offset(_stack_offset) { }
    operator FieldReference() { return {stack_offset, 0}; }
};


template<>
struct LocalVar<float_jt*> {
    int stack_offset;
    LocalVar(int _stack_offset): stack_offset(_stack_offset) { }
    operator FieldReference() { return {stack_offset, 0, true}; }
};

template<typename T>
struct LocalVar<T*> {
    int stack_offset;
    LocalVar(int _stack_offset): stack_offset(_stack_offset) { }
    operator FieldReference() { return {stack_offset, 0}; }
};

template<const int LEN>
struct LocalVar<float_jt[LEN]> {
    int stack_offset;
    LocalVar(int _stack_offset): stack_offset(_stack_offset) { }
    operator FieldReference() { return {stack_offset, 0, true}; }
    FieldReference index(int idx) {
        return FieldReference(stack_offset, idx * sizeof(float_jt));
    }
};

template<typename T, const int LEN>
struct LocalVar<T[LEN]> {
    int stack_offset;
    LocalVar(int _stack_offset): stack_offset(_stack_offset) { }
    operator FieldReference() { return {stack_offset, 0}; }
    FieldReference index(int idx) {
        return FieldReference(stack_offset, idx * sizeof(T));
    }
};

struct _Mem {
    _Mem();

    friend struct Mem;

    static _Mem R0;
    static _Mem R1;
    static _Mem FR0;
    static _Mem FR1;
    static _Mem RETURN;
    static _Mem FRETURN;

    int16_t flags = 0;
    // how many pointers there are before the value.
    // int -> 0, int* -> 1, int** -> 2
    // this is only nonzero when IS_POINTER flag is present
    int16_t ptr_depth = 0;
    sljit2_s32 arg_1 = 0;
    int_jt arg_2 = 0;

    static const int IS_ARG =           0b0000001; // an argument
    static const int IS_FLOAT =         0b0000010; // a float value or a pointer to a root float value
    static const int IS_LOCAL =         0b0000100; // a stack-allocated value or a pointer to a root stack-allocated value
    static const int IS_REGISTER =      0b0001000; // a float/int register or a pointer to a root float/int register
    static const int IS_POINTER =       0b0010000; // a pointer to some underlying value (incompatible with IS_FLOAT)
    static const int IS_CONST =         0b0100000; // a virtual memory location for an integer constant
    static const int IS_TEMP_RESERVED = 0b1000000; // a virtual memory location for an integer constant

    _Mem(Register reg): arg_1(reg.arg_1), flags(IS_REGISTER) { }
    _Mem(FieldReference field): arg_1() { }
    _Mem(sljit2_s32 _arg_1, int_jt _arg_2): arg_1(_arg_1), arg_2(_arg_2) { }
    _Mem(sljit2_s32 _arg_1, int_jt _arg_2, int _flags):
        flags(_flags),
        arg_1(_arg_1),
        arg_2(_arg_2) { }
    _Mem(MemReservation reservation): _Mem(reservation.id, -1, IS_LOCAL | IS_TEMP_RESERVED) { }
    _Mem(MemReservation reservation, int _flags): _Mem(reservation.id, -1, _flags | IS_LOCAL | IS_TEMP_RESERVED) { }

    constexpr inline bool has_all_flags(int _flags) const {
        return (flags & _flags) == _flags;
    }

    constexpr inline bool has_any_flag(int _flags) const {
        return (flags & _flags) != 0;
    }

    // whether the value is an integer constant
    constexpr inline bool is_const() const {
        return has_all_flags(IS_CONST);
    }

    // whether the memory location represents a function argument.
    // this location is always a safe register.
    // this is type-agnostic
    constexpr inline bool is_arg() const {
        return has_all_flags(IS_ARG);
    }

    // whether the memory location represents a concrete float.
    // this returns false for float pointers.
    constexpr inline bool is_float() const {
        return has_all_flags(IS_FLOAT) && !is_pointer();
    }

    // whether the memory location represents a concrete float.
    // this returns false for float pointers.
    constexpr inline bool is_root_type_float() const {
        return has_all_flags(IS_FLOAT);
    }

    // whether the memory is a temporary stack-allocated variable
    // at this point it has been reserved but not assigned a stack offset.
    constexpr inline bool is_temporary_reserved() const {
        return is_register() || has_all_flags(IS_TEMP_RESERVED);
    }

    // whether the memory is any kind of register
    constexpr inline bool is_register() const {
        return has_all_flags(IS_REGISTER);
    }

    // whether the memory is an int register
    constexpr inline bool is_int_register() const {
        return has_all_flags(IS_REGISTER) && !is_float();
    }

    // whether the memory is a float register
    // this will return false for pointers to floats, as pointers are stored in int registers
    constexpr inline bool is_float_register() const {
        return has_all_flags(IS_REGISTER | IS_FLOAT) && !is_pointer();
    }

    // whether the value is a pointer to some underlying value
    constexpr inline bool is_pointer() const {
        return has_all_flags(IS_POINTER);
    }

    // whether the value is on the stack.
    // Stack values may not be directly returned in a return instruction, so this is important to know
    constexpr inline bool is_stack_allocated() const {
        return has_all_flags(IS_LOCAL);
    }

    // a local address that has not yet been initialized, and thus still contains the type size
    constexpr inline bool is_uninit_local_address() const {
        return is_stack_allocated() && arg_2 == -1 && !is_temporary_reserved();
    }

    // returns the type size in bytes of this *uninitialized* local variable.
    // Using this after calling initialize_local is UB.
    sljit2_s32 get_uninit_local_size() const {
        return arg_1;
    }

    // returns the register index (R0 -> 0)
    int get_register_index() {
        return arg_1 - 1;
    }

    // a value that is owned by the function (i.e. not heap-allocated)
    bool is_locally_owned() const {
        return has_any_flag(IS_LOCAL | IS_ARG | IS_REGISTER);
    }

    // overwrite the type metadata with the stack offset, flags, and stack pointer of the concrete memory location
    void initialize_local(int stack_offset) {
        arg_1 = SLJIT2_MEM1(SLJIT2_SP);
        arg_2 = stack_offset;
        flags |= IS_LOCAL;
        flags &= ~IS_TEMP_RESERVED;
    }

    bool operator==(const _Mem &other) const {
        return arg_1 == other.arg_1
               && arg_2 == other.arg_2
               && flags == other.flags
               && ptr_depth == other.ptr_depth;
    }
    bool operator!=(const _Mem &other) const {
        return !(*this == other);
    }
};
template<>
struct std::hash<_Mem> {
    size_t operator()(const _Mem &reg) const {
        hash<sljit2_s32> int32_hash;
        hash<int_jt > int64_hash;
        return int32_hash(reg.flags) + int32_hash(reg.arg_1) + int64_hash(reg.arg_2);
    }
};

struct MemArenaBlock {
    static const int CAPACITY = 32;
    _Mem *block;
    MemArenaBlock *prev = nullptr;
    int cursor = 0;

    MemArenaBlock() {
        block = new _Mem[CAPACITY];
    }

    MemArenaBlock(MemArenaBlock *prev): prev(prev) {
        block = new _Mem[CAPACITY];
    }

    _Mem &operator[](int index) {
        return block[index];
    }

    void clear() {
        delete[] block;
        if (prev) {
            prev->clear();
        }
    }
};

struct MemArena {
    MemArenaBlock head = MemArenaBlock();

    _Mem *add(_Mem mem) {
        if (head.cursor >= MemArenaBlock::CAPACITY) {
            auto new_block = MemArenaBlock(new MemArenaBlock(head));
            head = new_block;
        }
        auto &value = head[head.cursor++];
        value = mem;
        return &value;
    }

    void clear() {
        head.clear();
    }
};



struct Mem {
    _Mem *_mem;
    Asm *owner;

    static Mem R0;
    static Mem R1;
    static Mem FR0;

    static Mem RETURN;
    static Mem FRETURN;

    void increment_count();
    void decrement_count();

    Mem() = default;
    Mem(_Mem *addr, Asm *_owner): _mem(addr), owner(_owner) {
        // start out with a count of 1
        // this will be decremented to be 0 on deconstruction
        if (_owner) {
            increment_count();
        }
    }
    Mem(const Mem& other) {
        _mem = other._mem;
        owner = other.owner;
        if (owner) {
            increment_count();
        }
    }

    bool operator==(const Mem &other) const {
        return _mem == other._mem || *_mem == *other._mem;
    }

    _Mem &operator*() {
        return *_mem;
    }

    _Mem const &operator*() const {
        return *_mem;
    }

    _Mem *operator->() {
        return _mem;
    }

    _Mem const *operator->() const {
        return _mem;
    }

public:
    ~Mem() {
        if (owner) {
            decrement_count();
        }
    }
};

/**
 * A temporary rvalue type to allow passing a _Mem into a pass-by-copy lambda
 * This is required, as copying a Mem instance will mess with the register allocation logic
 */
struct MemCopy {
    _Mem *_mem;

    MemCopy() {

    }

    // NOLINT(*-explicit-constructor)
    MemCopy(Mem &mem) {
        _mem = mem._mem;
    }

    _Mem &operator*() {
        return *_mem;
    }

    _Mem const &operator*() const {
        return *_mem;
    }

    _Mem *operator->() {
        return _mem;
    }

    _Mem const *operator->() const {
        return _mem;
    }
};




#endif //SLJIT2_MEM_H
