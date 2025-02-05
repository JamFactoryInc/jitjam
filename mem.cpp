//
// Created by jam on 13/09/2024.
//

#include "mem.h"
#include "asm.h"

template<>
FieldReference _field<void>(int size, int stack_offset, const void *struct_addr, const void *field_addr, bool _is_float) {
    // ensure that we received a field of this struct instance
    auto field_addr_val = reinterpret_cast<size_t>(field_addr);
    auto struct_addr_val = reinterpret_cast<size_t>(struct_addr);
    Jassert::check(field_addr_val >= struct_addr_val && field_addr_val < (struct_addr_val + size));
    return { stack_offset, static_cast<int>(field_addr_val - struct_addr_val), _is_float };
}

template<>
FieldReference _field<float_jt>(int size, int stack_offset, const void *struct_addr, const void *field_addr, bool _is_float) {
    return _field<void>(size, stack_offset, struct_addr, field_addr, _is_float);
}

template<>
FieldReference _field<int_jt>(int size, int stack_offset, const void *struct_addr, const void *field_addr, bool _is_float) {
    return _field<void>(size, stack_offset, struct_addr, field_addr, _is_float);
}

template<>
FieldReference _field<void *>(int size, int stack_offset, const void *struct_addr, const void *field_addr, bool _is_float) {
    return _field<void>(size, stack_offset, struct_addr, field_addr, _is_float);
}

_Mem _Mem::R0 = _Mem { SLJIT2_R0, 0, IS_REGISTER };
_Mem _Mem::R1 = _Mem { SLJIT2_R1, 0, IS_REGISTER };
_Mem _Mem::FR0 = _Mem { SLJIT2_FR0, 0, IS_REGISTER | IS_FLOAT };
_Mem _Mem::FR1 = _Mem { SLJIT2_FR1, 0, IS_REGISTER | IS_FLOAT };
_Mem _Mem::RETURN = _Mem { SLJIT2_RETURN_REG, 0, IS_REGISTER };
_Mem _Mem::FRETURN = _Mem { SLJIT2_RETURN_FREG, 0, IS_REGISTER | IS_FLOAT };

Mem Mem::R0 = Mem(&_Mem::R0, nullptr);
Mem Mem::R1 = Mem(&_Mem::R1, nullptr);
Mem Mem::FR0 = Mem(&_Mem::FR0, nullptr);

Mem Mem::RETURN = Mem(&_Mem::RETURN, nullptr);
Mem Mem::FRETURN = Mem(&_Mem::FRETURN, nullptr);

_Mem::_Mem() {}

void Mem::increment_count() {
    if (_mem->is_register()) {
        if (_mem->is_float()) {
            ++owner->float_registers_in_use[_mem->arg_1];
        } else {
            ++owner->general_registers_in_use[_mem->arg_1];
        }
    } else if (_mem->is_temporary_reserved()) {
        auto index = _mem->arg_1;
        auto &reservation_counts = owner->reservation_counts;
        if (index >= reservation_counts.size()) {
            reservation_counts.resize(reservation_counts.size() * 2, 0);
        }
        auto &reservation_count = reservation_counts[index];
        if (reservation_count >= 0) {
            ++reservation_count;
        }
    }
}

void Mem::decrement_count() {
    if (_mem->is_register()) {
        if (_mem->is_float()) {
            --owner->float_registers_in_use[_mem->arg_1];
        }
        --owner->general_registers_in_use[_mem->arg_1];
    } else if (_mem->is_temporary_reserved()) {
        // we can naively access this since we can assume this was constructed before it was destructed
        auto &reservation_count = owner->reservation_counts[_mem->arg_1];
        if (--reservation_count == 0) {
            reservation_count = -1;
            owner->mempack.free(_mem->arg_1);
        }
    }
}
