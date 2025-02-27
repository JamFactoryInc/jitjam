//
// Created by jam on 11/09/2024.
//

#ifndef SLJIT2_ASM_H
#define SLJIT2_ASM_H

#include "mem.h"
#include "../../utils.h"
#include "../../sljit_interop/arg_type_utils.h"
#include "../mempack/memory_prealloc.h"
#include <vector>
#include <functional>
#include <unordered_set>
#include <memory>
#include <cmath>

using namespace jt;

#define SPLIT(mem) mem->arg_1, mem->arg_2

struct SavedRegisters {
    std::vector<int> from_reg_index;
    std::vector<Mem> to;
};

struct Asm {
    friend struct Mem;

    sljit2_compiler *compiler;
    std::unordered_set<sljit2_s32> initialized_floats;
    std::vector<std::function<void()>> constructor = std::vector<std::function<void()>>();
    std::vector<std::function<void()>> instructions = std::vector<std::function<void()>>();
    std::vector<std::function<void()>> destructor = std::vector<std::function<void()>>();


    std::unordered_map<int_jt, _Mem> constants = std::unordered_map<int_jt, _Mem>();
    // this holds all the values of our returned _Mem pointers
    // we need to use a linked list so the pointers are never invalidated due to vector resizing
    MemArena backings = MemArena();

    std::vector<_Mem> general_registers;
    std::vector<_Mem> float_registers;

    MemPacker mempack = MemPacker();

    // keyed-into via ReservationId
    std::vector<ReservationId> reservation_counts = std::vector<ReservationId>(8, 0);
    std::vector<_Mem*> preallocated_stack_temporaries = std::vector<_Mem*>();

    int float_registers_in_use[SLJIT2_NUMBER_OF_FLOAT_REGISTERS + 1] = { 0 };
    int general_registers_in_use[SLJIT2_NUMBER_OF_REGISTERS + 1] = { 0 };

    int args = 0;
    int float_args = 0;
    int required_registers = 0;
    int required_float_registers = 0;

    int stack_size = 0;

    static std::vector<_Mem> _init_registers() {
        auto registers = std::vector<_Mem>();
        registers.resize(SLJIT2_NUMBER_OF_REGISTERS + 1);
        for (int i = 1; i <= SLJIT2_NUMBER_OF_REGISTERS; ++i) {
            registers[i] = _Mem { i, 0, _Mem::IS_REGISTER };
        }
        return registers;
    }

    static std::vector<_Mem> _init_float_registers() {
        auto registers = std::vector<_Mem>();
        registers.resize(SLJIT2_NUMBER_OF_FLOAT_REGISTERS + 1);
        for (int i = 1; i <= SLJIT2_NUMBER_OF_FLOAT_REGISTERS; ++i) {
            registers[i] = _Mem { i, 0, _Mem::IS_REGISTER | _Mem::IS_FLOAT };
        }
        return registers;
    }

    Asm() {
        compiler = sljit2_create_compiler(nullptr);
        general_registers = _init_registers();
        float_registers = _init_float_registers();

        constructor.reserve(16);
        instructions.reserve(16);
        destructor.reserve(16);

        preallocated_stack_temporaries.reserve(8);
    }

    inline void write_init(const std::function<void()> &instr) {
        constructor.push_back(instr);
    }

    void write_deinit(const std::function<void()> &instr) {
        destructor.push_back(instr);
    }

    inline void write_instr(const std::function<void()> &instr) {
        instructions.push_back(instr);
    }

    void init_arg(Mem const& reg) {
        if (!reg->is_float()) {
            args = std::max(args, SLJIT2_NUMBER_OF_REGISTERS - reg->arg_1 + 1);
            return;
        }
        bool is_initialized = initialized_floats.find(reg->arg_1) != initialized_floats.end();
        if (!is_initialized) {
            initialized_floats.insert(reg->arg_1);
            int arg_number = SLJIT2_NUMBER_OF_FLOAT_REGISTERS - reg->arg_1;
            // copy our float reg to the associated save reg, so we can freely use it as temp memory
            write_init([=]() {
                sljit2_emit_fop1(compiler, SLJIT2_MOV_F64, SLJIT2_FS(arg_number), 0, SLJIT2_FR(arg_number), 0);
            });
            float_args = std::max(float_args, arg_number);
            required_float_registers = std::max(float_args, arg_number + 1);
        }
    }

    void require_memory(const std::initializer_list<Mem> registers) {
        for (const Mem &reg: registers) {
            if (reg->is_arg()) {
                init_arg(reg);
            } else if (reg->is_int_register()) {
                required_registers = std::max(required_registers, reg->arg_1);
            } else if (reg->is_float_register()) {
                required_float_registers = std::max(required_float_registers, reg->arg_1);
            } else if (reg->is_uninit_local_address()) {
                int size = reg->get_uninit_local_size();
                reg._mem->initialize_local(stack_size);
                stack_size += size;
            }
        }
    }

    SavedRegisters save_float_scratches() {
        std::vector<int> from_reg_index = std::vector<int>();
        std::vector<Mem> to_mem = std::vector<Mem>();
        for (int i = 0; i < SLJIT2_NUMBER_OF_SCRATCH_FLOAT_REGISTERS; ++i)
        {
            int scratch_register_index = SLJIT2_FR(i);
            if (float_registers_in_use[scratch_register_index] > 0)
            {
                auto register_mem = &float_registers[scratch_register_index];
                Mem stack_location = temp_local<float_jt>();
                from_reg_index.push_back(scratch_register_index);
                to_mem.emplace_back(stack_location);

                move(Mem(register_mem, this), stack_location);
            }
        }

        return SavedRegisters {
            from_reg_index,
            to_mem
        };
    }

    SavedRegisters save_general_scratches() {
        std::vector<int> from_reg_index = std::vector<int>();
        std::vector<Mem> to_mem = std::vector<Mem>();

        for (int i = 0; i < SLJIT2_NUMBER_OF_SCRATCH_REGISTERS; ++i)
        {
            int scratch_register_index = SLJIT2_R(i);
            if (general_registers_in_use[scratch_register_index] > 0)
            {
                auto register_mem = &general_registers[scratch_register_index];
                Mem stack_location = temp_local<int_jt>();
                from_reg_index.push_back(scratch_register_index);
                to_mem.emplace_back(stack_location);

                move(Mem(register_mem, this), stack_location);
            }
        }

        return SavedRegisters {
            from_reg_index,
            to_mem
        };
    }

    void restore_float_scratches(SavedRegisters saved_registers) {
        for (int i = 0; i < saved_registers.from_reg_index.size(); ++i) {
            auto float_reg_index = saved_registers.from_reg_index[i];
            auto register_mem = &float_registers[float_reg_index];

            auto stack_mem = saved_registers.to[i];
            move(stack_mem, Mem(register_mem, this));
        }
    }

    void restore_general_scratches(SavedRegisters saved_registers) {
        for (int i = 0; i < saved_registers.from_reg_index.size(); ++i) {
            auto reg_index = saved_registers.from_reg_index[i];
            auto register_mem = &general_registers[reg_index];

            auto stack_mem = saved_registers.to[i];
            move(stack_mem, Mem(register_mem, this));
        }
    }

    int get_general_scratch() {
        // R0 & R1 are always reserved to guarantee that basic ops have a register free for temp values
        // division always operates on both R0 & R1
        for (int i = SLJIT2_R1 + 1; i <= SLJIT2_NUMBER_OF_SCRATCH_REGISTERS; ++i) {
            int &usages = general_registers_in_use[i];
            if (usages <= 0) {
                return i;
            }
        }
        return -1;
    }

    int get_general_saved() {
        for (int i = 0; i < SLJIT2_NUMBER_OF_SAVED_REGISTERS; ++i) {
            int &usages = general_registers_in_use[SLJIT2_S(i)];
            if (usages <= 0) {
                return i;
            }
        }
        return -1;
    }

    int get_float_scratch() {
        // FR0 is always reserved to guarantee that basic ops have a register free for temp values
        for (int i = SLJIT2_FR0 + 1; i < SLJIT2_NUMBER_OF_SCRATCH_FLOAT_REGISTERS; ++i) {
            int &usages = float_registers_in_use[i];
            if (usages <= 0) {
                return i;
            }
        }
        return -1;
    }

    int get_float_saved() {
        for (int i = 0; i < SLJIT2_NUMBER_OF_SAVED_FLOAT_REGISTERS; ++i) {
            int &usages = float_registers_in_use[SLJIT2_FS(i)];
            if (usages <= 0) {
                return i;
            }
        }
        return -1;
    }

    // gets an address to the given _Mem value that lives as long as the enclosing Asm instance in the same thread
    _Mem *_addr(_Mem value) {
        return backings.add(value);
    }

public:

    template<typename R>
    std::function<R(void *)> compile_array_args() {
        sljit2_s32 arg_types = ArgTypeOf<R>::value;
        arg_types |= ArgTypeOf<sljit2_up>::value << 4;
        return reinterpret_cast<R (*)()>(_compile(arg_types));
    }

    template<typename R>
    R (*compile())() {
        sljit2_s32 arg_types = ArgTypeOf<R>::value;
        required_registers = std::max(required_registers, ArgTypeOf<R>::uses_int_reg());
        required_float_registers = std::max(required_float_registers, ArgTypeOf<R>::uses_float_reg());

        return reinterpret_cast<R (*)()>(_compile(arg_types));
    }

    template<typename R, typename A1>
    R (*compile())(A1) {
        sljit2_s32 arg_types = ArgTypeOf<R>::value;
        arg_types |= ArgTypeOf<A1>::value << 4;

        required_registers = std::max(required_registers, ArgTypeOf<R>::uses_int_reg());
        required_float_registers = std::max(required_float_registers, ArgTypeOf<R>::uses_float_reg());

        args = std::max(args,
            ArgTypeOf<A1>::uses_int_reg()
        );
        float_args = std::max(float_args,
            ArgTypeOf<A1>::uses_float_reg()
        );
        return reinterpret_cast<R (*)(A1)>(_compile(arg_types));
    }

    template<typename R, typename A1, typename A2>
    R (*compile())(A1, A2) {
        sljit2_s32 arg_types = ArgTypeOf<R>::value;
        arg_types |= ArgTypeOf<A1>::value << 4;
        arg_types |= ArgTypeOf<A2>::value << 8;

        required_registers = std::max(required_registers, ArgTypeOf<R>::uses_int_reg());
        required_float_registers = std::max(required_float_registers, ArgTypeOf<R>::uses_float_reg());

        args = std::max(args,
            ArgTypeOf<A1>::uses_int_reg()
                + ArgTypeOf<A2>::uses_int_reg()
        );
        float_args = std::max(float_args,
            ArgTypeOf<A1>::uses_float_reg()
                + ArgTypeOf<A2>::uses_float_reg()
        );

        return reinterpret_cast<R (*)(A1, A2)>(_compile(arg_types));
    }

    template<typename R, typename A1, typename A2, typename A3>
    R (*compile())(A1, A2, A3) {
        sljit2_s32 arg_types = ArgTypeOf<R>::value;
        arg_types |= ArgTypeOf<A1>::value << 4;
        arg_types |= ArgTypeOf<A2>::value << 8;
        arg_types |= ArgTypeOf<A3>::value << 12;

        required_registers = std::max(required_registers, ArgTypeOf<R>::uses_int_reg());
        required_float_registers = std::max(required_float_registers, ArgTypeOf<R>::uses_float_reg());

        args = std::max(args,
            ArgTypeOf<A1>::uses_int_reg()
                + ArgTypeOf<A2>::uses_int_reg()
                + ArgTypeOf<A3>::uses_int_reg()
        );
        float_args = std::max(float_args,
            ArgTypeOf<A1>::uses_float_reg()
                + ArgTypeOf<A2>::uses_float_reg()
                + ArgTypeOf<A3>::uses_float_reg()
        );

        return reinterpret_cast<R (*)(A1, A2, A3)>(_compile(arg_types));
    }

    template<typename R, typename A1, typename A2, typename A3, typename A4>
    R (*compile())(A1, A2, A3, A4) {
        sljit2_s32 arg_types = ArgTypeOf<R>::value;
        arg_types |= ArgTypeOf<A1>::value << 4;
        arg_types |= ArgTypeOf<A2>::value << 8;
        arg_types |= ArgTypeOf<A3>::value << 12;
        arg_types |= ArgTypeOf<A4>::value << 16;

        required_registers = std::max(required_registers, ArgTypeOf<R>::uses_int_reg());
        required_float_registers = std::max(required_float_registers, ArgTypeOf<R>::uses_float_reg());

        args = std::max(args,
            ArgTypeOf<A1>::uses_int_reg()
                + ArgTypeOf<A2>::uses_int_reg()
                + ArgTypeOf<A3>::uses_int_reg()
                + ArgTypeOf<A4>::uses_int_reg()
        );
        float_args = std::max(float_args,
            ArgTypeOf<A1>::uses_float_reg()
                + ArgTypeOf<A2>::uses_float_reg()
                + ArgTypeOf<A3>::uses_float_reg()
                + ArgTypeOf<A4>::uses_float_reg()
        );

        return reinterpret_cast<R (*)(A1, A2, A3, A4)>(_compile(arg_types));
    }

    template<typename R, typename A1, typename A2, typename A3, typename A4, typename A5>
    R (*compile())(A1, A2, A3, A4, A5) {
        sljit2_s32 arg_types = ArgTypeOf<R>::value;
        arg_types |= ArgTypeOf<A1>::value << 4;
        arg_types |= ArgTypeOf<A2>::value << 8;
        arg_types |= ArgTypeOf<A3>::value << 12;
        arg_types |= ArgTypeOf<A4>::value << 16;
        arg_types |= ArgTypeOf<A5>::value << 20;

        required_registers = std::max(required_registers, ArgTypeOf<R>::uses_int_reg());
        required_float_registers = std::max(required_float_registers, ArgTypeOf<R>::uses_float_reg());

        args = std::max(args,
            ArgTypeOf<A1>::uses_int_reg()
                + ArgTypeOf<A2>::uses_int_reg()
                + ArgTypeOf<A3>::uses_int_reg()
                + ArgTypeOf<A4>::uses_int_reg()
                + ArgTypeOf<A5>::uses_int_reg()
        );
        float_args = std::max(float_args,
            ArgTypeOf<A1>::uses_float_reg()
                + ArgTypeOf<A2>::uses_float_reg()
                + ArgTypeOf<A3>::uses_float_reg()
                + ArgTypeOf<A4>::uses_float_reg()
                + ArgTypeOf<A5>::uses_float_reg()
        );

        return reinterpret_cast<R (*)(A1, A2, A3, A4, A5)>(_compile(arg_types));
    }

    void *_compile(sljit2_s32 arg_types) {
        // optimize & assign the memory locations for the required stack temporaries
        auto mem_requirements = mempack.pack();
        for (auto temporary : preallocated_stack_temporaries) {
            auto stack_offset = stack_size + mem_requirements.offsets[temporary->arg_1];
            temporary->initialize_local(stack_offset);
        }

        stack_size += mem_requirements.total_bytes;

        required_float_registers = std::max(required_float_registers, float_args);

        Jassert::check(required_registers <= SLJIT2_NUMBER_OF_REGISTERS, "Required more registers than available!\nRequired: {}\nAvailable: {}", { required_registers, SLJIT2_NUMBER_OF_REGISTERS });
        Jassert::check(required_float_registers <= SLJIT2_NUMBER_OF_FLOAT_REGISTERS, "Required more float registers than available!\nRequired: {}\nAvailable: {}", { required_float_registers, SLJIT2_NUMBER_OF_FLOAT_REGISTERS });

        sljit2_emit_enter(
            compiler, 0, arg_types,
            required_registers | SLJIT2_ENTER_FLOAT(required_float_registers),
            args | SLJIT2_ENTER_FLOAT(float_args),
            stack_size
        );
        for (const auto& init: constructor) {
            init();
        }
        for (const auto& instr: instructions) {
            instr();
        }
        for (const auto& deinit: destructor) {
            deinit();
        }

        return sljit2_generate_code(compiler, 0, nullptr);
    }

    std::vector<Mem> get_call_args(std::initializer_list<Mem> list)
    {
        auto locations = std::vector<Mem>();
        for( auto &elem : list )
        {
            locations.emplace_back(elem);
        }
        return locations;
    }

    template<typename T>
    Mem local_variable() {
        return Mem(_addr(_Mem { sizeof(T), -1, _Mem::IS_LOCAL }), this);
    }

    template<typename T>
    Mem temp_local() {
        auto mem = _addr(_Mem(mempack.reserve(sizeof(T))));
        preallocated_stack_temporaries.push_back(mem);
        return Mem(mem, this);
    }

    template<>
    Mem temp_local<float_jt>() {
        auto mem = _addr(_Mem(
            mempack.reserve(sizeof(float_jt)),
            _Mem::IS_FLOAT
        ));
        preallocated_stack_temporaries.push_back(mem);
        return Mem(mem, this);
    }

    // Returns a memory location pointing to an available general register, if any.
    // If there are no available registers, this returns a stack location
    Mem reg() {
        int scratch_index = get_general_scratch();
        if (scratch_index == -1 || scratch_index > SLJIT2_NUMBER_OF_SCRATCH_REGISTERS) {
            // overflow onto the stack if there's no free registers at this time
            Mem mem = temp_local<int_jt>();
            require_memory({ mem });
            return mem;
        }
        Mem reg = Mem(&general_registers[scratch_index], this);
        require_memory({ reg });
        return reg;
    }

    // Returns a memory location pointing to an available float register, if any.
    // If there are no available registers, this returns a stack location
    Mem float_reg() {
        int scratch_index = get_float_scratch();
        if (scratch_index == -1 || scratch_index >= SLJIT2_NUMBER_OF_SCRATCH_FLOAT_REGISTERS) {
            // overflow onto the stack if there's no free registers at this time
            Mem mem = temp_local<float_jt>();
            require_memory({ mem });
            return mem;
        }
        Mem reg = Mem(&float_registers[scratch_index], this);
        require_memory({ reg });
        return reg;
    }

    // A memory location representing the nth non-float argument passed to this function, where 0 is the first argument.
    Mem arg(int arg_index) {
        Jassert::check(arg_index >= 0, "Arg index must be greater than 0. Received index: {}", { arg_index });

        auto *future_max_args = &args;
        write_instr([=]() {
            Jassert::check(arg_index >= 0, "Arg index outside of range defined by compiled function signature. Max index: {} Received index: {} ", { *future_max_args - 1, arg_index });
        });

        return Mem(_addr(_Mem { SLJIT2_S(arg_index), 0, _Mem::IS_REGISTER | _Mem::IS_ARG }), this);
    }

    // a float argument with the given 0-based fn arg index
    Mem float_arg(int arg_index) {
        Jassert::check(arg_index >= 0, "Float arg index must be greater than 0. Received index: {}", { arg_index });

        auto *future_max_args = &float_args;
        write_instr([=]() {
            Jassert::check(arg_index >= 0, "Float arg index outside of range defined by compiled function signature. Max index: {} Received index: {} ", { *future_max_args - 1, arg_index });
        });

        return Mem(_addr(_Mem { SLJIT2_FS(arg_index), 0, _Mem::IS_REGISTER | _Mem::IS_ARG | _Mem::IS_FLOAT }), this);
    }

    // the virtual memory location of a constant integer
    Mem integral_const(int_jt const_val) {
        auto entry = constants.find(const_val);
        if (entry != constants.end()) {
            return Mem(&entry->second, this);
        }
        auto &mem = constants[const_val] = _Mem { SLJIT2_IMM, const_val, _Mem::IS_CONST };
        return Mem(&mem, this);
    }

    // an int pointer to a memory location that will not change over the course of the generated function's existence
    // essentially a static pointer, only needing to outlive the generated code
    Mem absolute_address(int_jt* address) {
        Jassert::check(address, "Absolute address is null");
        return Mem(_addr(_Mem { SLJIT2_MEM0(), reinterpret_cast<int_jt>(address), _Mem::IS_POINTER }), this);
    }

    // a float pointer to a memory location that will not change over the course of the generated function's existence
    // essentially a static pointer, only needing to outlive the generated code
    Mem absolute_address(float_jt* address) {
        Jassert::check(address, "Absolute address is null");
        return Mem(_addr(_Mem { SLJIT2_MEM0(), reinterpret_cast<int_jt>(address), _Mem::IS_POINTER | _Mem::IS_FLOAT}), this);
    }

    Mem address_offset(Register address_register, int_jt byte_offset) {
        Jassert::check(byte_offset >= 0, "Absolute address is null");
        return Mem(_addr(_Mem { SLJIT2_MEM1(address_register), byte_offset }), this);
    }

    Mem array_index(Register address_register, Register index_register, int_jt size) {
        return Mem(_addr(_Mem { SLJIT2_MEM2(address_register, index_register), size }), this);
    }

    template<typename ARRAY_TYPE>
    Mem array_index(Register address_register, Register index_register) {
        return Mem(_addr(_Mem { SLJIT2_MEM2(address_register, index_register), sizeof(ARRAY_TYPE) }), this);
    }

    bool coerce_floats(Mem &lhs, Mem &rhs) {
        if (lhs->is_root_type_float() == rhs->is_root_type_float()) {
            return lhs->is_float();
        }
        Mem casting_temp_reg = Mem::FR0;
        if (lhs == Mem::FR0 || rhs == Mem::FR0) {
            casting_temp_reg = float_reg();
        }
        if (!lhs->is_root_type_float()) {
            int_to_float(lhs, casting_temp_reg);
            lhs = casting_temp_reg;
        } else if (!rhs->is_root_type_float()) {
            int_to_float(rhs, casting_temp_reg);
            rhs = casting_temp_reg;
        }
        return true;
    }

    Mem move(Mem src, Mem dst) {
        bool float_op = coerce_floats(src, dst);
        require_memory({ src, dst });
        if (src == dst) {
            return dst;
        }

        MemCopy _dst = dst;
        MemCopy _src = src;
        if (float_op) {
            write_instr([=]() {
                sljit2_emit_fop1(compiler, SLJIT2_MOV_F64, SPLIT(_dst), SPLIT(_src));
            });
        } else {
            write_instr([=]() {
                sljit2_emit_op1(compiler, SLJIT2_MOV, SPLIT(_dst), SPLIT(_src));
            });
        }
        return dst;
    }

    void return_value(Mem src) {
        require_memory({ src });

        if (src->is_float()) {
            src = move(src, Mem::FRETURN);
            MemCopy _src = src;
            write_instr([=]() {
                sljit2_emit_return(compiler, SLJIT2_MOV_F64, SPLIT(_src));
            });
        } else {
            src = move(src, Mem::RETURN);
            MemCopy _src = src;
            write_instr([=]() {
                sljit2_emit_return(compiler, SLJIT2_MOV, SPLIT(_src));
            });
        }
    }

    void return_void() {
        write_instr([=]() {
            sljit2_emit_return_void(compiler);
        });
    }

    Mem set_const(Mem dst, int_jt value) {
        require_memory({ dst });
        if (dst->is_int_register()) {
            if (value == 0) {
                return set_zero(dst);
            }
//            MemCopy _dst = dst;
//            binary_op(SLJIT2_MOV)
//            write_instr([=]() {
//                sljit2_emit_op2()
//                sljit2_emit_const(compiler, SPLIT(_dst), value);
//            });
//            return dst;
        }
        move(integral_const(value), dst);
        return dst;
    }

    Mem set_constf(Mem dst, float_jt value) {
        if (dst->is_float_register()) {
            require_memory({ dst });
            MemCopy _dst = dst;
            write_instr([=]() {
                sljit2_emit_fset64(compiler, _dst->arg_1, value);
            });
        } else {
            require_memory({ dst, Mem::FR0 });
            write_instr([=]() {
                sljit2_emit_fset64(compiler, Mem::FR0->arg_1, value);
            });
            move(Mem::FR0, dst);
        }

        return dst;
    }

    Mem float_to_int(Mem src, Mem dst) {
        require_memory({ src, dst });
        MemCopy _src = src;
        MemCopy _dst = dst;
        write_instr([=]() {
            sljit2_emit_fop1(compiler, SLJIT2_CONV_SW_FROM_F64, SPLIT(_dst), SPLIT(_src));
        });
        return dst;
    }

    Mem int_to_float(Mem src, Mem dst) {
        require_memory({ src, dst });
        MemCopy _src = src;
        MemCopy _dst = dst;
        write_instr([=]() {
            sljit2_emit_fop1(compiler, SLJIT2_CONV_F64_FROM_SW, SPLIT(_dst), SPLIT(_src));
        });
        return dst;
    }

    Mem binary_op(int op, Mem lhs, Mem rhs, Mem dst) {
        if (coerce_floats(lhs, rhs)) {
            switch (op) {
                case SLJIT2_ADD: op = SLJIT2_ADD_F64; break;
                case SLJIT2_SUB: op = SLJIT2_SUB_F64; break;
                case SLJIT2_MUL: op = SLJIT2_MUL_F64; break;
                case SLJIT2_DIV_SW: op = SLJIT2_DIV_F64; break;
            }
            require_memory({ lhs, rhs, dst });
            MemCopy _lhs = lhs;
            MemCopy _rhs = rhs;
            MemCopy _dst = dst;
            write_instr([=]() {
                sljit2_emit_fop2(compiler, op, SPLIT(_dst), SPLIT(_lhs), SPLIT(_rhs));
            });
        } else {
            require_memory({ lhs, rhs, dst });
            MemCopy _lhs = lhs;
            MemCopy _rhs = rhs;
            MemCopy _dst = dst;
            write_instr([=]() {
                sljit2_emit_op2(compiler, op, SPLIT(_dst), SPLIT(_lhs), SPLIT(_rhs));
            });
        }
        return dst;
    }

    Mem add(Mem lhs, Mem rhs, Mem dst) {
        return binary_op(SLJIT2_ADD, lhs, rhs, dst);
    }

    Mem sub(Mem lhs, Mem rhs, Mem dst) {
        return binary_op(SLJIT2_SUB, lhs, rhs, dst);
    }

    Mem mul(Mem lhs, Mem rhs, Mem dst) {
        return binary_op(SLJIT2_MUL, lhs, rhs, dst);
    }

//    void div_mod(Mem lhs, Mem rhs, Mem quotient_dst, Mem remainder_dst) {
//        if (rhs->is_const()) {
//            int_jt const_val = rhs->arg_2;
//            bool is_power_of_2 = const_val != 0 && !(const_val & (const_val - 1));
//            if (is_power_of_2) {
//                auto pow = integral_const((int_jt)std::log2(const_val));
//                Mem quot = shift_right(lhs, pow, quotient_dst->is_register() ? quotient_dst : Mem::R0);
//                auto truncated = shift_left(quot, pow, remainder_dst->is_register()
//                    ? remainder_dst
//                    : reg()
//                );
//                auto rem = sub(quot, truncated, remainder_dst);
//                if (*quot != *quotient_dst) {
//                    move(quot, quotient_dst);
//                }
//                return;
//            }
//        }
//        require_memory({ Mem::R0, Mem::R1, quotient_dst, remainder_dst });
//        move(lhs, Mem::R0);
//        move(rhs, Mem::R1);
//        write_instr([=]() {
//            sljit2_emit_op0(compiler, SLJIT2_DIVMOD_SW);
//        });
//        move(Mem::R0, quotient_dst);
//        move(Mem::R1, remainder_dst);
//    }

    Mem div(Mem lhs, Mem rhs, Mem dst) {
        if (coerce_floats(lhs, rhs)) {
            require_memory({ lhs, rhs, dst });
            MemCopy _lhs = lhs;
            MemCopy _rhs = rhs;
            MemCopy _dst = dst;
            write_instr([=]() {
                sljit2_emit_fop2(compiler, SLJIT2_DIV_F64, SPLIT(_dst), SPLIT(_lhs), SPLIT(_rhs));
            });
        } else {
            if (rhs->is_const()) {
                int_jt const_val = rhs->arg_2;
                bool is_power_of_2 = const_val != 0 && !(const_val & (const_val - 1));
                if (is_power_of_2) {
                    return shift_right(lhs, integral_const((int_jt) log2(const_val)), dst);
                }
            }
            require_memory({ Mem::R0, Mem::R1, dst });
            move(lhs, Mem::R0);
            move(rhs, Mem::R1);
            write_instr([=]() {
                sljit2_emit_op0(compiler, SLJIT2_DIV_SW);
            });
            move(Mem::R0, dst);
        }
        return dst;
    }

    Mem mod(Mem lhs, Mem rhs, Mem dst) {
        require_memory({ Mem::R0, Mem::R1, dst });
        move(lhs, Mem::R0);
        move(rhs, Mem::R1);
        write_instr([=]() {
            sljit2_emit_op0(compiler, SLJIT2_DIVMOD_SW);
        });
        move(Mem::R1, dst);
        return dst;
    }

    Mem neg(Mem src, Mem dst) {
        if (coerce_floats(src, dst)) {
            require_memory({ src, dst });
            MemCopy _src = src;
            MemCopy _dst = dst;
            write_instr([=]() {
                sljit2_emit_fop1(compiler, SLJIT2_NEG_F64, SPLIT(_dst), SPLIT(_src));
            });
        } else {
            require_memory({ src, dst });
            MemCopy _src = src;
            MemCopy _dst = dst;
            write_instr([=]() {
                sljit2_emit_op2(compiler, SLJIT2_MUL, SPLIT(_dst), SPLIT(_src), SLJIT2_IMM, -1);
            });
        }
        return dst;
    }

    Mem shift_left(Mem lhs, Mem rhs, Mem dst) {
        return binary_op(SLJIT2_SHL, lhs, rhs, dst);
    }

    Mem shift_right(Mem lhs, Mem rhs, Mem dst) {
        return binary_op(SLJIT2_ASHR, lhs, rhs, dst);
    }

    Mem bit_and(Mem lhs, Mem rhs, Mem dst) {
        return binary_op(SLJIT2_AND, lhs, rhs, dst);
    }

    Mem bit_or(Mem lhs, Mem rhs, Mem dst) {
        return binary_op(SLJIT2_OR, lhs, rhs, dst);
    }

    Mem bit_xor(Mem lhs, Mem rhs, Mem dst) {
        return binary_op(SLJIT2_XOR, lhs, rhs, dst);
    }

    Mem set_zero(Mem reg) {
        return bit_xor(reg, reg, reg);
    }

    Mem set_from_flag(int flag_type, Mem dst) {
        require_memory({ dst });
        MemCopy _dst = dst;
        write_instr([=]() {
            sljit2_emit_op_flags(compiler, SLJIT2_MOV, SPLIT(_dst), flag_type);
        });
        return dst;
    }

    Mem less_than(Mem lhs, Mem rhs, Mem dst) {
        binary_op(SLJIT2_SUB | SLJIT2_SET_LESS, lhs, rhs, dst);
        set_from_flag(SLJIT2_LESS, dst);
        return dst;
    }

    Mem less_than_or_equal(Mem lhs, Mem rhs, Mem dst) {
        binary_op(SLJIT2_SUB | SLJIT2_SET_LESS_EQUAL, lhs, rhs, dst);
        set_from_flag(SLJIT2_LESS_EQUAL, dst);
        return dst;
    }

    Mem greater_than(Mem lhs, Mem rhs, Mem dst) {
        binary_op(SLJIT2_SUB | SLJIT2_SET_GREATER, lhs, rhs, dst);
        set_from_flag(SLJIT2_GREATER, dst);
        return dst;
    }

    Mem greater_than_or_equal(Mem lhs, Mem rhs, Mem dst) {
        binary_op(SLJIT2_SUB | SLJIT2_SET_GREATER_EQUAL, lhs, rhs, dst);
        set_from_flag(SLJIT2_GREATER_EQUAL, dst);
        return dst;
    }

    Mem equal(Mem lhs, Mem rhs, Mem dst) {
        binary_op(SLJIT2_SUB | SLJIT2_SET_Z, lhs, rhs, dst);
        set_from_flag(SLJIT2_EQUAL, dst);
        return dst;
    }

    Mem not_equal(Mem lhs, Mem rhs, Mem dst) {
        binary_op(SLJIT2_SUB | SLJIT2_SET_Z, lhs, rhs, dst);
        set_from_flag(SLJIT2_NOT_EQUAL, dst);
        return dst;
    }

    Mem logical_and(Mem lhs, Mem rhs, Mem dst) {
        require_memory({ Mem::R0, Mem::R1 });
        set_zero(Mem::R0);
        not_equal(rhs, Mem::R0, Mem::R1);
        not_equal(lhs, Mem::R0, Mem::R0);
        bit_and(Mem::R0, Mem::R1, dst);
        return dst;
    }

    Mem logical_or(Mem lhs, Mem rhs, Mem dst) {
        require_memory({ Mem::R0, Mem::R1 });
        // dst = (lhs | rhs) != 0
        set_zero(Mem::R0);
        bit_or(lhs, rhs, Mem::R1);
        not_equal(Mem::R0, Mem::R1, dst);
        return dst;
    }

    void call_static_fast(void (*fn_ptr)()) {
        auto saved_floats = save_float_scratches();
        auto saved_generals = save_general_scratches();

        auto fn_ptr_int = (sljit2_uw)(fn_ptr);
        write_instr([=]() {
            sljit2_jump *jmp = sljit2_emit_jump(compiler, SLJIT2_FAST_CALL);
            sljit2_set_target(jmp, fn_ptr_int);
        });

        restore_float_scratches(saved_floats);
        restore_general_scratches(saved_generals);
    }

    template<typename T>
    void call_static(T fn_ptr, FunctionSignature signature) {
        auto saved_floats = save_float_scratches();
        auto saved_generals = save_general_scratches();

        auto fn_ptr_int = (sljit2_uw)reinterpret_cast<void*(*)()>(fn_ptr);
        write_instr([=]() {
            sljit2_jump *jmp = sljit2_emit_call(compiler, SLJIT2_CALL, SLJIT2_ARGS0V());
            sljit2_set_target(jmp, fn_ptr_int);
        });

        restore_float_scratches(saved_floats);
        restore_general_scratches(saved_generals);
    }

    void call_dynamic(Mem fn_ptr_location, FunctionSignature signature, std::vector<Mem> call_args) {
        auto saved_floats = save_float_scratches();
        auto saved_generals = save_general_scratches();

        int float_count = 0;
        int int_count = 0;
        for (auto & mem : call_args) {
            if (mem->is_float()) {
                int register_index = SLJIT2_FR(float_count++);
                auto safe_reg = &float_registers[register_index];
                move(mem, Mem(safe_reg, this));
            } else {
                int register_index = SLJIT2_R(int_count++);
                auto safe_reg = &general_registers[register_index];
                move(mem, Mem(safe_reg, this));
            }
        }

        MemCopy _loc = fn_ptr_location;

        write_instr([=]() {
            sljit2_emit_icall(compiler, SLJIT2_CALL, signature.as_sljit_args(), SPLIT(_loc));
        });

        restore_float_scratches(saved_floats);
        restore_general_scratches(saved_generals);
    }

    Jump jump() {
        Jump jump = Jump();
        write_instr([=]() {
            *jump.jmp = sljit2_emit_jump(compiler, SLJIT2_JUMP);
        });
        return jump;
    }

    Jump jump_if_true(Mem &cond) {
        Mem zero = Mem::R0;
        if (cond == Mem::R0) {
            zero = Mem::R1;
        }
        require_memory({ cond, zero });
        set_zero(zero);
        return jump_if_not_equal(cond, zero);
    }

    Jump jump_if_false(Mem &cond = Mem::R0) {
        Mem zero = Mem::R0;
        if (cond == Mem::R0) {
            zero = Mem::R1;
        }
        require_memory({ cond, zero });
        set_zero(zero);
        return jump_if_equal(cond, zero);
    }

    Jump jump_if_equal(Mem lhs, Mem rhs) {
        require_memory({ lhs, rhs });
        Jump jump = Jump();
        MemCopy _lhs = lhs;
        MemCopy _rhs = rhs;
        write_instr([=]() {
//            binary_op(SLJIT2_SUB | SLJIT2_SET_Z, lhs, rhs, Mem::R0);
//            *jump.jmp = sljit2_emit_jump(compiler, SLJIT2_EQUAL);
//            *jump.jmp = sljit2_emit_jump(compiler, SLJIT2_JUMP);
            *jump.jmp = sljit2_emit_cmp(compiler, SLJIT2_EQUAL, SPLIT(_lhs), SPLIT(_rhs));
        });
        return jump;
    }

    Jump jump_if_not_equal(Mem lhs, Mem rhs) {
        require_memory({ lhs, rhs });
        Jump jump = Jump();
        MemCopy _lhs = lhs;
        MemCopy _rhs = rhs;
        write_instr([=]() {
            *jump.jmp = sljit2_emit_cmp(compiler, SLJIT2_NOT_EQUAL, SPLIT(_lhs), SPLIT(_rhs));
        });
        return jump;
    }

    Jump jump_if_less_than(Mem lhs, Mem rhs) {
        require_memory({ lhs, rhs });
        Jump jump = Jump();
        MemCopy _lhs = lhs;
        MemCopy _rhs = rhs;
        write_instr([=]() {
            *jump.jmp = sljit2_emit_cmp(compiler, SLJIT2_LESS, SPLIT(_lhs), SPLIT(_rhs));
        });
        return jump;
    }

    Jump jump_if_less_or_equal(Mem lhs, Mem rhs) {
        require_memory({ lhs, rhs });
        Jump jump = Jump();
        MemCopy _lhs = lhs;
        MemCopy _rhs = rhs;
        write_instr([=]() {
            *jump.jmp = sljit2_emit_cmp(compiler, SLJIT2_LESS_EQUAL, SPLIT(_lhs), SPLIT(_rhs));
        });
        return jump;
    }

    Jump jump_if_greater_than(Mem lhs, Mem rhs) {
        require_memory({ lhs, rhs });
        Jump jump = Jump();
        MemCopy _lhs = lhs;
        MemCopy _rhs = rhs;
        write_instr([=]() {
            *jump.jmp = sljit2_emit_cmp(compiler, SLJIT2_GREATER, SPLIT(_lhs), SPLIT(_rhs));
        });
        return jump;
    }

    Jump jump_if_greater_or_equal(Mem lhs, Mem rhs) {
        require_memory({ lhs, rhs });
        Jump jump = Jump();
        MemCopy _lhs = lhs;
        MemCopy _rhs = rhs;
        write_instr([=]() {
            *jump.jmp = sljit2_emit_cmp(compiler, SLJIT2_GREATER_EQUAL, SPLIT(_lhs), SPLIT(_rhs));
        });
        return jump;
    }

    Label jump_label() {
        Label label = Label();
        write_instr([=]() {
            *label.label = sljit2_emit_label(compiler);
        });
        return label;
    }

    void bind_jump(const Jump &from, const Label &to) {
        write_instr([=]() {
            sljit2_set_label(*from.jmp, *to.label);
        });
    }

//    template<typename R, typename A1, typename A2>
//    void invoke(R (*fn)(A1, A2), Mem arg1, Mem arg2, Mem return_value) {
//        sljit2_s32 arg_types = ArgTypeOf<R>::value;
//        arg_types |= ArgTypeOf<A1>::value << 4;
//        arg_types |= ArgTypeOf<A2>::value << 8;
//
//        auto temp_safe_storage = local_variable<>()
//
//        required_registers = std::max(required_registers, ArgTypeOf<R>::uses_int_reg());
//        required_float_registers = std::max(required_float_registers, ArgTypeOf<R>::uses_float_reg());
//
//        args = std::max(args,
//            ArgTypeOf<A1>::uses_int_reg()
//                + ArgTypeOf<A2>::uses_int_reg()
//        );
//        float_args = std::max(float_args,
//            ArgTypeOf<A1>::uses_float_reg()
//                + ArgTypeOf<A2>::uses_float_reg()
//        );
//
//        return reinterpret_cast<R (*)(A1, A2)>(_compile(arg_types));
//    }

    static void _memcpy(ptr_jt dst, ptr_jt src, int_jt bytes) {
        memcpy(reinterpret_cast<void*>(dst), reinterpret_cast<void*>(src), (size_t)bytes);
    }

    void mem_copy(Mem src_ptr, Mem dst_ptr, Mem bytes_to_copy) {
        auto fn = sljit2_emit_call(compiler, SLJIT2_CALL, SLJIT2_ARGS3V(P, P, W));
        sljit2_set_target(fn, SLJIT2_FUNC_UADDR(_memcpy));
    }

    static void _malloc(int_jt bytes) {
        malloc((size_t)bytes);
    }

    Mem mem_alloc(Mem bytes_to_copy) {
        auto fn = sljit2_emit_call(compiler, SLJIT2_CALL, SLJIT2_ARGS3V(P, P, W));
        sljit2_set_target(fn, SLJIT2_FUNC_UADDR(_memcpy));
    }

    static void _delete(ptr_jt ptr) {
        free(reinterpret_cast<void*>(ptr));
    }

    Mem mem_delete(Mem bytes_to_copy) {
        auto fn = sljit2_emit_call(compiler, SLJIT2_CALL, SLJIT2_ARGS3V(P, P, W));
        sljit2_set_target(fn, SLJIT2_FUNC_UADDR(_memcpy));
    }

    template<typename T>
    void free_fn(T &fn) {
        sljit2_free_code(reinterpret_cast<void*>(fn), nullptr);
    }

    ~Asm() {
        sljit2_free_compiler(compiler);
        backings.clear();
    }
};


#endif //SLJIT2_ASM_H
