cmake_minimum_required(VERSION 3.28.1)
project(jitjam CXX)

set(CMAKE_CXX_STANDARD 11)

IF(MACRO_ALIASING)
    add_compile_definitions(MACRO_ALIASING)
ENDIF(MACRO_ALIASING)

add_library(jitjam STATIC
        utils.cpp
        compiler/codegen/asm.h
        compiler/codegen/mem.cpp
        sljit/src/sljitLir.h sljit/src/sljitLir.cpp
        compiler/mempack/memory_prealloc.h
        sljit_interop/arg_type_utils.h
        timeline_map.h
        compiler/preprocess/constraints/runtime_values.h
        handles/handles.h
        compiler/preprocess/constraints/variable_bounds.h
        compiler/preprocess/engine/preprocess_engine.h
        handles/handle_impls.h
        config/jitjam_config.h
        config/operator_overloads.h
        config/type_conversions.h
        compiler/opcodes.h
        compiler/preprocess/engine/preprocess_engine.cpp
        compiler/preprocess/constraints/constraint_value.h
        compiler/preprocess/instruction.h
        compiler/preprocess/var.h
        compiler/preprocess/block.h
        compiler/preprocess/constraints/variable_constraint.h
        compiler/preprocess/constraints/variable_constraints.h
        compiler/preprocess/variable_type.h
        compiler/preprocess/block.cpp
)

add_executable(tests tests/doctest.h
        tests/test_main.h
        tests/test_utils.h
        tests/jit_codegen_test.h
        tests/jit_codegen_test.cpp
        tests/memory_prealloc_test.h
        tests/memory_prealloc_test.cpp
)

target_link_libraries(tests jitjam)