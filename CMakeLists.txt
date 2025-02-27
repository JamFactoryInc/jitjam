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
        compiler/preprocess/bytecode.h
        sljit/src/sljitLir.h sljit/src/sljitLir.cpp
        compiler/mempack/memory_prealloc.h
        sljit_interop/arg_type_utils.h
        timeline_map.h
        compiler/preprocess/bytecode.cpp
        compiler/preprocess/runtime_values.h
        compiler/preprocess/handles.h
        compiler/preprocess/variable_bounds.h
        compiler/preprocess/engine/preprocess_engine.h
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