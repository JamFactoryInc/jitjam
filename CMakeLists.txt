cmake_minimum_required(VERSION 3.29)
project(jitjam CXX)

set(CMAKE_CXX_STANDARD 11)

IF(MACRO_ALIASING)
    add_compile_definitions(MACRO_ALIASING)
ENDIF(MACRO_ALIASING)

add_library(jitjam STATIC
        utils.cpp
        asm.h
        mem.cpp
        bytecode.h
        optimizer.h
        sljit/src/sljitLir.h sljit/src/sljitLir.cpp
        memory_prealloc.h
        _utils.h
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