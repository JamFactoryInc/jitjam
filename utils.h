//
// Created by jam on 16/09/2024.
//

#ifndef JITJAM_UTILS_H
#define JITJAM_UTILS_H

#include <memory>
#include <type_traits>
#include <iostream>
#include <cstdint>
#include <libunwind.h>
#include <cxxabi.h>


struct Arg {
    std::string str;

    template<typename T>
    Arg(T arg): str(std::to_string(arg)) { }

    Arg(const char *arg): str(std::string(arg)) { }

    Arg(std::string arg): str(arg) { }
};

namespace logger {
    void _format(std::basic_ostream<char> &ostream, const char *msg, std::initializer_list<Arg> args);
    void error(const char *msg, std::initializer_list<Arg> args = {});
    void debug(const char *msg, std::initializer_list<Arg> args = {});
    void info(const char *msg, std::initializer_list<Arg> args = {});
}

struct Jassert {

    // Asserts that the passed condition is true.
    // The second arg may be a message string, and following args will be used as format args for the message
    // Example: check(false, "Assertion failed: {}", { x })
    // Where x = 10, this prints "[ERROR] Assertion failed: 10" and panics
    static void check(bool cond, const char *msg = nullptr, std::initializer_list<Arg> args = {});
};

namespace utils {
    void print_backtrace(bool skip_first = false);
    void raise(const char *msg, std::initializer_list<Arg> args);

    std::string demangle_type_name(const char* mangled_name);
}

#endif //JITJAM_UTILS_H
