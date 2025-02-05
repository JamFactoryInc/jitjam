//
// Created by jam on 16/09/2024.
//

#ifndef JITJAM_UTILS_H
#define JITJAM_UTILS_H

#include <memory>
#include <type_traits>
#include <iostream>
#include <cassert>
#include <cstdint>


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
    template<const int LINE, const char* FILE>
    struct _internal {
        const char *file;
        const int line;

//        static void _check(bool cond, const char *msg, std::initializer_list<Arg> args);
//        static void _check(bool cond, const char *msg);
//        static void _check(bool cond);

//        static void check(bool cond, const char *msg = nullptr, std::initializer_list<Arg> args = {});

    };

    // Asserts that the passed condition is true.
    // The second arg may be a message string, and following args will be used as format args for the message
    // Example: check(false, "Assertion failed: {}", { x })
    // Where x = 10, this prints "[ERROR] Assertion failed: 10" and panics
    static void check(bool cond, const char *msg = nullptr, std::initializer_list<Arg> args = {});
};



//const Jassert jassert;

//#define _check_args(...) _internal::_check(__VA_ARGS__)
//
//#undef jassert
//#define jassert Jassert::_internal { __FILE__, __LINE__ }


//constexpr std::initializer_list<char> test1(const char test[1]) {
//    return {};
//}
//
//
//void test() {
//    static constexpr const char *X = "";
//    Jassert::_internal<1, X>;
//}

#endif //JITJAM_UTILS_H
