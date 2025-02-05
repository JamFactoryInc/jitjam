//
// Created by jam on 27/09/2024.
//

#include "utils.h"

void Jassert::check(bool cond, const char *msg, std::initializer_list<Arg> args) {
    if (!cond) {
        logger::_format(std::cerr, "Assertion failed\n", {});
        if (msg) {
            logger::_format(std::cerr, msg, args);
        }
        std::cerr << std::endl;
        exit(1);
    }
}


//void Jassert::_check(const char *file, const int line, bool cond, const char *msg) {
//    Jassert::_check(file, line, cond, msg, {});
//}
//
//void Jassert::_check(const char *file, const int line, bool cond) {
//    Jassert::_check(file, line, cond, nullptr, {});
//}

inline void logger::info(const char *msg, std::initializer_list<Arg> args) {
    std::cout << "[INFO ] ";
    logger::_format(std::cout, msg, args);
    std::cout << std::endl;
}

inline void logger::debug(const char *msg, std::initializer_list<Arg> args) {
#ifdef _DEBUG_BUILD
    std::cout << "[DEBUG] ";
    logger::_format(std::cout, msg, args);
    std::cout << std::endl;
#endif
}

inline void logger::error(const char *msg, std::initializer_list<Arg> args) {
    std::cout << "[ERROR] ";
    logger::_format(std::cerr, msg, args);
    std::cout << std::endl;
}

void logger::_format(std::basic_ostream<char> &ostream, const char *msg, std::initializer_list<Arg> args) {
    std::intptr_t msg_ptr = reinterpret_cast<std::intptr_t>(msg);
    std::intptr_t bracket_start = 0;
    std::intptr_t bracket_end;
    std::intptr_t substr_start = msg_ptr;
    auto iter = args.begin();
    size_t byte;
    while ((byte = *reinterpret_cast<char *>(msg_ptr))) {
        bracket_end = (bracket_start != 0) * (byte == '}') * msg_ptr;
        bracket_start = msg_ptr * (byte == '{');
        if (bracket_end) {
            if (iter == args.end()) {
                continue;
            }
            const size_t len = bracket_end - substr_start - 1;
            ostream << std::string(reinterpret_cast<const char *>(substr_start), len);

            substr_start = bracket_end + 1;
            ostream << iter->str;
            iter++;
        }
        ++msg_ptr;
    }

    ostream << std::string(reinterpret_cast<const char *>(substr_start), msg_ptr - substr_start);

    if (iter != args.end()) {
        ostream << " {";
        while (true) {
            ostream << iter->str;
            if (++iter != args.end()) {
                ostream << ", ";
            } else {
                break;
            }
        }
        ostream << "}";
    }
}


void test(int (*wow)()) {

    wow();

}

