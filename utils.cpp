//
// Created by jam on 27/09/2024.
//

#include "utils.h"

void Jassert::check(bool cond, const char *msg, std::initializer_list<Arg> args) {
    if (!cond) {
        utils::raise(msg, args);
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
    std::cerr << "[ERROR] ";
    logger::_format(std::cerr, msg, args);
    std::cerr << std::endl;
}

void logger::_format(std::basic_ostream<char> &ostream, const char *msg, std::initializer_list<Arg> args) {
    auto msg_ptr = reinterpret_cast<std::intptr_t>(msg);
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

void utils::print_backtrace(bool skip_first) {
    unw_cursor_t cursor;
    unw_context_t context;

    // Initialize cursor to current frame for local unwinding.
    unw_getcontext(&context);
    unw_init_local(&cursor, &context);


    if (skip_first && unw_step(&cursor) <= 0) {
        return;
    }
    // Unwind frames one by one, going up the frame stack.
    while (unw_step(&cursor) > 0) {
        unw_word_t offset, pc;
        unw_get_reg(&cursor, UNW_REG_IP, &pc);
        if (pc == 0) {
            break;
        }

        char sym[256];
        if (unw_get_proc_name(&cursor, sym, sizeof(sym), &offset) == 0) {
            char* nameptr = sym;
            int status;
            char* demangled = abi::__cxa_demangle(sym, nullptr, nullptr, &status);
            if (status == 0) {
                nameptr = demangled;
            }
            std::cerr << "at: " << nameptr << " + " << offset << std::endl;
            std::free(demangled);
        }
    }
}

void utils::raise(const char *msg, std::initializer_list<Arg> args) {
    logger::error(msg, args);
    print_backtrace(true);
    std::exit(1);
}

std::string utils::demangle_type_name(const char* mangled_name) {
    int status;
    std::string tname = mangled_name;
    char *demangled_name = abi::__cxa_demangle(tname.c_str(), nullptr, nullptr, &status);
    if(status == 0) {
        tname = demangled_name;
        std::free(demangled_name);
    }
    return tname;
}

