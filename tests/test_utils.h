//
// Created by jam on 17/09/2024.
//

#ifndef JITJAM_TEST_UTILS_H
#define JITJAM_TEST_UTILS_H

#include <iostream>
#include "../utils.h"

namespace assertions {
    template<typename E, typename A>
    static void _assert_equals(E expected, A actual, const char *file, int line) {
        if (expected == actual) {
            return;
        }

        std::cerr << "Assertion failed at " << file << ':' << line << std::endl
                  << "Expected: " << expected << std::endl
                  << "Actual:   " << actual << std::endl;
        FAIL("");
    }
}

#define assert_equals(expected, actual) assertions::_assert_equals(expected, actual, __FILE__, __LINE__)

#endif //JITJAM_TEST_UTILS_H
