
//

#include "test_variable_bounds.h"

TEST_SUITE("Variable Bounds Tests") {

    TEST_CASE("test int addition")
    {
        SUBCASE("+ +") {
            ResultFlags flags = ResultFlags::None;
            VariableBounds lhs = { 0l, 1l };
            VariableBounds rhs = { 2l, 3l };
            auto result = VariableBounds::add(
                lhs,
                rhs,
                flags
            ).as_int();

            CHECK_EQ(result.get_min(), 0l + 2l);
            CHECK_EQ(result.get_max(), 1l + 3l);
        }

        SUBCASE("+ -") {
            ResultFlags flags = ResultFlags::None;
            VariableBounds lhs = { 0l, 3l };
            VariableBounds rhs = { -4l, -2l };
            auto result = VariableBounds::add(
                lhs,
                rhs,
                flags
            ).as_int();

            CHECK_EQ(result.get_min(), 0 + -4);
            CHECK_EQ(result.get_max(), 3 + -2);
        }

        SUBCASE("barely not overflowing") {
            ResultFlags flags = ResultFlags::None;
            VariableBounds lhs = { 0l, INTPTR_MAX };
            VariableBounds rhs = { 0l, 0l };
            auto result = VariableBounds::add(
                lhs,
                rhs,
                flags
            ).as_int();

            CHECK_EQ(result.get_min(), 0);
            CHECK_EQ(result.get_max(), INTPTR_MAX);
        }

        SUBCASE("barely not underflowing") {
            ResultFlags flags = ResultFlags::None;
            VariableBounds lhs = { INTPTR_MIN, 0l };
            VariableBounds rhs = { 0l, 0l };
            auto result = VariableBounds::add(
                lhs,
                rhs,
                flags
            ).as_int();

            CHECK_EQ(result.get_min(), INTPTR_MIN);
            CHECK_EQ(result.get_max(), 0);
        }

        SUBCASE("overflows 1") {
            ResultFlags flags = ResultFlags::None;
            VariableBounds lhs = { 0l, INTPTR_MAX };
            VariableBounds rhs = { 0l, 1l };
            auto result = VariableBounds::add(
                lhs,
                rhs,
                flags
            ).as_int();

            CHECK_EQ(result.get_min(), INTPTR_MIN);
            CHECK_EQ(result.get_max(), INTPTR_MAX);
        }

        SUBCASE("overflows 2") {
            ResultFlags flags = ResultFlags::None;
            VariableBounds lhs = { 0l, 1l };
            VariableBounds rhs = { 0l, INTPTR_MAX };
            auto result = VariableBounds::add(
                lhs,
                rhs,
                flags
            ).as_int();

            CHECK_EQ(result.get_min(), INTPTR_MIN);
            CHECK_EQ(result.get_max(), INTPTR_MAX);
        }

        SUBCASE("underflows 1") {
            ResultFlags flags = ResultFlags::None;
            VariableBounds lhs = { INTPTR_MIN, 0l };
            VariableBounds rhs = { -1l, 0l };
            auto result = VariableBounds::add(
                lhs,
                rhs,
                flags
            ).as_int();

            CHECK_EQ(result.get_min(), INTPTR_MIN);
            CHECK_EQ(result.get_max(), INTPTR_MAX);
        }

        SUBCASE("underflows 2") {
            ResultFlags flags = ResultFlags::None;
            VariableBounds lhs = { -1l, 0l };
            VariableBounds rhs = { INTPTR_MIN, 0l };
            auto result = VariableBounds::add(
                lhs,
                rhs,
                flags
            ).as_int();

            CHECK_EQ(result.get_min(), INTPTR_MIN);
            CHECK_EQ(result.get_max(), INTPTR_MAX);
        }
    }

    TEST_CASE("test float addition")
    {
        SUBCASE("+ +") {
            ResultFlags flags = ResultFlags::None;
            VariableBounds lhs = { 0.0, 1.0 };
            VariableBounds rhs = { 2.0, 3.0 };
            auto result = VariableBounds::add(
                lhs,
                rhs,
                flags
            ).as_float();

            CHECK_LE(result.get_min(), 0.0 + 2.0);
            CHECK_GE(result.get_max(), 1.0 + 3.0);
        }

        SUBCASE("+ -") {
            ResultFlags flags = ResultFlags::None;
            VariableBounds lhs = { 0.0, 3.0 };
            VariableBounds rhs = { -4.0, -2.0 };
            auto result = VariableBounds::add(
                lhs,
                rhs,
                flags
            ).as_float();

            CHECK_LE(result.get_min(), 0.0 + -4.0);
            CHECK_GE(result.get_max(), 3.0 + -2.0);
        }
    }

    TEST_CASE("test int subtraction")
    {
        SUBCASE("+ +") {
            ResultFlags flags = ResultFlags::None;
            VariableBounds lhs = { 0l, 1l };
            VariableBounds rhs = { 2l, 3l };
            auto result = VariableBounds::subtract(
                lhs,
                rhs,
                flags
            ).as_int();

            CHECK_EQ(result.get_min(), 0l - 3l);
            CHECK_EQ(result.get_max(), 1l - 2l);
        }

        SUBCASE("+ -") {
            ResultFlags flags = ResultFlags::None;
            VariableBounds lhs = { 0l, 3l };
            VariableBounds rhs = { -4l, -2l };
            auto result = VariableBounds::subtract(
                lhs,
                rhs,
                flags
            ).as_int();

            CHECK_EQ(result.get_min(), 0 - -2);
            CHECK_EQ(result.get_max(), 3 - -4);
        }

        SUBCASE("barely not overflowing 1") {
            ResultFlags flags = ResultFlags::None;
            VariableBounds lhs = { 0l, INTPTR_MAX };
            VariableBounds rhs = { 0l, 0l };
            auto result = VariableBounds::subtract(
                lhs,
                rhs,
                flags
            ).as_int();

            CHECK_EQ(result.get_min(), 0);
            CHECK_EQ(result.get_max(), INTPTR_MAX);
        }

        SUBCASE("barely not overflowing 2") {
            ResultFlags flags = ResultFlags::None;
            VariableBounds lhs = { 0l, 0l };
            VariableBounds rhs = { INTPTR_MIN + 1, 0l };
            auto result = VariableBounds::subtract(
                lhs,
                rhs,
                flags
            ).as_int();

            CHECK_EQ(result.get_min(), 0);
            CHECK_EQ(result.get_max(), INTPTR_MAX);
        }

        SUBCASE("barely not underflowing 1") {
            ResultFlags flags = ResultFlags::None;
            VariableBounds lhs = { INTPTR_MIN, 0l };
            VariableBounds rhs = { 0l, 0l };
            auto result = VariableBounds::subtract(
                lhs,
                rhs,
                flags
            ).as_int();

            CHECK_EQ(result.get_min(), INTPTR_MIN);
            CHECK_EQ(result.get_max(), 0);
        }

        SUBCASE("barely not underflowing 2") {
            ResultFlags flags = ResultFlags::None;
            VariableBounds lhs = { -1l, 0l };
            VariableBounds rhs = { 0l, INTPTR_MAX };
            auto result = VariableBounds::subtract(
                lhs,
                rhs,
                flags
            ).as_int();

            CHECK_EQ(result.get_min(), INTPTR_MIN);
            CHECK_EQ(result.get_max(), 0);
        }

        SUBCASE("overflows 1") {
            ResultFlags flags = ResultFlags::None;
            VariableBounds lhs = { 0l, INTPTR_MAX };
            VariableBounds rhs = { -1l, 0l };
            auto result = VariableBounds::subtract(
                lhs,
                rhs,
                flags
            ).as_int();

            CHECK_EQ(result.get_min(), INTPTR_MIN);
            CHECK_EQ(result.get_max(), INTPTR_MAX);
        }

        SUBCASE("overflows 2") {
            ResultFlags flags = ResultFlags::None;
            VariableBounds lhs = { 0l, 0l };
            VariableBounds rhs = { INTPTR_MIN, 0l };
            auto result = VariableBounds::subtract(
                lhs,
                rhs,
                flags
            ).as_int();

            CHECK_EQ(result.get_min(), INTPTR_MIN);
            CHECK_EQ(result.get_max(), INTPTR_MAX);
        }

        SUBCASE("underflows 1") {
            ResultFlags flags = ResultFlags::None;
            VariableBounds lhs = { INTPTR_MIN, 0l };
            VariableBounds rhs = { 0l, 1l };
            auto result = VariableBounds::subtract(
                lhs,
                rhs,
                flags
            ).as_int();

            CHECK_EQ(result.get_min(), INTPTR_MIN);
            CHECK_EQ(result.get_max(), INTPTR_MAX);
        }

        SUBCASE("underflows 2") {
            ResultFlags flags = ResultFlags::None;
            VariableBounds lhs = { -2l, 0l };
            VariableBounds rhs = { 0l, INTPTR_MAX };
            auto result = VariableBounds::subtract(
                lhs,
                rhs,
                flags
            ).as_int();

            CHECK_EQ(result.get_min(), INTPTR_MIN);
            CHECK_EQ(result.get_max(), INTPTR_MAX);
        }
    }

    TEST_CASE("test float subtraction")
    {
        SUBCASE("+ +") {
            ResultFlags flags = ResultFlags::None;
            VariableBounds lhs = { 0.0, 1.0 };
            VariableBounds rhs = { 2.0, 3.0 };
            auto result = VariableBounds::subtract(
                lhs,
                rhs,
                flags
            ).as_float();

            CHECK_LE(result.get_min(), 0.0 - 3.0);
            CHECK_GE(result.get_max(), 1.0 - 2.0);
        }

        SUBCASE("+ -") {
            ResultFlags flags = ResultFlags::None;
            VariableBounds lhs = { 0.0, 3.0 };
            VariableBounds rhs = { -4.0, -2.0 };
            auto result = VariableBounds::subtract(
                lhs,
                rhs,
                flags
            ).as_float();

            CHECK_LE(result.get_min(), 0.0 - -2.0);
            CHECK_GE(result.get_max(), 3.0 - -4.0);
        }
    }

    TEST_CASE("test int multiplication")
    {
        SUBCASE("++ * ++") {
            ResultFlags flags = ResultFlags::None;
            VariableBounds lhs = { 1l, 4l };
            VariableBounds rhs = { 2l, 5l };
            auto result = VariableBounds::multiply(
                lhs,
                rhs,
                flags
            ).as_int();

            CHECK_EQ(result.get_min(), 1l * 2l);
            CHECK_EQ(result.get_max(), 4l * 5l);
        }

        SUBCASE("++ * -+") {
            ResultFlags flags = ResultFlags::None;
            VariableBounds lhs = { 1l, 4l };
            VariableBounds rhs = { -3l, 5l };
            auto result = VariableBounds::multiply(
                lhs,
                rhs,
                flags
            ).as_int();

            CHECK_EQ(result.get_min(), 4l * -3l);
            CHECK_EQ(result.get_max(), 4l * 5l);
        }

        SUBCASE("++ * --") {
            ResultFlags flags = ResultFlags::None;
            VariableBounds lhs = { 1l, 4l };
            VariableBounds rhs = { -7l, -3l };
            auto result = VariableBounds::multiply(
                lhs,
                rhs,
                flags
            ).as_int();

            CHECK_EQ(result.get_min(), 4l * -7l);
            CHECK_EQ(result.get_max(), 1l * -3l);
        }

        SUBCASE("-+ * --") {
            ResultFlags flags = ResultFlags::None;
            VariableBounds lhs = { -1l, 4l };
            VariableBounds rhs = { -7l, -3l };
            auto result = VariableBounds::multiply(
                lhs,
                rhs,
                flags
            ).as_int();

            CHECK_EQ(result.get_min(), 4l * -7l);
            CHECK_EQ(result.get_max(), -1l * -7l);
        }

        SUBCASE("-- * --") {
            ResultFlags flags = ResultFlags::None;
            VariableBounds lhs = { -1l, -4l };
            VariableBounds rhs = { -7l, -3l };
            auto result = VariableBounds::multiply(
                lhs,
                rhs,
                flags
            ).as_int();

            CHECK_EQ(result.get_min(), -1l * -3l);
            CHECK_EQ(result.get_max(), -4l * -7l);
        }

        SUBCASE("overflows") {
            ResultFlags flags = ResultFlags::None;
            VariableBounds lhs = { 0l, INTPTR_MAX };
            VariableBounds rhs = { 0l, 2l };
            auto result = VariableBounds::multiply(
                lhs,
                rhs,
                flags
            ).as_int();

            CHECK_EQ(result.get_min(), INTPTR_MIN);
            CHECK_EQ(result.get_max(), INTPTR_MAX);
        }

        SUBCASE("underflows") {
            ResultFlags flags = ResultFlags::None;
            VariableBounds lhs = { INTPTR_MIN, 0l };
            VariableBounds rhs = { 0l, 2l };
            auto result = VariableBounds::multiply(
                lhs,
                rhs,
                flags
            ).as_int();

            CHECK_EQ(result.get_min(), INTPTR_MIN);
            CHECK_EQ(result.get_max(), INTPTR_MAX);
        }

        SUBCASE("technically doesn't overflow") {
            ResultFlags flags = ResultFlags::None;
            VariableBounds lhs = { 0l, (INTPTR_MAX / 2) + 1 };
            VariableBounds rhs = { -2, -2l };
            auto result = VariableBounds::multiply(
                lhs,
                rhs,
                flags
            ).as_int();

            CHECK_EQ(result.get_min(), INTPTR_MIN);
            CHECK_EQ(result.get_max(), 0l);
        }
    }

    TEST_CASE("test float multiplication")
    {
        SUBCASE("++ * ++") {
            ResultFlags flags = ResultFlags::None;
            VariableBounds lhs = { 1.0, 2.0 };
            VariableBounds rhs = { 3.0, 4.0 };
            auto result = VariableBounds::multiply(
                lhs,
                rhs,
                flags
            ).as_float();

            CHECK_LE(result.get_min(), 1.0 * 3.0);
            CHECK_GE(result.get_max(), 2.0 * 4.0);
        }

        SUBCASE("++ * -+") {
            ResultFlags flags = ResultFlags::None;
            VariableBounds lhs = { 1.0, 2.0 };
            VariableBounds rhs = { -3.0, 4.0 };
            auto result = VariableBounds::multiply(
                lhs,
                rhs,
                flags
            ).as_float();

            CHECK_LE(result.get_min(), 2.0 * -3.0);
            CHECK_GE(result.get_max(), 2.0 * 4.0);
        }

        SUBCASE("-+ * ++") {
            ResultFlags flags = ResultFlags::None;
            VariableBounds lhs = { -1.0, 2.0 };
            VariableBounds rhs = { 3.0, 4.0 };
            auto result = VariableBounds::multiply(
                lhs,
                rhs,
                flags
            ).as_float();

            CHECK_LE(result.get_min(), -1.0 * 4.0);
            CHECK_GE(result.get_max(), 2.0 * 4.0);
        }

        SUBCASE("-+ * -+") {
            ResultFlags flags = ResultFlags::None;
            VariableBounds lhs = { -7.0, 2.0 };
            VariableBounds rhs = { -3.0, 4.0 };
            auto result = VariableBounds::multiply(
                lhs,
                rhs,
                flags
            ).as_float();

            CHECK_LE(result.get_min(), -7.0 * 4.0);
            CHECK_GE(result.get_max(), -7.0 * -3.0);
        }

        SUBCASE("-+ * --") {
            ResultFlags flags = ResultFlags::None;
            VariableBounds lhs = { -7.0, 2.0 };
            VariableBounds rhs = { -3.0, -4.0 };
            auto result = VariableBounds::multiply(
                lhs,
                rhs,
                flags
            ).as_float();

            CHECK_LE(result.get_min(), 2.0 * -4.0);
            CHECK_GE(result.get_max(), -7.0 * -4.0);
        }

        SUBCASE("-- * --") {
            ResultFlags flags = ResultFlags::None;
            VariableBounds lhs = { -7.0, -2.0 };
            VariableBounds rhs = { -3.0, -4.0 };
            auto result = VariableBounds::multiply(
                lhs,
                rhs,
                flags
            ).as_float();

            CHECK_LE(result.get_min(), -2.0 * -3.0);
            CHECK_GE(result.get_max(), -7.0 * -4.0);
        }
    }

    TEST_CASE("test int division")
    {
        SUBCASE("++ / 0..2") {
            VariableBounds lhs = { 2l, 4l };
            VariableBounds rhs = { 0l, 2l };
            ResultFlags flags = ResultFlags::None;
            auto result = VariableBounds::divide(
                lhs,
                rhs,
                flags
            ).as_int();

            CHECK_EQ(lhs.as_int().get_min(), 2l);
            CHECK_EQ(lhs.as_int().get_max(), 4l);

            CHECK_EQ(rhs.as_int().get_min(), 1l);
            CHECK_EQ(rhs.as_int().get_max(), 2l);

            CHECK_EQ(result.get_min(), 1l);
            CHECK_EQ(result.get_max(), 4l);
        }

        SUBCASE("++ / -2..0") {
            VariableBounds lhs = { 2l, 4l };
            VariableBounds rhs = { -2l, 0l };
            ResultFlags flags = ResultFlags::None;
            auto result = VariableBounds::divide(
                lhs,
                rhs,
                flags
            ).as_int();

            CHECK_EQ(lhs.as_int().get_min(), 2l);
            CHECK_EQ(lhs.as_int().get_max(), 4l);

            CHECK_EQ(rhs.as_int().get_min(), -2l);
            CHECK_EQ(rhs.as_int().get_max(), -1l);

            CHECK_EQ(result.get_min(), -4l);
            CHECK_EQ(result.get_max(), -1l);
        }

        SUBCASE("++ / ++") {
            VariableBounds lhs = { 1l, 4l };
            VariableBounds rhs = { 2l, 5l };
            ResultFlags flags = ResultFlags::None;
            auto result = VariableBounds::divide(
                lhs,
                rhs,
                flags
            ).as_int();

            CHECK_EQ(result.get_min(), 1l / 5l);
            CHECK_EQ(result.get_max(), 4l / 2l);
        }

        SUBCASE("++ / -+") {
            VariableBounds lhs = { 1l, 6l };
            VariableBounds rhs = { -3l, 5l };
            ResultFlags flags = ResultFlags::None;
            auto result = VariableBounds::divide(
                lhs,
                rhs,
                flags
            ).as_int();

            CHECK_EQ(result.get_min(), 6l / -3l);
            CHECK_EQ(result.get_max(), 6l / 5l);
        }

        SUBCASE("++ / --") {
            VariableBounds lhs = { 4l, 7l };
            VariableBounds rhs = { -3l, -1l };
            ResultFlags flags = ResultFlags::None;
            auto result = VariableBounds::divide(
                lhs,
                rhs,
                flags
            ).as_int();

            CHECK_EQ(result.get_min(), 7l / -1l);
            CHECK_EQ(result.get_max(), 3l / -3l);
        }

        SUBCASE("-+ / --") {
            VariableBounds lhs = { -7l, 4l };
            VariableBounds rhs = { -3l, -1l };
            ResultFlags flags = ResultFlags::None;
            auto result = VariableBounds::divide(
                lhs,
                rhs,
                flags
            ).as_int();

            CHECK_EQ(result.get_min(), 4l / -1l);
            CHECK_EQ(result.get_max(), -7l / -1l);
        }

        SUBCASE("-- / --") {
            VariableBounds lhs = { -7l, -4l };
            VariableBounds rhs = { -3l, -1l };
            ResultFlags flags = ResultFlags::None;
            auto result = VariableBounds::divide(
                lhs,
                rhs,
                flags
            ).as_int();

            CHECK_EQ(result.get_min(), -4l / -3l);
            CHECK_EQ(result.get_max(), -7l / -1l);
        }
    }

    TEST_CASE("test float division")
    {

        SUBCASE("++ / ++") {
            VariableBounds lhs = { 1.0, 4.0 };
            VariableBounds rhs = { 2.0, 5.0 };
            ResultFlags flags = ResultFlags::None;
            auto result = VariableBounds::divide(
                lhs,
                rhs,
                flags
            ).as_float();

            CHECK_LE(result.get_min(), 1.0 / 5.0);
            CHECK_GE(result.get_max(), 4.0 / 2.0);
        }

        SUBCASE("++ / -+") {
            VariableBounds lhs = { 1.0, 6.0 };
            VariableBounds rhs = { -3.0, 5.0 };
            ResultFlags flags = ResultFlags::None;
            auto result = VariableBounds::divide(
                lhs,
                rhs,
                flags
            ).as_float();

            CHECK_LE(result.get_min(), 6.0 / -3.0);
            CHECK_GE(result.get_max(), 6.0 / 5.0);
        }

        SUBCASE("++ / --") {
            VariableBounds lhs = { 3.0, 7.0 };
            VariableBounds rhs = { -3.0, -1.0 };
            ResultFlags flags = ResultFlags::None;
            auto result = VariableBounds::divide(
                lhs,
                rhs,
                flags
            ).as_float();

            CHECK_LE(result.get_min(), 7.0 / -1.0);
            CHECK_GE(result.get_max(), 3.0 / -3.0);
        }

        SUBCASE("-+ / --") {
            VariableBounds lhs = { -7.0, 4.0 };
            VariableBounds rhs = { -3.0, -1.0 };
            ResultFlags flags = ResultFlags::None;
            auto result = VariableBounds::divide(
                lhs,
                rhs,
                flags
            ).as_float();

            CHECK_LE(result.get_min(), 4.0 / -1.0);
            CHECK_GE(result.get_max(), -7.0 / -1.0);
        }

        SUBCASE("-- / --") {
            VariableBounds lhs = { -7.0, -4.0 };
            VariableBounds rhs = { -3.0, -1.0 };
            ResultFlags flags = ResultFlags::None;
            auto result = VariableBounds::divide(
                lhs,
                rhs,
                flags
            ).as_float();

            CHECK_LE(result.get_min(), -4.0 / -3.0);
            CHECK_GE(result.get_max(), -7.0 / -1.0);
        }

        SUBCASE("div by zero") {
            VariableBounds lhs = { -7.0, -4.0 };
            VariableBounds rhs = { -0.0, 0.0 };
            ResultFlags flags = ResultFlags::None;
            auto result = VariableBounds::divide(
                lhs,
                rhs,
                flags
            ).as_float();

            CHECK_LE(result.get_min(), -std::numeric_limits<float_jt>::infinity());
            CHECK_GE(result.get_max(), std::numeric_limits<float_jt>::infinity());
        }
    }

    TEST_CASE("test int modulus")
    {
        SUBCASE("++ % 0..2") {
            VariableBounds lhs = { 2l, 4l };
            VariableBounds rhs = { 0l, 2l };
            ResultFlags flags = ResultFlags::None;
            auto result = VariableBounds::mod(
                lhs,
                rhs,
                flags
            ).as_int();

            CHECK_EQ(lhs.as_int().get_min(), 2l);
            CHECK_EQ(lhs.as_int().get_max(), 4l);

            CHECK_EQ(rhs.as_int().get_min(), 1l);
            CHECK_EQ(rhs.as_int().get_max(), 2l);

            CHECK_EQ(result.get_min(), 1l);
            CHECK_EQ(result.get_max(), 4l);
        }

        SUBCASE("++ % -2..0") {
            VariableBounds lhs = { 2l, 4l };
            VariableBounds rhs = { -2l, 0l };
            ResultFlags flags = ResultFlags::None;
            auto result = VariableBounds::mod(
                lhs,
                rhs,
                flags
            ).as_int();

            CHECK_EQ(lhs.as_int().get_min(), 2l);
            CHECK_EQ(lhs.as_int().get_max(), 4l);

            CHECK_EQ(rhs.as_int().get_min(), -2l);
            CHECK_EQ(rhs.as_int().get_max(), -1l);

            CHECK_EQ(result.get_min(), -4l);
            CHECK_EQ(result.get_max(), -1l);
        }

        SUBCASE("++ % ++") {
            VariableBounds lhs = { 1l, 4l };
            VariableBounds rhs = { 2l, 5l };
            ResultFlags flags = ResultFlags::None;
            auto result = VariableBounds::mod(
                lhs,
                rhs,
                flags
            ).as_int();

            CHECK_EQ(result.get_min(), 1l / 5l);
            CHECK_EQ(result.get_max(), 4l / 2l);
        }

        SUBCASE("++ % -+") {
            VariableBounds lhs = { 1l, 6l };
            VariableBounds rhs = { -3l, 5l };
            ResultFlags flags = ResultFlags::None;
            auto result = VariableBounds::mod(
                lhs,
                rhs,
                flags
            ).as_int();

            CHECK_EQ(result.get_min(), 6l / -3l);
            CHECK_EQ(result.get_max(), 6l / 5l);
        }

        SUBCASE("++ % --") {
            VariableBounds lhs = { 4l, 7l };
            VariableBounds rhs = { -3l, -1l };
            ResultFlags flags = ResultFlags::None;
            auto result = VariableBounds::mod(
                lhs,
                rhs,
                flags
            ).as_int();

            CHECK_EQ(result.get_min(), 7l / -1l);
            CHECK_EQ(result.get_max(), 3l / -3l);
        }

        SUBCASE("-+ % --") {
            VariableBounds lhs = { -7l, 4l };
            VariableBounds rhs = { -3l, -1l };
            ResultFlags flags = ResultFlags::None;
            auto result = VariableBounds::mod(
                lhs,
                rhs,
                flags
            ).as_int();

            CHECK_EQ(result.get_min(), 4l / -1l);
            CHECK_EQ(result.get_max(), -7l / -1l);
        }

        SUBCASE("-- % --") {
            VariableBounds lhs = { -7l, -4l };
            VariableBounds rhs = { -3l, -1l };
            ResultFlags flags = ResultFlags::None;
            auto result = VariableBounds::mod(
                lhs,
                rhs,
                flags
            ).as_int();

            CHECK_EQ(result.get_min(), -4l / -3l);
            CHECK_EQ(result.get_max(), -7l / -1l);
        }
    }
}