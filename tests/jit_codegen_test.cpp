//
// Created by jam on 13/09/2024.
//

#include "jit_codegen_test.h"

TEST_SUITE("Codegen Tests") {

#define assert_state(gen, _stack_size, _required_registers, _required_float_registers, _args, _float_args) \
        CHECK_EQ(_stack_size, gen.stack_size);\
        CHECK_EQ(_required_registers, gen.required_registers);\
        CHECK_EQ(_required_float_registers, gen.required_float_registers);\
        CHECK_EQ(_args, gen.args);\
        CHECK_EQ(_float_args, gen.float_args);


    TEST_CASE("test sum of args") {
        Asm gen = Asm();

        auto a = gen.arg(0);
        auto b = gen.arg(1);
        auto res = gen.add(a, b, gen.reg());
        gen.return_value(res);

        auto fn = gen.compile<int_jt, int_jt, int_jt>();
        int_jt result = fn(1, 2);

        CHECK_EQ(3, result);
        assert_state(gen, 0, 3, 0, 2, 0)

        gen.free_fn(fn);
    }

    TEST_CASE("test sum of registers") {
        Asm gen = Asm();

        auto a = gen.reg();
        auto b = gen.reg();
        gen.set_const(a, 2);
        gen.set_const(b, 3);
        auto res = gen.add(a, b, Mem::R0);
        gen.return_value(res);

        auto fn = gen.compile<int_jt>();
        int_jt result = fn();

        CHECK_EQ(5, result);
        assert_state(gen, 0, 4, 0, 0, 0)

        gen.free_fn(fn);
    }

    TEST_CASE("test sum of locals") {
        Asm gen = Asm();

        auto a = gen.local_variable<int_jt>();
        auto b = gen.local_variable<int_jt>();
        gen.set_const(a, 2);
        gen.set_const(b, 3);
        auto res = gen.add(a, b, gen.reg());
        gen.return_value(res);

        auto fn = gen.compile<int_jt>();
        int_jt result = fn();

        CHECK_EQ(5, result);
        assert_state(gen, 16, 3, 0, 0, 0)

        gen.free_fn(fn);
    }

    TEST_CASE("test return local") {
        Asm gen = Asm();

        auto a = gen.local_variable<int_jt>();
        gen.set_const(a, 2);
        gen.return_value(a);

        auto fn = gen.compile<int_jt>();
        int_jt result = fn();

        CHECK_EQ(2, result);
        assert_state(gen, 8, 1, 0, 0, 0)

        gen.free_fn(fn);
    }

    TEST_CASE("test 0 args") {
        Asm gen = Asm();

        Mem reg = gen.set_const(gen.reg(), 3);
        gen.return_value(reg);

        auto fn = gen.compile<int_jt>();
        int_jt result = fn();

        CHECK_EQ(3, result);
        assert_state(gen, 0, 3, 0, 0, 0)

        gen.free_fn(fn);
    }

    TEST_CASE("test 1 arg") {
        Asm gen = Asm();

        auto a = gen.arg(0);
        gen.return_value(a);

        auto fn = gen.compile<int_jt, int_jt>();
        int_jt result = fn(1);

        CHECK_EQ(1, result);
        assert_state(gen, 0, 1, 0, 1, 0)

        gen.free_fn(fn);
    }

    TEST_CASE("test 2 args") {
        Asm gen = Asm();

        auto a = gen.arg(0);
        auto b = gen.arg(1);
        auto res = gen.add(a, b, gen.reg());
        gen.return_value(res);

        auto fn = gen.compile<int_jt, int_jt, int_jt>();
        int_jt result = fn(1, 2);

        CHECK_EQ(1 + 2, result);
        assert_state(gen, 0, 3, 0, 2, 0)

        gen.free_fn(fn);
    }

    TEST_CASE("test 3 args") {
        Asm gen = Asm();

        auto a = gen.arg(0);
        auto b = gen.arg(1);
        auto c = gen.arg(2);
        auto res = gen.add(a, gen.add(b, c, Mem::R0), Mem::R0);
        gen.return_value(res);

        auto fn = gen.compile<int_jt, int_jt, int_jt, int_jt>();
        int_jt result = fn(1, 2, 3);

        CHECK_EQ(1 + 2 + 3, result);
        assert_state(gen, 0, 1, 0, 3, 0)

        gen.free_fn(fn);
    }

    TEST_CASE("test 4 args") {
        Asm gen = Asm();

        auto a = gen.arg(0);
        auto b = gen.arg(1);
        auto c = gen.arg(2);
        auto d = gen.arg(3);
        auto res = gen.add(a, gen.add(b, gen.add(c, d, Mem::R0), Mem::R0), Mem::R0);
        gen.return_value(res);

        auto fn = gen.compile<int_jt, int_jt, int_jt, int_jt, int_jt>();
        int_jt result = fn(1, 2, 3, 4);

        CHECK_EQ(1 + 2 + 3 + 4, result);
        assert_state(gen, 0, 1, 0, 4, 0)

        gen.free_fn(fn);
    }

    TEST_CASE("test 0 float args") {
        Asm gen = Asm();


        Mem freg = gen.set_constf(gen.float_reg(), 3.0);
        gen.return_value(freg);

        auto fn = gen.compile<float_jt>();
        float_jt result = fn();

        CHECK_EQ(3.0, result);
        assert_state(gen, 0, 0, 2, 0, 0)

        gen.free_fn(fn);
    }

    TEST_CASE("test 1 float arg") {
        Asm gen = Asm();

        auto a = gen.float_arg(0);
        gen.return_value(a);

        auto fn = gen.compile<float_jt, float_jt>();
        float_jt result = fn(1.0);

        CHECK_EQ(1.0, result);
        assert_state(gen, 0, 0, 1, 0, 1)

        gen.free_fn(fn);
    }

    TEST_CASE("test 2 float args") {
        Asm gen = Asm();

        auto a = gen.float_arg(0);
        auto b = gen.float_arg(1);
        auto res = gen.add(a, b, Mem::FR0);
        gen.return_value(res);

        auto fn = gen.compile<float_jt, float_jt, float_jt>();
        float_jt result = fn(1.0, 2.0);

        CHECK_EQ(1.0 + 2.0, result);
        assert_state(gen, 0, 0, 2, 0, 2)

        gen.free_fn(fn);
    }

    TEST_CASE("test 3 float args") {
        Asm gen = Asm();

        auto a = gen.float_arg(0);
        auto b = gen.float_arg(1);
        auto c = gen.float_arg(2);
        auto res = gen.add(a, gen.add(b, c, Mem::FR0), Mem::FR0);
        gen.return_value(res);

        auto fn = gen.compile<float_jt, float_jt, float_jt, float_jt>();
        float_jt result = fn(1.0, 2.0, 3.0);

        CHECK_EQ(1.0 + 2.0 + 3.0, result);
        assert_state(gen, 0, 0, 3, 0, 3)

        gen.free_fn(fn);
    }

    TEST_CASE("test 4 float args") {
        Asm gen = Asm();

        auto a = gen.float_arg(0);
        auto b = gen.float_arg(1);
        auto c = gen.float_arg(2);
        auto d = gen.float_arg(3);
        auto res = gen.add(a, gen.add(b, gen.add(c, d, Mem::FR0), Mem::FR0), Mem::FR0);
        gen.return_value(res);

        auto fn = gen.compile<float_jt, float_jt, float_jt, float_jt, float_jt>();
        float_jt result = fn(1.0, 2.0, 3.0, 4.0);

        CHECK_EQ(1.0 + 2.0 + 3.0 + 4.0, result);
        assert_state(gen, 0, 0, 4, 0, 4)

        gen.free_fn(fn);
    }

    TEST_CASE("test int - int") {
        Asm gen = Asm();

        auto a = gen.arg(0);
        auto b = gen.arg(1);
        auto res = gen.sub(a, b, Mem::R0);
        gen.return_value(res);

        auto fn = gen.compile<int_jt, int_jt, int_jt>();
        int_jt result = fn(11, 2);

        CHECK_EQ(9, result);
        assert_state(gen, 0, 1, 0, 2, 0)

        gen.free_fn(fn);
    }

    TEST_CASE("test int * int") {
        Asm gen = Asm();

        auto a = gen.arg(0);
        auto b = gen.arg(1);
        auto res = gen.mul(a, b, Mem::R0);
        gen.return_value(res);

        auto fn = gen.compile<int_jt, int_jt, int_jt>();
        int_jt result = fn(3, 5);

        CHECK_EQ(15, result);
        assert_state(gen, 0, 1, 0, 2, 0)

        gen.free_fn(fn);
    }

    TEST_CASE("test int / int") {
        Asm gen = Asm();

        auto a = gen.arg(0);
        auto b = gen.arg(1);
        auto res = gen.div(a, b, Mem::R0);
        gen.return_value(res);

        auto fn = gen.compile<int_jt, int_jt, int_jt>();
        int_jt result = fn(11, 2);

        CHECK_EQ(5, result);
        assert_state(gen, 0, 2, 0, 2, 0)

        gen.free_fn(fn);
    }

//    TEST_CASE("test int / int power of 2 optimization") {
//        Asm gen = Asm();
//
//        auto a = gen.arg(0);
//        auto b = gen.arg(1);
//
//        int_jt quotient = 0;
//        int_jt remainder = 0;
//
//        auto addr_quotient = gen.absolute_address(&quotient);
//        auto addr_remainder = gen.absolute_address(&remainder);
//
//        auto res = gen.div(a, b, gen.reg());
//        gen.move(&gen.TR0, addr_quotient);
//        gen.move(&gen.TR1, addr_remainder);
//
//        auto fn = gen.compile<void, int_jt, int_jt>();
//        fn(11, 3);
//
//        CHECK_EQ(11 / 3, addr_quotient);
//        CHECK_EQ(11 % 3, addr_remainder);
//        assert_state(gen, 0, 2, 0, 2, 0)
//
//        gen.free_fn(fn);
//    }

    TEST_CASE("test int % int") {
        Asm gen = Asm();

        auto a = gen.arg(0);
        auto b = gen.arg(1);
        auto res = gen.mod(a, b, Mem::R0);
        gen.return_value(res);

        auto fn = gen.compile<int_jt, int_jt, int_jt>();
        int_jt result = fn(15, 11);

        CHECK_EQ(4, result);
        assert_state(gen, 0, 2, 0, 2, 0)

        gen.free_fn(fn);
    }

    TEST_CASE("test int < int") {
        Asm gen = Asm();

        auto a = gen.arg(0);
        auto b = gen.arg(1);
        auto res = gen.less_than(a, b, Mem::R0);
        gen.return_value(res);

        auto fn = gen.compile<int_jt, int_jt, int_jt>();

        SUBCASE("less") {
            int_jt result = fn(0, 1);
            CHECK_EQ(true, result);
        }
        SUBCASE("eq") {
            int_jt result = fn(1, 1);
            CHECK_EQ(false, result);
        }
        SUBCASE("greater") {
            int_jt result = fn(1, 0);
            CHECK_EQ(false, result);
        }

        assert_state(gen, 0, 1, 0, 2, 0)

        gen.free_fn(fn);
    }

    TEST_CASE("test int <= int") {
        Asm gen = Asm();

        auto a = gen.arg(0);
        auto b = gen.arg(1);
        auto res = gen.less_than_or_equal(a, b, Mem::R0);
        gen.return_value(res);

        auto fn = gen.compile<int_jt, int_jt, int_jt>();

        SUBCASE("less") {
            int_jt result = fn(0, 1);
            CHECK_EQ(true, result);
        }
        SUBCASE("eq") {
            int_jt result = fn(1, 1);
            CHECK_EQ(true, result);
        }
        SUBCASE("greater") {
            int_jt result = fn(1, 0);
            CHECK_EQ(false, result);
        }

        assert_state(gen, 0, 1, 0, 2, 0)

        gen.free_fn(fn);
    }

    TEST_CASE("test int > int") {
        Asm gen = Asm();

        auto a = gen.arg(0);
        auto b = gen.arg(1);
        auto res = gen.greater_than(a, b, Mem::R0);
        gen.return_value(res);

        auto fn = gen.compile<int_jt, int_jt, int_jt>();

        SUBCASE("less") {
            int_jt result = fn(0, 1);
            CHECK_EQ(false, result);
        }
        SUBCASE("eq") {
            int_jt result = fn(1, 1);
            CHECK_EQ(false, result);
        }
        SUBCASE("greater") {
            int_jt result = fn(1, 0);
            CHECK_EQ(true, result);
        }

        assert_state(gen, 0, 1, 0, 2, 0)

        gen.free_fn(fn);
    }

    TEST_CASE("test int >= int") {
        Asm gen = Asm();

        auto a = gen.arg(0);
        auto b = gen.arg(1);
        auto res = gen.greater_than_or_equal(a, b, Mem::R0);
        gen.return_value(res);

        auto fn = gen.compile<int_jt, int_jt, int_jt>();

        SUBCASE("less") {
            int_jt result = fn(0, 1);
            CHECK_EQ(false, result);
        }
        SUBCASE("eq") {
            int_jt result = fn(1, 1);
            CHECK_EQ(true, result);
        }
        SUBCASE("greater") {
            int_jt result = fn(1, 0);
            CHECK_EQ(true, result);
        }

        assert_state(gen, 0, 1, 0, 2, 0)

        gen.free_fn(fn);
    }

    TEST_CASE("test int == int") {
        Asm gen = Asm();

        auto a = gen.arg(0);
        auto b = gen.arg(1);
        auto res = gen.equal(a, b, Mem::R0);
        gen.return_value(res);

        auto fn = gen.compile<int_jt, int_jt, int_jt>();

        SUBCASE("eq 0") {
            int_jt result = fn(0, 0);
            CHECK_EQ(true, result);
        }
        SUBCASE("eq 1") {
            int_jt result = fn(1, 1);
            CHECK_EQ(true, result);
        }
        SUBCASE("neq") {
            int_jt result = fn(0, 1);
            CHECK_EQ(false, result);
        }

        assert_state(gen, 0, 1, 0, 2, 0)

        gen.free_fn(fn);
    }

    TEST_CASE("test int != int") {
        Asm gen = Asm();

        auto a = gen.arg(0);
        auto b = gen.arg(1);
        auto res = gen.not_equal(a, b, Mem::R0);
        gen.return_value(res);

        auto fn = gen.compile<int_jt, int_jt, int_jt>();

        SUBCASE("eq 0") {
            int_jt result = fn(0, 0);
            CHECK_EQ(false, result);
        }
        SUBCASE("eq 1") {
            int_jt result = fn(1, 1);
            CHECK_EQ(false, result);
        }
        SUBCASE("neq") {
            int_jt result = fn(0, 1);
            CHECK_EQ(true, result);
        }

        assert_state(gen, 0, 1, 0, 2, 0)

        gen.free_fn(fn);
    }

    TEST_CASE("test negate int") {
        Asm gen = Asm();

        auto a = gen.arg(0);
        gen.return_value(gen.neg(a, Mem::R0));

        auto fn = gen.compile<int_jt, int_jt>();
        int_jt result = fn(2);

        CHECK_EQ(-2, result);
        assert_state(gen, 0, 1, 0, 1, 0)

        gen.free_fn(fn);
    }

    TEST_CASE("test float - float") {
        Asm gen = Asm();

        auto a = gen.float_arg(0);
        auto b = gen.float_arg(1);
        auto res = gen.sub(a, b, Mem::FR0);
        gen.return_value(res);

        auto fn = gen.compile<float_jt, float_jt, float_jt>();
        float_jt result = fn(1.0, 10.0);

        CHECK_EQ(-9.0, result);
        assert_state(gen, 0, 0, 2, 0, 2)

        gen.free_fn(fn);
    }

    TEST_CASE("test float * float") {
        Asm gen = Asm();

        auto a = gen.float_arg(0);
        auto b = gen.float_arg(1);
        auto res = gen.mul(a, b, Mem::FR0);
        gen.return_value(res);

        auto fn = gen.compile<float_jt, float_jt, float_jt>();
        float_jt result = fn(2.0, 3.0);

        CHECK_EQ(6.0, result);
        assert_state(gen, 0, 0, 2, 0, 2)

        gen.free_fn(fn);
    }

    TEST_CASE("test float / float") {
        Asm gen = Asm();

        auto a = gen.float_arg(0);
        auto b = gen.float_arg(1);
        auto res = gen.div(a, b, Mem::FR0);
        gen.return_value(res);

        auto fn = gen.compile<float_jt, float_jt, float_jt>();
        float_jt result = fn(3.0, 2.0);

        CHECK_EQ(1.5, result);
        assert_state(gen, 0, 0, 2, 0, 2)

        gen.free_fn(fn);
    }

    TEST_CASE("test negate float") {
        Asm gen = Asm();

        auto a = gen.float_arg(0);
        gen.return_value(gen.neg(a, Mem::FR0));

        auto fn = gen.compile<float_jt, float_jt>();
        float_jt result = fn(2.0);

        CHECK_EQ(-2.0, result);
        assert_state(gen, 0, 0, 1, 0, 1)

        gen.free_fn(fn);
    }

    TEST_CASE("test int + float") {
        Asm gen = Asm();

        auto a = gen.arg(0);
        auto b = gen.float_arg(0);
        auto res = gen.add(a, b, Mem::FR0);
        gen.return_value(res);

        auto fn = gen.compile<float_jt, int_jt, float_jt>();
        float_jt result = fn(1, 1.5);

        CHECK_EQ(2.5, result);
        assert_state(gen, 0, 0, 1, 1, 1)

        gen.free_fn(fn);
    }

    TEST_CASE("test float + int") {
        Asm gen = Asm();

        auto a = gen.float_arg(0);
        auto b = gen.arg(0);
        auto sum = gen.add(a, b, Mem::FR0);
        gen.return_value(sum);

        auto fn = gen.compile<float_jt, float_jt, int_jt>();
        float_jt result = fn(1.5, 1);

        CHECK_EQ(2.5, result);
        assert_state(gen, 0, 0, 1, 1, 1)

        gen.free_fn(fn);
    }

    TEST_CASE("test int - float") {
        Asm gen = Asm();

        auto a = gen.arg(0);
        auto b = gen.float_arg(0);
        auto res = gen.sub(a, b, Mem::FR0);
        gen.return_value(res);

        auto fn = gen.compile<float_jt, int_jt, float_jt>();
        float_jt result = fn(1, 1.5);

        CHECK_EQ(-0.5, result);
        assert_state(gen, 0, 0, 1, 1, 1)

        gen.free_fn(fn);
    }

    TEST_CASE("test float - int") {
        Asm gen = Asm();

        auto a = gen.float_arg(0);
        auto b = gen.arg(0);
        auto res = gen.sub(a, b, Mem::FR0);
        gen.return_value(res);

        auto fn = gen.compile<float_jt, float_jt, int_jt>();
        float_jt result = fn(1.5, 1);

        CHECK_EQ(0.5, result);
        assert_state(gen, 0, 0, 1, 1, 1)

        gen.free_fn(fn);
    }

    TEST_CASE("test int * float") {
        Asm gen = Asm();

        auto a = gen.arg(0);
        auto b = gen.float_arg(0);
        auto res = gen.mul(a, b, Mem::FR0);
        gen.return_value(res);

        auto fn = gen.compile<float_jt, int_jt, float_jt>();
        float_jt result = fn(2, 1.25);

        CHECK_EQ(2.5, result);
        assert_state(gen, 0, 0, 1, 1, 1)

        gen.free_fn(fn);
    }

    TEST_CASE("test float * int") {
        Asm gen = Asm();

        auto a = gen.float_arg(0);
        auto b = gen.arg(0);
        auto res = gen.mul(a, b, Mem::FR0);
        gen.return_value(res);

        auto fn = gen.compile<float_jt, float_jt, int_jt>();
        float_jt result = fn(1.25, 2);

        CHECK_EQ(2.5, result);
        assert_state(gen, 0, 0, 1, 1, 1)

        gen.free_fn(fn);
    }

    TEST_CASE("test int / float") {
        Asm gen = Asm();

        auto a = gen.arg(0);
        auto b = gen.float_arg(0);
        auto res = gen.div(a, b, Mem::FR0);
        gen.return_value(res);

        auto fn = gen.compile<float_jt, int_jt, float_jt>();
        float_jt result = fn(3, 2.0);

        CHECK_EQ(1.5, result);
        assert_state(gen, 0, 0, 1, 1, 1)

        gen.free_fn(fn);
    }

    TEST_CASE("test float / int") {
        Asm gen = Asm();

        auto a = gen.float_arg(0);
        auto b = gen.arg(0);
        auto res = gen.div(a, b, Mem::FR0);
        gen.return_value(res);

        auto fn = gen.compile<float_jt, float_jt, int_jt>();
        float_jt result = fn(3.0, 2);

        CHECK_EQ(1.5, result);
        assert_state(gen, 0, 0, 1, 1, 1)

        gen.free_fn(fn);
    }

    TEST_CASE("test cast int to float") {
        Asm gen = Asm();

        auto a = gen.arg(0);
        auto res = gen.int_to_float(a, Mem::FR0);
        gen.return_value(res);

        auto fn = gen.compile<float_jt, int_jt>();
        float_jt result = fn(3);

        CHECK_EQ(3.0, result);
        assert_state(gen, 0, 0, 1, 1, 0)

        gen.free_fn(fn);
    }

    TEST_CASE("test cast float to int") {
        Asm gen = Asm();

        auto a = gen.float_arg(0);
        auto res = gen.float_to_int(a, Mem::R0);
        gen.return_value(res);

        auto fn = gen.compile<int_jt, float_jt>();
        int_jt result = fn(3.5);

        CHECK_EQ(3, result);
        assert_state(gen, 0, 1, 1, 0, 1)

        gen.free_fn(fn);
    }

    TEST_CASE("test shl") {
        Asm gen = Asm();

        auto a = gen.arg(0);
        auto b = gen.arg(1);
        auto res = gen.shift_left(a, b, Mem::R0);
        gen.return_value(res);

        auto fn = gen.compile<int_jt, int_jt, int_jt>();
        SUBCASE("shift 2") {
            int_jt result = fn(1, 2);
            CHECK_EQ(1 << 2, result);
        }
        SUBCASE("shift 0") {
            int_jt result = fn(1, 0);
            CHECK_EQ(1, result);
        }

        assert_state(gen, 0, 1, 0, 2, 0)

        gen.free_fn(fn);
    }

    TEST_CASE("test shr") {
        Asm gen = Asm();

        auto a = gen.arg(0);
        auto b = gen.arg(1);
        auto res = gen.shift_right(a, b, Mem::R0);
        gen.return_value(res);

        auto fn = gen.compile<int_jt, int_jt, int_jt>();
        SUBCASE("shift 2") {
            int_jt result = fn(100, 2);
            CHECK_EQ(100 >> 2, result);
        }
        SUBCASE("shift 0") {
            int_jt result = fn(100, 0);
            CHECK_EQ(100, result);
        }

        assert_state(gen, 0, 1, 0, 2, 0)

        gen.free_fn(fn);
    }

    TEST_CASE("test int | int") {
        Asm gen = Asm();

        auto a = gen.arg(0);
        auto b = gen.arg(1);
        auto res = gen.bit_or(a, b, Mem::R0);
        gen.return_value(res);

        auto fn = gen.compile<int_jt, int_jt, int_jt>();
        int_jt result = fn(
            0b1010,
            0b1100
        );
        CHECK_EQ(0b1110, result);

        assert_state(gen, 0, 1, 0, 2, 0)

        gen.free_fn(fn);
    }

    TEST_CASE("test int ^ int") {
        Asm gen = Asm();

        auto a = gen.arg(0);
        auto b = gen.arg(1);
        auto res = gen.bit_xor(a, b, Mem::R0);
        gen.return_value(res);

        auto fn = gen.compile<int_jt, int_jt, int_jt>();
        int_jt result = fn(
            0b1010,
            0b1100
        );
        CHECK_EQ(0b0110, result);

        assert_state(gen, 0, 1, 0, 2, 0)

        gen.free_fn(fn);
    }

    TEST_CASE("test int & int") {
        Asm gen = Asm();

        auto a = gen.arg(0);
        auto b = gen.arg(1);
        auto res = gen.bit_and(a, b, Mem::R0);
        gen.return_value(res);

        auto fn = gen.compile<int_jt, int_jt, int_jt>();
        int_jt result = fn(
            0b1010,
            0b1100
        );
        CHECK_EQ(0b1000, result);

        assert_state(gen, 0, 1, 0, 2, 0)

        gen.free_fn(fn);
    }

    TEST_CASE("test zero int") {
        Asm gen = Asm();

        Mem reg = gen.reg();
        gen.set_const(reg, 10);
        gen.set_zero(reg);
        gen.return_value(reg);

        auto fn = gen.compile<int_jt>();
        int_jt result = fn();
        CHECK_EQ(0, result);

        assert_state(gen, 0, 3, 0, 0, 0)

        gen.free_fn(fn);
    }

    TEST_CASE("test int || int") {
        Asm gen = Asm();

        auto a = gen.arg(0);
        auto b = gen.arg(1);
        auto res = gen.logical_or(a, b, Mem::R0);
        gen.return_value(res);

        auto fn = gen.compile<int_jt, int_jt, int_jt>();
        SUBCASE("true || true") {
            int_jt result = fn(true, true);
            CHECK_EQ(true, result);
        }
        SUBCASE("true || false") {
            int_jt result = fn(true, false);
            CHECK_EQ(true, result);
        }
        SUBCASE("false || true") {
            int_jt result = fn(false, true);
            CHECK_EQ(true, result);
        }
        SUBCASE("false || false") {
            int_jt result = fn(false, false);
            CHECK_EQ(false, result);
        }
        SUBCASE("10 || false") {
            int_jt result = fn(10, false);
            CHECK_EQ(true, result);
        }
        SUBCASE("false || 10") {
            int_jt result = fn(false, 10);
            CHECK_EQ(true, result);
        }
        SUBCASE("10 && 10") {
            int_jt result = fn(10, 10);
            CHECK_EQ(true, result);
        }

        assert_state(gen, 0, 2, 0, 2, 0)

        gen.free_fn(fn);
    }

    TEST_CASE("test int && int") {
        Asm gen = Asm();

        auto a = gen.arg(0);
        auto b = gen.arg(1);
        auto res = gen.logical_and(a, b, Mem::R0);
        gen.return_value(res);

        auto fn = gen.compile<int_jt, int_jt, int_jt>();
        SUBCASE("true && true") {
            int_jt result = fn(true, true);
            CHECK_EQ(true, result);
        }
        SUBCASE("true && false") {
            int_jt result = fn(true, false);
            CHECK_EQ(false, result);
        }
        SUBCASE("false && true") {
            int_jt result = fn(false, true);
            CHECK_EQ(false, result);
        }
        SUBCASE("false && false") {
            int_jt result = fn(false, false);
            CHECK_EQ(false, result);
        }
        SUBCASE("10 && false") {
            int_jt result = fn(10, false);
            CHECK_EQ(false, result);
        }
        SUBCASE("false && 10") {
            int_jt result = fn(false, 10);
            CHECK_EQ(false, result);
        }
        SUBCASE("10 && 10") {
            int_jt result = fn(10, 10);
            CHECK_EQ(true, result);
        }

        assert_state(gen, 0, 2, 0, 2, 0)

        gen.free_fn(fn);
    }

    TEST_CASE("test uncond jump") {
        Asm gen = Asm();

        Mem reg = gen.reg();
        gen.set_const(reg, 1);

        auto jump = gen.jump();

        // this will be jumped over
        gen.set_const(reg, 2);

        auto label = gen.jump_label();
        gen.bind_jump(jump, label);

        gen.return_value(reg);

        auto fn = gen.compile<int_jt>();
        int_jt result = fn();
        CHECK_EQ(1, result);

        assert_state(gen, 0, 3, 0, 0, 0)

        gen.free_fn(fn);
    }


    TEST_CASE("test jump if eq") {
        Asm gen = Asm();

        auto a = gen.arg(0);
        auto b = gen.arg(1);

        // the val we're using to track which branch path is taken
        auto val = gen.set_const(gen.reg(), 1);

        auto jump_if_eq = gen.jump_if_equal(a, b);

        gen.set_const(val, 2);

        auto jump_else_end = gen.jump();

        auto label_is_eq = gen.jump_label();
        gen.bind_jump(jump_if_eq, label_is_eq);

        gen.set_const(val, 3);

        auto label_end = gen.jump_label();
        gen.bind_jump(jump_else_end, label_end);

        gen.return_value(val);

        auto fn = gen.compile<int_jt, int_jt, int_jt>();

        SUBCASE("equal") {
            int_jt result = fn(1, 1);
            CHECK_EQ(3, result);
        }
        SUBCASE("not equal") {
            int_jt result = fn(1, 0);
            CHECK_EQ(2, result);
        }

        assert_state(gen, 0, 3, 0, 2, 0)

        gen.free_fn(fn);
    }


    TEST_CASE("test jump if not eq") {
        Asm gen = Asm();

        auto a = gen.arg(0);
        auto b = gen.arg(1);

        // the val we're using to track which branch path is taken
        auto val = gen.set_const(gen.reg(), 1);

        auto jump_if_eq = gen.jump_if_not_equal(a, b);

        gen.set_const(val, 2);

        auto jump_else_end = gen.jump();

        auto label_is_eq = gen.jump_label();
        gen.bind_jump(jump_if_eq, label_is_eq);

        gen.set_const(val, 3);

        auto label_end = gen.jump_label();
        gen.bind_jump(jump_else_end, label_end);

        gen.return_value(val);

        auto fn = gen.compile<int_jt, int_jt, int_jt>();

        SUBCASE("equal") {
            int_jt result = fn(1, 1);
            CHECK_EQ(2, result);
        }
        SUBCASE("not equal") {
            int_jt result = fn(1, 0);
            CHECK_EQ(3, result);
        }

        assert_state(gen, 0, 3, 0, 2, 0)

        gen.free_fn(fn);
    }

    TEST_CASE("test jump if less") {
        Asm gen = Asm();

        auto a = gen.arg(0);
        auto b = gen.arg(1);

        // the val we're using to track which branch path is taken
        auto val = gen.set_const(gen.reg(), 1);

        auto jump_if_eq = gen.jump_if_less_than(a, b);

        gen.set_const(val, 2);

        auto jump_else_end = gen.jump();

        auto label_is_eq = gen.jump_label();
        gen.bind_jump(jump_if_eq, label_is_eq);

        gen.set_const(val, 3);

        auto label_end = gen.jump_label();
        gen.bind_jump(jump_else_end, label_end);

        gen.return_value(val);

        auto fn = gen.compile<int_jt, int_jt, int_jt>();

        SUBCASE("less") {
            int_jt result = fn(1, 2);
            CHECK_EQ(3, result);
        }
        SUBCASE("not less") {
            int_jt result = fn(1, 1);
            CHECK_EQ(2, result);
        }

        assert_state(gen, 0, 3, 0, 2, 0)

        gen.free_fn(fn);
    }

    TEST_CASE("test jump if less or equal") {
        Asm gen = Asm();

        auto a = gen.arg(0);
        auto b = gen.arg(1);

        // the val we're using to track which branch path is taken
        auto val = gen.set_const(gen.reg(), 1);

        auto jump_if_eq = gen.jump_if_less_or_equal(a, b);

        gen.set_const(val, 2);

        auto jump_else_end = gen.jump();

        auto label_is_eq = gen.jump_label();
        gen.bind_jump(jump_if_eq, label_is_eq);

        gen.set_const(val, 3);

        auto label_end = gen.jump_label();
        gen.bind_jump(jump_else_end, label_end);

        gen.return_value(val);

        auto fn = gen.compile<int_jt, int_jt, int_jt>();

        SUBCASE("less") {
            int_jt result = fn(1, 2);
            CHECK_EQ(3, result);
        }
        SUBCASE("eq") {
            int_jt result = fn(2, 2);
            CHECK_EQ(3, result);
        }
        SUBCASE("greater") {
            int_jt result = fn(2, 1);
            CHECK_EQ(2, result);
        }

        assert_state(gen, 0, 3, 0, 2, 0)

        gen.free_fn(fn);
    }

    TEST_CASE("test jump if greater") {
        Asm gen = Asm();

        auto a = gen.arg(0);
        auto b = gen.arg(1);

        // the val we're using to track which branch path is taken
        auto val = gen.set_const(gen.reg(), 1);

        auto jump_if_eq = gen.jump_if_greater_than(a, b);

        gen.set_const(val, 2);

        auto jump_else_end = gen.jump();

        auto label_is_eq = gen.jump_label();
        gen.bind_jump(jump_if_eq, label_is_eq);

        gen.set_const(val, 3);

        auto label_end = gen.jump_label();
        gen.bind_jump(jump_else_end, label_end);

        gen.return_value(val);

        auto fn = gen.compile<int_jt, int_jt, int_jt>();

        SUBCASE("greater") {
            int_jt result = fn(2, 1);
            CHECK_EQ(3, result);
        }
        SUBCASE("not greater") {
            int_jt result = fn(1, 1);
            CHECK_EQ(2, result);
        }

        assert_state(gen, 0, 3, 0, 2, 0)

        gen.free_fn(fn);
    }

    TEST_CASE("test jump if greater or equal") {
        Asm gen = Asm();

        auto a = gen.arg(0);
        auto b = gen.arg(1);

        // the val we're using to track which branch path is taken
        auto val = gen.set_const(gen.reg(), 1);

        auto jump_if_eq = gen.jump_if_greater_or_equal(a, b);

        gen.set_const(val, 2);

        auto jump_else_end = gen.jump();

        auto label_is_eq = gen.jump_label();
        gen.bind_jump(jump_if_eq, label_is_eq);

        gen.set_const(val, 3);

        auto label_end = gen.jump_label();
        gen.bind_jump(jump_else_end, label_end);

        gen.return_value(val);

        auto fn = gen.compile<int_jt, int_jt, int_jt>();

        SUBCASE("greater") {
            int_jt result = fn(2, 1);
            CHECK_EQ(3, result);
        }
        SUBCASE("eq") {
            int_jt result = fn(2, 2);
            CHECK_EQ(3, result);
        }
        SUBCASE("less") {
            int_jt result = fn(1, 2);
            CHECK_EQ(2, result);
        }

        assert_state(gen, 0, 3, 0, 2, 0)

        gen.free_fn(fn);
    }

    TEST_CASE("test jump if true") {
        Asm gen = Asm();

        auto a = gen.arg(0);

        // the val we're using to track which branch path is taken
        auto val = gen.local_variable<int_jt>();
        gen.set_const(val, 1);

        auto jump_if_true = gen.jump_if_true(a);

        gen.set_const(val, 2);

        auto jump_else_end = gen.jump();

        auto label_is_true = gen.jump_label();
        gen.bind_jump(jump_if_true, label_is_true);

        gen.set_const(val, 3);

        auto label_end = gen.jump_label();
        gen.bind_jump(jump_else_end, label_end);

        gen.return_value(val);

        auto fn = gen.compile<int_jt, int_jt>();

        SUBCASE("true") {
            int_jt result = fn(true);
            CHECK_EQ(3, result);
        }
        SUBCASE("truthy") {
            int_jt result = fn(10);
            CHECK_EQ(3, result);
        }
        SUBCASE("not true") {
            int_jt result = fn(false);
            CHECK_EQ(2, result);
        }

        assert_state(gen, 8, 1, 0, 1, 0)

        gen.free_fn(fn);
    }

    TEST_CASE("test jump if false") {
        Asm gen = Asm();

        auto a = gen.arg(0);

        // the val we're using to track which branch path is taken
        auto val = gen.local_variable<int_jt>();
        gen.set_const(val, 1);

        auto jump_if_true = gen.jump_if_false(a);

        gen.set_const(val, 2);

        auto jump_else_end = gen.jump();

        auto label_is_true = gen.jump_label();
        gen.bind_jump(jump_if_true, label_is_true);

        gen.set_const(val, 3);

        auto label_end = gen.jump_label();
        gen.bind_jump(jump_else_end, label_end);

        gen.return_value(val);

        auto fn = gen.compile<int_jt, int_jt>();

        SUBCASE("true") {
            int_jt result = fn(true);
            CHECK_EQ(2, result);
        }
        SUBCASE("truthy") {
            int_jt result = fn(10);
            CHECK_EQ(2, result);
        }
        SUBCASE("not true") {
            int_jt result = fn(false);
            CHECK_EQ(3, result);
        }

        assert_state(gen, 8, 1, 0, 1, 0)

        gen.free_fn(fn);
    }

    TEST_CASE("test assign ptr") {
        Asm gen = Asm();

        int_jt to_be_assigned_int = 0;
        float_jt to_be_assigned_float = 0.0;
        gen.set_const(gen.absolute_address(&to_be_assigned_int), 3);
        gen.set_constf(gen.absolute_address(&to_be_assigned_float), 3.0);
        gen.return_void();

        auto fn = gen.compile<void>();
        fn();

        CHECK_EQ(3, to_be_assigned_int);
        CHECK_EQ(3.0, to_be_assigned_float);
        assert_state(gen, 0, 0, 1, 0, 0)

        gen.free_fn(fn);
    }

    Mem add_registers(Asm &gen, int arg1, int arg2) {
        auto a = gen.reg();
        auto b = gen.reg();
        gen.set_const(a, arg1);
        gen.set_const(b, arg2);
        return gen.add(a, b, gen.reg());
    }

    TEST_CASE("test many sum of registers") {
        Asm gen = Asm();

        int_jt out_1 = 0;
        int_jt out_2 = 0;
        int_jt out_3 = 0;

        Mem sum_1 = add_registers(gen, 10, 11);
        Mem sum_2 = add_registers(gen, 4, 5);
        Mem sum_3 = add_registers(gen, 6, 7);

        gen.move(sum_1, gen.absolute_address(&out_1));
        gen.move(sum_2, gen.absolute_address(&out_2));
        gen.move(sum_3, gen.absolute_address(&out_3));

        gen.return_void();

        auto fn = gen.compile<void>();
        fn();

        CHECK_EQ(10 + 11, out_1);
        CHECK_EQ(4 + 5, out_2);
        CHECK_EQ(6 + 7, out_3);

        // 7 for the 2 required arg registers in each sum operation
        assert_state(gen, 0, 7, 0, 0, 0)

        gen.free_fn(fn);
    }

    Mem add_temp_locals(Asm &gen, int arg1, int arg2) {
        auto a = gen.temp_local<int_jt>();
        auto b = gen.temp_local<int_jt>();
        gen.set_const(a, arg1);
        gen.set_const(b, arg2);
        auto ret = gen.temp_local<int_jt>();
        return gen.add(a, b, ret);
    }

    TEST_CASE("test sum of temp locals") {
        Asm gen = Asm();

        int_jt out_1 = 0;
        int_jt out_2 = 0;
        int_jt out_3 = 0;

        Mem sum_1 = add_temp_locals(gen, 1, 2);
        gen.move(sum_1, gen.absolute_address(&out_1));

        Mem sum_2 = add_temp_locals(gen, 10, 20);
        gen.move(sum_2, gen.absolute_address(&out_2));

        Mem sum_3 = add_temp_locals(gen, 100, 200);
        gen.move(sum_3, gen.absolute_address(&out_3));

        gen.return_void();

        auto fn = gen.compile<void>();
        fn();

        CHECK_EQ(1 + 2, out_1);
        CHECK_EQ(10 + 20, out_2);
        CHECK_EQ(100 + 200, out_3);

        // 16 for the 2 temp ints used in each function,
        // and 8 * 3 for the 3 return values kept alive by our sum_1 - 3
        assert_state(gen, 16 + 8 * 3, 0, 0, 0, 0)

        gen.free_fn(fn);
    }

    TEST_CASE("test sum of temp locals 2") {
        Asm gen = Asm();

        int_jt out_1 = 0;
        int_jt out_2 = 0;

        {
            auto a = gen.temp_local<int_jt>();
            auto b = gen.temp_local<int_jt>();
        }

        Mem sum_2 {};
        {
            auto a = gen.temp_local<int_jt>();
            auto b = gen.temp_local<int_jt>();
            gen.set_const(a, 10);
            gen.set_const(b, 20);
            auto ret = gen.temp_local<int_jt>();
            sum_2 = gen.add(a, b, ret);
        }
        gen.move(sum_2, gen.absolute_address(&out_2));

        gen.return_void();

        auto fn = gen.compile<void>();
        fn();

        CHECK_EQ(10 + 20, out_2);

        // 16 for the 2 temp ints used in each function,
        // and 8 * 3 for the 3 return values kept alive by our sum_1 - 3
        assert_state(gen, 24, 0, 0, 0, 0)

        gen.free_fn(fn);
    }

    TEST_CASE("test sum of temp locals 2") {
        Asm gen = Asm();

        int_jt out_1 = 0;
        int_jt out_2 = 0;

        {
            auto a = gen.local_variable<int_jt>();
            auto b = gen.temp_local<int_jt>();
        }

        Mem sum_2 {};
        {
            auto a = gen.temp_local<int_jt>();
            auto b = gen.temp_local<int_jt>();
            gen.set_const(a, 10);
            gen.set_const(b, 20);
            auto ret = gen.temp_local<int_jt>();
            sum_2 = gen.add(a, b, ret);
        }
        gen.move(sum_2, gen.absolute_address(&out_2));

        gen.return_void();

        auto fn = gen.compile<void>();
        fn();

        CHECK_EQ(10 + 20, out_2);

        // 16 for the 2 temp ints used in each function,
        // and 8 * 3 for the 3 return values kept alive by our sum_1 - 3
        assert_state(gen, 24, 0, 0, 0, 0)

        gen.free_fn(fn);
    }

    TEST_CASE("test sum of temp locals 2") {
        Asm gen = Asm();

        auto print_fn = []() {
            std::cout << "Hello, world!" << std::endl;
        };

        // TODO: figure out how to store / restore scratch registers before and after function call
        // we have to load them to the stack, but we'll have to figure out how to minimise stack usage by loading / restoring only the in-use registers
        // use float_registers_in_use & general_registers_in_use
        Mem val = gen.set_const(gen.temp_local<int_jt>(), 123);

        gen.call_static(print_fn);

        gen.return_value(val);

        auto fn = gen.compile<int_jt>();
        int_jt result = fn();

        CHECK_EQ(123, result);

        gen.free_fn(fn);
    }

//    TEST_CASE("test check assert") {
//        jassert::check()
//    }

//    TEST_CASE("Bench Codegen") {
//            auto start = std::chrono::system_clock::now();
//
//            const int iters = 10000;
//            for (int i = 0; i < iters; ++i) {
//                Asm gen = Asm();
//
//                int_jt out_1 = 0;
//                int_jt out_2 = 0;
//                int_jt out_3 = 0;
//
//                Mem sum_1 = add_temp_locals(gen, 1, 2);
//                gen.move(sum_1, gen.absolute_address(&out_1));
//
//                Mem sum_2 = add_temp_locals(gen, 10, 20);
//                gen.move(sum_2, gen.absolute_address(&out_2));
//
//                Mem sum_3 = add_temp_locals(gen, 100, 200);
//                gen.move(sum_3, gen.absolute_address(&out_3));
//
//                gen.return_void();
//
//                auto fn = gen.compile<void>();
//                gen.free_fn(fn);
//            }
//
//            auto end = std::chrono::system_clock::now();
//
//            auto dur = (end - start).count();
//            auto avg =  dur / ((double)iters * 1000);
//            std::cout << avg << "us";
//
//    }

}