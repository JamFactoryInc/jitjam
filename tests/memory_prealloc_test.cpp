//
// Created by jam on 20/09/2024.
//

#include "memory_prealloc_test.h"
#include "../memory_prealloc.h"
#include <chrono>

TEST_SUITE("Mempack Tests") {
    TEST_CASE("Sanity check") {
        auto prealloc = MemPacker();

        auto id = prealloc.reserve(8);
        prealloc.free(id.id);

        auto req = prealloc.pack();

        assert_equals(8, req.total_bytes);
        assert_equals(1, req.offsets.size());
        assert_equals(0, req.offsets[id.id]);
    }

    TEST_CASE("2 non-overlapping reservations") {
        auto prealloc = MemPacker();

        auto _1 = prealloc.reserve(8);
        prealloc.free(_1.id);
        auto _2 = prealloc.reserve(8);
        prealloc.free(_2.id);

        auto req = prealloc.pack();

        assert_equals(8, req.total_bytes);
        assert_equals(2, req.offsets.size());
        assert_equals(0, req.offsets[_1.id]);
        assert_equals(0, req.offsets[_2.id]);
    }

    TEST_CASE("2 overlapping reservations") {
        auto prealloc = MemPacker();

        auto _1 = prealloc.reserve(8);
        auto _2 = prealloc.reserve(8);
        prealloc.free(_1.id);
        prealloc.free(_2.id);

        auto req = prealloc.pack();

        assert_equals(16, req.total_bytes);
        assert_equals(2, req.offsets.size());
        assert_equals(0, req.offsets[_1.id]);
        assert_equals(8, req.offsets[_2.id]);
    }

    TEST_CASE("3 overlapping reservations") {
        auto prealloc = MemPacker();

        auto _1 = prealloc.reserve(8);
        auto _2 = prealloc.reserve(8);
        auto _3 = prealloc.reserve(8);
        prealloc.free(_1.id);
        prealloc.free(_2.id);
        prealloc.free(_3.id);

        auto req = prealloc.pack();

        assert_equals(24, req.total_bytes);
        assert_equals(3, req.offsets.size());
        assert_equals(0, req.offsets[_1.id]);
        assert_equals(8, req.offsets[_2.id]);
        assert_equals(16, req.offsets[_3.id]);
    }

    TEST_CASE("2 non-overlapping reservations - res 2 is wider") {
        auto prealloc = MemPacker();

        auto _1 = prealloc.reserve(8);
        prealloc.free(_1.id);
        auto _2 = prealloc.reserve(16);
        prealloc.free(_2.id);

        auto req = prealloc.pack();

        assert_equals(16, req.total_bytes);
        assert_equals(2, req.offsets.size());
        assert_equals(0, req.offsets[_1.id]);
        assert_equals(0, req.offsets[_2.id]);
    }

    TEST_CASE("2 non-overlapping reservations - res 1 is wider") {
        auto prealloc = MemPacker();

        auto _1 = prealloc.reserve(16);
        prealloc.free(_1.id);
        auto _2 = prealloc.reserve(8);
        prealloc.free(_2.id);

        auto req = prealloc.pack();

        assert_equals(16, req.total_bytes);
        assert_equals(2, req.offsets.size());
        assert_equals(0, req.offsets[_1.id]);
        assert_equals(0, req.offsets[_2.id]);
    }

    TEST_CASE("2 non-overlapping reservations - res 1 is wider") {
        auto prealloc = MemPacker();

        auto _1 = prealloc.reserve(16);
        prealloc.free(_1.id);
        auto _2 = prealloc.reserve(8);
        prealloc.free(_2.id);

        auto req = prealloc.pack();

        assert_equals(16, req.total_bytes);
        assert_equals(2, req.offsets.size());
        assert_equals(0, req.offsets[_1.id]);
        assert_equals(0, req.offsets[_2.id]);
    }

    TEST_CASE("temporally distinct values of increasing size should be in the same channel") {
        auto prealloc = MemPacker();

        auto _1 = prealloc.reserve(8);
        prealloc.free(_1.id);
        auto _2 = prealloc.reserve(16);
        prealloc.free(_2.id);
        auto _3 = prealloc.reserve(24);
        prealloc.free(_3.id);
        auto _4 = prealloc.reserve(32);
        prealloc.free(_4.id);


        auto req = prealloc.pack();

        assert_equals(32, req.total_bytes);
        assert_equals(4, req.offsets.size());
        assert_equals(0, req.offsets[_1.id]);
        assert_equals(0, req.offsets[_2.id]);
        assert_equals(0, req.offsets[_3.id]);
        assert_equals(0, req.offsets[_4.id]);
    }

    TEST_CASE("overlapping values are packed into a vacant wider channel") {
        auto prealloc = MemPacker();

        auto _1 = prealloc.reserve(8);
        auto _2 = prealloc.reserve(8);
        prealloc.free(_1.id);
        prealloc.free(_2.id);
        auto _3 = prealloc.reserve(16);
        prealloc.free(_3.id);

        auto req = prealloc.pack();

        assert_equals(16, req.total_bytes);
        assert_equals(3, req.offsets.size());
        assert_equals(0, req.offsets[_1.id]);
        assert_equals(8, req.offsets[_2.id]);
        assert_equals(0, req.offsets[_3.id]);
    }

    TEST_CASE("Non-overlapping") {
        auto packer = MemPacker();

        auto _1 = packer.reserve(8);
        packer.free(_1.id);
        auto _2 = packer.reserve(8);
        packer.free(_2.id);

        auto req = packer.pack();

        CHECK_EQ(req.total_bytes, 8);
        CHECK_EQ(req.offsets[_1.id], 0);
        CHECK_EQ(req.offsets[_2.id], 0);
    }

    TEST_CASE("Non-overlapping many") {
        auto packer = MemPacker();

        auto _1 = packer.reserve(8);
        packer.free(_1.id);
        auto _2 = packer.reserve(8);
        packer.free(_2.id);
        auto _3 = packer.reserve(8);
        packer.free(_3.id);
        auto _4 = packer.reserve(8);
        packer.free(_4.id);
        auto _5 = packer.reserve(8);
        packer.free(_5.id);
        auto _6 = packer.reserve(8);
        packer.free(_6.id);
        auto _7 = packer.reserve(8);
        packer.free(_7.id);
        auto _8 = packer.reserve(8);
        packer.free(_8.id);

        auto req = packer.pack();

        CHECK_EQ(req.total_bytes, 8);
        CHECK_EQ(req.offsets[_1.id], 0);
        CHECK_EQ(req.offsets[_2.id], 0);
        CHECK_EQ(req.offsets[_3.id], 0);
        CHECK_EQ(req.offsets[_4.id], 0);
        CHECK_EQ(req.offsets[_5.id], 0);
        CHECK_EQ(req.offsets[_6.id], 0);
        CHECK_EQ(req.offsets[_7.id], 0);
        CHECK_EQ(req.offsets[_8.id], 0);
    }

    TEST_CASE("Overlapping") {
        auto packer = MemPacker();

        auto _1 = packer.reserve(8);
        auto _2 = packer.reserve(8);
        packer.free(_1.id);
        packer.free(_2.id);

        auto req = packer.pack();

        CHECK_EQ(req.total_bytes, 16);
        CHECK_EQ(req.offsets[_1.id], 0);
        CHECK_EQ(req.offsets[_2.id], 8);
    }

    TEST_CASE("Pack overlapping spans into free larger lane") {
        auto packer = MemPacker();

        auto _1 = packer.reserve(8);
        auto _2 = packer.reserve(8);
        packer.free(_1.id);
        packer.free(_2.id);
        auto _3 = packer.reserve(16);
        packer.free(_3.id);

        auto req = packer.pack();

        CHECK_EQ(req.total_bytes, 16);
        CHECK_EQ(req.offsets[_1.id], 0);
        CHECK_EQ(req.offsets[_2.id], 8);
        CHECK_EQ(req.offsets[_3.id], 0);
    }

    TEST_CASE("Pack overlapping spans into free larger lane 2") {
        auto packer = MemPacker();

        auto _1 = packer.reserve(16);
        packer.free(_1.id);

        auto _2 = packer.reserve(8);
        auto _3 = packer.reserve(8);
        packer.free(_2.id);
        packer.free(_3.id);

        auto req = packer.pack();

        CHECK_EQ(req.total_bytes, 16);
        CHECK_EQ(req.offsets[_1.id], 0);
        CHECK_EQ(req.offsets[_2.id], 0);
        CHECK_EQ(req.offsets[_3.id], 8);
    }

    TEST_CASE("Long-lived large lane") {
        auto packer = MemPacker();

        auto _1 = packer.reserve(16);

        auto _2 = packer.reserve(8);
        auto _3 = packer.reserve(8);
        packer.free(_2.id);
        packer.free(_3.id);

        packer.free(_1.id);

        auto req = packer.pack();

        CHECK_EQ(req.total_bytes, 32);
        CHECK_EQ(req.offsets[_1.id], 0);
        CHECK_EQ(req.offsets[_2.id], 16);
        CHECK_EQ(req.offsets[_3.id], 24);
    }

    TEST_CASE("Lots of small reservations") {
        auto packer = MemPacker();

        auto _1 = packer.reserve(8);
        auto _2 = packer.reserve(8);
        auto _3 = packer.reserve(8);
        auto _4 = packer.reserve(8);
        auto _5 = packer.reserve(8);
        auto _6 = packer.reserve(8);
        packer.free(_1.id);
        packer.free(_2.id);
        packer.free(_3.id);
        packer.free(_4.id);
        packer.free(_5.id);
        packer.free(_6.id);

        auto req = packer.pack();

        CHECK_EQ(req.total_bytes, 6 * 8);
        CHECK_EQ(req.offsets[_1.id], 8 * 0);
        CHECK_EQ(req.offsets[_2.id], 8 * 1);
        CHECK_EQ(req.offsets[_3.id], 8 * 2);
        CHECK_EQ(req.offsets[_4.id], 8 * 3);
        CHECK_EQ(req.offsets[_5.id], 8 * 4);
        CHECK_EQ(req.offsets[_6.id], 8 * 5);
    }

    TEST_CASE("Mix of big and small") {
        auto packer = MemPacker();

        auto _64 = packer.reserve(64);

        auto _16_1 = packer.reserve(16);

        packer.free(_64.id);

        auto _8_1 = packer.reserve(8);
        auto _16_2 = packer.reserve(16);
        auto _8_2 = packer.reserve(8);

        packer.free(_16_1.id);
        packer.free(_16_2.id);
        packer.free(_8_1.id);
        packer.free(_8_2.id);

        auto req = packer.pack();

        CHECK_EQ(req.total_bytes, 64 + 16);
        CHECK_EQ(req.offsets[_64.id], 0);
        CHECK_EQ(req.offsets[_16_1.id], 64);
        CHECK_EQ(req.offsets[_8_1.id], 16);
        CHECK_EQ(req.offsets[_16_2.id], 0);
        CHECK_EQ(req.offsets[_8_2.id], 24);
    }

//    TEST_CASE("Bench") {
//        auto start = std::chrono::system_clock::now();
//
//        const int iters = 10000;
//        for (int i = 0; i < iters; ++i) {
//            auto packer = MemPacker();
//
//            auto _256 = packer.reserve(256);
//
//            auto _16_1 = packer.reserve(16);
//
//            packer.free(_256.id);
//
//            auto _8_1 = packer.reserve(8);
//            auto _16_2 = packer.reserve(16);
//            auto _8_2 = packer.reserve(8);
//
//            packer.free(_16_1.id);
//            packer.free(_16_2.id);
//            packer.free(_8_1.id);
//            packer.free(_8_2.id);
//
//            auto req = packer.pack();
//        }
//
//        auto end = std::chrono::system_clock::now();
//
//        auto dur = (end - start).count();
//        auto avg =  dur / ((double)iters * 1000);
//        std::cout << avg << "us";
//    }
}
