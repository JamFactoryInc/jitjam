
//

#ifndef JITJAM_VARIABLE_BOUNDS_H
#define JITJAM_VARIABLE_BOUNDS_H

#include "runtime_values.h"
#include "../../../checked_ops.h"

using namespace jt;

enum class ResultFlags: uint {
    None =              0b00000001,
    // this bound check provided extra narrowing info to the lhs arg and mutated it. Dependents should be re-processed
    LhsNarrowed =       0b00000001,
    // this bound check provided extra narrowing info to the lhs arg and mutated it. Dependents should be re-processed
    RhsNarrowed =       0b00000010,
    // the result of the operation is always equal to the lhs
    ResultIsLhs =       0b00000100,
    // the result of the operation is always equal to the rhs
    ResultIsRhs =       0b00001000,
    // the result is a constant value
    ResultIsConst =     0b00010000,
    // the denominator is always zero
    ConstantDivZero =   0b00100000,
    OpError =           0b01000000,
};

ResultFlags operator |(const ResultFlags &a, const ResultFlags &b) {
    return static_cast<ResultFlags>(static_cast<uint>(a) | static_cast<uint>(b));
}
ResultFlags operator &(const ResultFlags &a, const ResultFlags &b) {
    return static_cast<ResultFlags>(static_cast<uint>(a) & static_cast<uint>(b));
}

enum class ValueFlags: uint {
    Nil =          0b00000000,
    NonZero =       0b00000001,
    PowerOfTwo =    0b00000010,
    Even =          0b00000100,
    Odd =           0b00001000,
    // for floats, whether the value is representable as an exact integral
    Integral =      0b00010000,

    NonZeroFilter =     ~NonZero,
    PowerOfTwoFilter =  ~PowerOfTwo,
    EvenFilter =        ~Even,
    OddFilter =         ~Odd,
};
ValueFlags operator |(const ValueFlags &a, const ValueFlags &b) {
    return static_cast<ValueFlags>(static_cast<uint>(a) | static_cast<uint>(b));
}
void operator |=(ValueFlags &a, const ValueFlags &b) {
    a = static_cast<ValueFlags>(static_cast<uint>(a) | static_cast<uint>(b));
}
ValueFlags operator &(const ValueFlags &a, const ValueFlags &b) {
    return static_cast<ValueFlags>(static_cast<uint>(a) & static_cast<uint>(b));
}
void operator &=(ValueFlags &a, const ValueFlags &b) {
    a = static_cast<ValueFlags>(static_cast<uint>(a) & static_cast<uint>(b));
}

class IntBounds {
    friend class VariableBounds;

    uintptr_t unique_value_id = 0;
    ValueFlags value_flags = ValueFlags::Nil;
    int_jt min;
    int_jt max;

    void set_nonzero() {
        value_flags |= ValueFlags::NonZero;
    }

    void set_even() {
        value_flags |= ValueFlags::Even;
        value_flags &= ValueFlags::OddFilter;
    }

    void set_odd() {
        value_flags |= ValueFlags::Odd;
        value_flags |= ValueFlags::NonZero;
        value_flags &= ValueFlags::EvenFilter;
        value_flags &= ValueFlags::PowerOfTwoFilter;
    }

    void set_power_of_two() {
        set_even();
        value_flags |= ValueFlags::PowerOfTwo;
    }

    bool has_flag(const ValueFlags &flag) const {
        return (value_flags & flag) != ValueFlags::Nil;
    }

    // return a number representing whether this value is even, odd, or indeterminate
    int_jt get_evenness() const {
        return static_cast<int_jt>(value_flags & (ValueFlags::Even | ValueFlags::Odd));
    }
    
    bool same_evenness_as(const IntBounds &other) const {
        return get_evenness() == other.get_evenness();
    }

    bool different_evenness_from(const IntBounds &other) const {
        return (get_evenness() & other.get_evenness()) != 0;
    }

    bool is_even() const {
        return has_flag(ValueFlags::Even);
    }

    bool is_odd() const {
        return has_flag(ValueFlags::Odd);
    }

    bool is_nonzero() const {
        return min > 0 || max < 0 || has_flag(ValueFlags::NonZero);
    }

    bool is_power_of_two() const {
        return has_flag(ValueFlags::PowerOfTwo);
    }
    
    bool is_constant() const {
        return min == max;
    }

    bool is_same_as(const IntBounds& other) const {
        return unique_value_id == other.unique_value_id && unique_value_id != 0;
    }
    
    void set_unbounded() {
        this->min = std::numeric_limits<int_jt>::min();
        this->max = std::numeric_limits<int_jt>::max();
        this->value_flags = ValueFlags::Nil;
    }

public:
    IntBounds(int_jt min, int_jt max): min(min), max(max) { }

    int_jt get_min() const {
        return min;
    }

    int_jt get_max() const {
        return max;
    }

    static void add(IntBounds &result, ResultFlags &flags, const IntBounds &lhs, const IntBounds &rhs) {
        if (lhs.same_evenness_as(rhs)) {
            result.set_even();
        } else if (lhs.different_evenness_from(rhs)) {
            result.set_odd();
        }

        if (lhs.is_same_as(rhs) && lhs.is_power_of_two()) {
            result.set_power_of_two();
        }

        if (lhs.is_constant() && rhs.is_constant()) {
            result.min = lhs.max + rhs.max;
            result.max = result.min;
            flags = ResultFlags::ResultIsConst;
            return;
        }

        if (lhs.is_constant() && lhs.max == 0) {
            result = rhs;
            flags = ResultFlags::ResultIsRhs;
            return;
        }

        if (rhs.is_constant() && rhs.max == 0) {
            result = lhs;
            flags = ResultFlags::ResultIsLhs;
            return;
        }

        bool overflows = false;
        int_jt lhs_min_rhs_min = safe_ops::checked_add(lhs.min, rhs.min, overflows);
        int_jt lhs_min_rhs_max = safe_ops::checked_add(lhs.min, rhs.max, overflows);
        int_jt lhs_max_rhs_min = safe_ops::checked_add(lhs.max, rhs.min, overflows);
        int_jt lhs_max_rhs_max = safe_ops::checked_add(lhs.max, rhs.max, overflows);

        if (overflows) {
            result.set_unbounded();
            return;
        }

        if (lhs.is_same_as(rhs)) {
            auto min_max = std::minmax({ lhs_min_rhs_min, lhs_max_rhs_max });
            result.min = min_max.first;
            result.max = min_max.second;
            return;
        }
        
        auto min_max = std::minmax({ lhs_min_rhs_min, lhs_min_rhs_max, lhs_max_rhs_min, lhs_max_rhs_max });
        result.min = min_max.first;
        result.max = min_max.second;
    }

    static void subtract(IntBounds &result, ResultFlags &flags, const IntBounds &lhs, const IntBounds &rhs) {
        if (lhs.is_constant() && rhs.is_constant()) {
            result.min = lhs.max - rhs.max;
            result.max = result.min;
            flags = ResultFlags::ResultIsConst;
            return;
        }

        if ((lhs.is_even() && rhs.is_even()) || (lhs.is_odd() && rhs.is_odd())) {
            result.set_even();
        } else if ((lhs.is_odd() && rhs.is_even()) || (lhs.is_even() && rhs.is_odd())) {
            result.set_odd();
        }
        
        if (lhs.is_same_as(rhs)) {
            result.min = 0;
            result.max = 0;
            flags = ResultFlags::ResultIsConst;
            return;
        }

        if (rhs.is_constant() && rhs.max == 0) {
            result = lhs;
            flags = ResultFlags::ResultIsLhs;
            return;
        }

        bool overflows = false;
        int_jt lhs_min_rhs_min = safe_ops::checked_sub(lhs.min, rhs.min, overflows);
        int_jt lhs_min_rhs_max = safe_ops::checked_sub(lhs.min, rhs.max, overflows);
        int_jt lhs_max_rhs_min = safe_ops::checked_sub(lhs.max, rhs.min, overflows);
        int_jt lhs_max_rhs_max = safe_ops::checked_sub(lhs.max, rhs.max, overflows);

        if (overflows) {
            result.set_unbounded();
            return;
        }

        auto min_max = std::minmax({ lhs_min_rhs_min, lhs_min_rhs_max, lhs_max_rhs_min, lhs_max_rhs_max });
        result.min = min_max.first;
        result.max = min_max.second;
    }

    static void multiply(IntBounds &result, ResultFlags &flags, const IntBounds &lhs, const IntBounds &rhs) {
        if (lhs.is_constant() && rhs.is_constant()) {
            result.min = lhs.max * rhs.max;
            result.max = result.min;
            flags = ResultFlags::ResultIsConst;
            return;
        }

        if (lhs.is_constant()) {
            if (lhs.max == 0) {
                flags = ResultFlags::ResultIsConst;
                result = lhs;
                return;
            }
            if (lhs.max == 1) {
                flags = ResultFlags::ResultIsRhs;
                result = rhs;
                return;
            }
        } else if (rhs.is_constant()) {
            if (rhs.max == 0) {
                flags = ResultFlags::ResultIsConst;
                result = rhs;
                return;
            }
            if (rhs.max == 1) {
                flags = ResultFlags::ResultIsLhs;
                result = lhs;
                return;
            }
        }

        if (lhs.is_even() || rhs.is_even()) {
            result.set_even();
        } else if (lhs.is_odd() && rhs.is_odd()) {
            result.set_odd();
        }

        if (lhs.is_nonzero() && rhs.is_nonzero()) {
            result.set_nonzero();
        }

        if (lhs.is_power_of_two() && rhs.is_power_of_two()) {
            result.set_power_of_two();
        }

        bool overflows = false;
        int_jt lhs_min_rhs_min = safe_ops::checked_multiply(lhs.min, rhs.min, overflows);
        int_jt lhs_min_rhs_max = safe_ops::checked_multiply(lhs.min, rhs.max, overflows);
        int_jt lhs_max_rhs_min = safe_ops::checked_multiply(lhs.max, rhs.min, overflows);
        int_jt lhs_max_rhs_max = safe_ops::checked_multiply(lhs.max, rhs.max, overflows);
        
        if (overflows) {
            result.set_unbounded();
            return;
        }

        if (lhs.is_same_as(rhs)) {
            auto min_max = std::minmax({ lhs_min_rhs_min, lhs_max_rhs_max });
            result.min = min_max.first;
            result.max = min_max.second;
            return;
        }
        
        auto min_max = std::minmax({ lhs_min_rhs_min, lhs_min_rhs_max, lhs_max_rhs_min, lhs_max_rhs_max });
        result.min = min_max.first;
        result.max = min_max.second;
    }

    static void divide(IntBounds &result, ResultFlags &flags, const IntBounds &lhs, IntBounds &rhs) {
        if (rhs.is_constant()) {
            if (rhs.max == 0) {
                flags = ResultFlags::ConstantDivZero;
                return;
            } else if (rhs.max == 1) {
                result = lhs;
                flags = ResultFlags::ResultIsLhs;
                return;
            } else if (lhs.is_constant()) {
                result.min = lhs.max / rhs.max;
                result.max = result.min;
                flags = ResultFlags::ResultIsConst;
                return;
            }
        } else {
            // adjust the denominator to be non-zero
            // we get some free value narrowing by assuming that it cannot be zero here
            if (rhs.min == 0) {
                ++rhs.min;
            }
            if (rhs.max == 0) {
                --rhs.max;
            }
        }

        if (lhs.is_nonzero()) {
            result.set_nonzero();
        }

        if (lhs.is_same_as(rhs)) {
            result.min = 1;
            result.max = 1;
            flags = ResultFlags::ResultIsConst;
            return;
        }

        int_jt lhs_min_rhs_min = lhs.min / rhs.min;
        int_jt lhs_max_rhs_min = lhs.max / rhs.min;
        int_jt lhs_min_rhs_max = lhs.min / rhs.max;
        int_jt lhs_max_rhs_max = lhs.max / rhs.max;
        
        auto min_max = std::minmax({ lhs_min_rhs_min, lhs_min_rhs_max, lhs_max_rhs_min, lhs_max_rhs_max });
        result.min = min_max.first;
        result.max = min_max.second;
    }

    static void mod(IntBounds &result, ResultFlags &flags, const IntBounds &lhs, IntBounds &rhs) {
        if (rhs.is_constant()) {
            if (rhs.max == 0) {
                flags = ResultFlags::ConstantDivZero;
                return;
            } else if (rhs.max == 1) {
                result.min = 0;
                result.max = 0;
                flags = ResultFlags::ResultIsConst;
                return;
            } else if (lhs.is_constant()) {
                result.min = lhs.max % rhs.max;
                result.max = result.min;
                flags = ResultFlags::ResultIsConst;
                return;
            }
        } else {
            // adjust the denominator to be non-zero
            // we get some free value narrowing by assuming that it cannot be zero here
            if (rhs.min == 0) {
                ++rhs.min;
            }
            if (rhs.max == 0) {
                --rhs.max;
            }
        }
        
        if (lhs.is_same_as(rhs)) {
             result.min = 0;
             result.max = 0;
             flags = ResultFlags::ResultIsConst;
             return;
        }

        // TODO fix potential issues with absolute value overflow

        bool overflows = false;
        auto smallest_rhs_magnitude = rhs.min < 0 && rhs.max > 0
            ? 1
            : std::min(
                safe_ops::checked_abs(rhs.min, overflows),
                safe_ops::checked_abs(rhs.max, overflows)
            );
        auto largest_rhs_magnitude = std::max(
                safe_ops::checked_abs(rhs.min, overflows),
                safe_ops::checked_abs(rhs.max, overflows)
            );
        auto largest_result_magnitude = largest_rhs_magnitude - 1;

        if (overflows) {
            result.set_unbounded();
            return;
        }

        if (lhs.is_constant()) {
            if (abs(lhs.max) < smallest_rhs_magnitude) {
                result = lhs;
                flags = ResultFlags::ResultIsConst;
                return;
            }
        }

        // x == x % y where 0 <= x < |y|
        if (lhs.min >= 0 && lhs.max <= largest_result_magnitude) {
            result = lhs;
            flags = ResultFlags::ResultIsLhs;
            return;
        }
        // x == x % y where x <= 0 && |x| < |y|
        if (abs(lhs.min) < smallest_rhs_magnitude && lhs.max <= 0) {
            result = lhs;
            flags = ResultFlags::ResultIsLhs;
            return;
        }

        if (lhs.min < 0) {
            if (lhs.max <= 0) {
                // -, -
                result.min = -largest_result_magnitude;
                result.max = 0;
            } else {
                // lhs.max > 0

                // -, +
                result.min = (abs(lhs.min) < largest_result_magnitude)
                             ? lhs.min
                             : -largest_result_magnitude;

                result.max = (lhs.max < largest_result_magnitude)
                             ? lhs.max
                             : largest_result_magnitude;
            }
        } else {
            // lhs.min >= 0 therefore lhs.max >= 0

            // +, +
            result.min = 0;
            result.max = largest_result_magnitude;
        }

    }

    static void greater_than(IntBounds &result, ResultFlags &flags, IntBounds &lhs, IntBounds &rhs) {
        if (lhs.is_same_as(rhs)) {
            result.max = 0;
            result.min = 0;
            flags = ResultFlags::ResultIsConst;
            return;
        }
        
        if (lhs.min > rhs.max) {
            result.max = 1;
            result.min = 1;
            flags = ResultFlags::ResultIsConst;
            return;
        } else if (lhs.max <= rhs.min) {
            result.max = 0;
            result.min = 0;
            flags = ResultFlags::ResultIsConst;
            return;
        }
    }

    static void greater_than_equal(IntBounds &result, ResultFlags &flags, IntBounds &lhs, IntBounds &rhs) {
        if (lhs.is_same_as(rhs)) {
            result.max = 1;
            result.min = 1;
            flags = ResultFlags::ResultIsConst;
            return;
        }
        
        if (lhs.min >= rhs.max) {
            result.max = 1;
            result.min = 1;
            flags = ResultFlags::ResultIsConst;
        } else if (lhs.max < rhs.min) {
            result.max = 0;
            result.min = 0;
            flags = ResultFlags::ResultIsConst;
        }
    }

    static void less_than(IntBounds &result, ResultFlags &flags, IntBounds &lhs, IntBounds &rhs) {
        if (lhs.is_same_as(rhs)) {
            result.max = 0;
            result.min = 0;
            flags = ResultFlags::ResultIsConst;
            return;
        }
        
        if (lhs.max < rhs.min) {
            result.max = 1;
            result.min = 1;
            flags = ResultFlags::ResultIsConst;
        } else if (lhs.min >= rhs.max) {
            result.max = 0;
            result.min = 0;
            flags = ResultFlags::ResultIsConst;
        }
    }

    static void less_than_equal(IntBounds &result, ResultFlags &flags, IntBounds &lhs, IntBounds &rhs) {
        if (lhs.is_same_as(rhs)) {
            result.max = 1;
            result.min = 1;
            flags = ResultFlags::ResultIsConst;
            return;
        }
        
        if (lhs.max <= rhs.min) {
            result.max = 1;
            result.min = 1;
            flags = ResultFlags::ResultIsConst;
        } else if (lhs.min > rhs.max) {
            result.max = 0;
            result.min = 0;
            flags = ResultFlags::ResultIsConst;
        }
    }

    static void equal(IntBounds &result, ResultFlags &flags, IntBounds &lhs, IntBounds &rhs) {
        if (lhs.is_same_as(rhs)) {
            result.max = 1;
            result.min = 1;
            flags = ResultFlags::ResultIsConst;
            return;
        }
        
    }

    static void not_equal(IntBounds &result, ResultFlags &flags, IntBounds &lhs, IntBounds &rhs) {

    }

    static void bit_or(IntBounds &result, ResultFlags &flags, IntBounds &lhs, IntBounds &rhs) {

    }

    static void bit_and(IntBounds &result, ResultFlags &flags, IntBounds &lhs, IntBounds &rhs) {

    }

    static void bit_xor(IntBounds &result, ResultFlags &flags, IntBounds &lhs, IntBounds &rhs) {

    }

    static void bit_shift_left(IntBounds &result, ResultFlags &flags, IntBounds &lhs, IntBounds &rhs) {

    }

    static void bit_shift_right(IntBounds &result, ResultFlags &flags, IntBounds &lhs, IntBounds &rhs) {

    }
};


class FloatBounds {
    friend class VariableBounds;

    uintptr_t unique_value_id = 0;
    ValueFlags value_flags = ValueFlags::Nil;
    float_jt min;
    float_jt max;

    static constexpr int mantissa = std::numeric_limits<float_jt>::digits;
    static constexpr int_jt integral_limit = 1l << mantissa;
    static constexpr auto float_integral_max = (float_jt)integral_limit;
    static constexpr auto float_integral_min = -float_integral_max;
    static constexpr auto NaN = std::numeric_limits<float_jt>::signaling_NaN();

    void set_unbounded() {
        this->value_flags = ValueFlags::Nil;
        this->min = NaN;
        this->max = NaN;
    }

    static bool check_nan(const FloatBounds &lhs, const FloatBounds &rhs, FloatBounds &result) {
        if (isnan(lhs.min) || isnan(rhs.min) || isnan(lhs.max) || isnan(rhs.max)) {
            result.set_unbounded();
            return true;
        }
        return false;
    }

    bool has_flag(const ValueFlags &flag) const {
        return (value_flags & flag) != ValueFlags::Nil;
    }

    bool is_integral() const {
        return this->has_flag(ValueFlags::Integral);
    }

    bool is_nonzero() const {
        return this->has_flag(ValueFlags::NonZero) || min > 0.0 || max < 0.0;
    }

    bool is_finite() const {
        return isfinite(min) && isfinite(max);
    }

    bool is_infinite() const {
        return isinf(min) || isinf(max);
    }

    bool is_const() const {
        return min == max && isfinite(min);
    }

    void set_integral() {
        value_flags |= ValueFlags::Integral;
    }

    static float_jt abs_error(float_jt value) {
        auto abs_value = abs(value);
        return nextafter(abs_value, INFINITY) - abs_value;
    }

    static float_jt round_up_error(float_jt value) {
        if (value == -std::numeric_limits<float_jt>::min()) {
            return 0.0;
        }
        if (value < std::numeric_limits<float_jt>::max()) {
            return nextafter(value, INFINITY);
        }
        return -std::numeric_limits<float_jt>::infinity();
    }

    static float_jt round_down_error(float_jt value) {
        if (value == std::numeric_limits<float_jt>::min()) {
            return 0.0;
        }
        if (value > -std::numeric_limits<float_jt>::max()) {
            return nextafter(value, -INFINITY);
        }
        return -std::numeric_limits<float_jt>::infinity();
    }

    static bool in_int_bounds(const float_jt &value) {
        return value > float_integral_min && value < float_integral_max;
    }

    static bool is_integral_result(const FloatBounds &lhs, const FloatBounds &rhs, const FloatBounds &result) {
        return lhs.is_integral() && rhs.is_integral()
               && in_int_bounds(result.min) && in_int_bounds(result.max);
    }

public:
    FloatBounds(float_jt min, float_jt max): min(min), max(max) { }

    float_jt get_min() const {
        return min;
    }

    float_jt get_max() const {
        return max;
    }

    static void add(FloatBounds &result, ResultFlags &flags, const FloatBounds &lhs, const FloatBounds &rhs) {
        if (check_nan(lhs, rhs, result)) return;

        if (lhs.is_const() && rhs.is_const()) {
            flags = ResultFlags::ResultIsConst;
            result.min = lhs.min + rhs.min;
            result.max = lhs.max + rhs.max;
            if (is_integral_result(lhs, rhs, result)) {
                result.set_integral();
            }
            return;
        }

        if (lhs.is_const() && lhs.max == 0.0) {
            flags = ResultFlags::ResultIsRhs;
            result = rhs;
            return;
        }

        if (rhs.is_const() && rhs.max == 0.0) {
            flags = ResultFlags::ResultIsLhs;
            result = lhs;
            return;
        }

        float_jt lhs_min_rhs_min = lhs.min + rhs.min;
        float_jt lhs_min_rhs_max = lhs.min + rhs.max;
        float_jt lhs_max_rhs_min = lhs.max + rhs.min;
        float_jt lhs_max_rhs_max = lhs.max + rhs.max;

        auto min_max = std::minmax({ lhs_min_rhs_min, lhs_min_rhs_max, lhs_max_rhs_min, lhs_max_rhs_max });
        result.min = min_max.first;
        result.max = min_max.second;

        if (is_integral_result(lhs, rhs, result)) {
            result.set_integral();
        } else {
            result.min = round_down_error(result.min);
            result.max = round_up_error(result.max);
        }
    }

    static void subtract(FloatBounds &result, ResultFlags &flags, const FloatBounds &lhs, const FloatBounds &rhs) {
        if (check_nan(lhs, rhs, result)) return;

        if (lhs.is_const() && rhs.is_const()) {
            flags = ResultFlags::ResultIsConst;
            result.min = lhs.min - rhs.min;
            result.max = lhs.max - rhs.max;
            if (is_integral_result(lhs, rhs, result)) {
                result.set_integral();
            }
            return;
        }

        if (rhs.is_const() && rhs.max == 0.0) {
            flags = ResultFlags::ResultIsLhs;
            result = lhs;
            return;
        }

        float_jt lhs_min_rhs_min = lhs.min - rhs.min;
        float_jt lhs_min_rhs_max = lhs.min - rhs.max;
        float_jt lhs_max_rhs_min = lhs.max - rhs.min;
        float_jt lhs_max_rhs_max = lhs.max - rhs.max;

        auto min_max = std::minmax({ lhs_min_rhs_min, lhs_min_rhs_max, lhs_max_rhs_min, lhs_max_rhs_max });
        result.min = min_max.first;
        result.max = min_max.second;

        if (is_integral_result(lhs, rhs, result)) {
            result.set_integral();
        } else {
            result.min = round_down_error(result.min);
            result.max = round_up_error(result.max);
        }
    }

    static void multiply(FloatBounds &result, ResultFlags &flags, const FloatBounds &lhs, const FloatBounds &rhs) {
        if (check_nan(lhs, rhs, result)) return;

        if (lhs.is_const() && rhs.is_const()) {
            result.max = lhs.max * rhs.max;
            result.min = result.max;

            if (isnan(result.max)) {
                result.set_unbounded();
                return;
            }

            flags = ResultFlags::ResultIsConst;
            if (is_integral_result(lhs, rhs, result)) {
                result.set_integral();
            }
            return;
        }

        if (rhs.is_const()) {
            if (rhs.max == 0.0) {
                if (lhs.is_finite()) {
                    flags = ResultFlags::ResultIsConst;
                    result.min = 0.0;
                    result.max = 0.0;
                } else {
                    result.set_unbounded();
                }
                return;
            }

            if (rhs.max == 1.0) {
                flags = ResultFlags::ResultIsLhs;
                result = lhs;
                return;
            }

            if (abs(rhs.max) == std::numeric_limits<float_jt>::infinity()) {
                if (lhs.max < 0.0) {
                    flags = ResultFlags::ResultIsConst;
                    result.min = -rhs.max;
                    return;
                }
                if (lhs.min > 0.0) {
                    flags = ResultFlags::ResultIsConst;
                    result.min = rhs.max;
                    return;
                }
                if (lhs.is_nonzero()) {
                    result.min = -std::numeric_limits<float_jt>::infinity();
                    result.max = std::numeric_limits<float_jt>::infinity();
                    return;
                }
            }
        }

        if (lhs.is_const()) {
            if (lhs.max == 0.0) {
                if (rhs.is_finite()) {
                    flags = ResultFlags::ResultIsConst;
                    result.min = 0.0;
                    result.max = 0.0;
                } else {
                    result.set_unbounded();
                }
                return;
            }

            if (lhs.max == 1.0) {
                flags = ResultFlags::ResultIsRhs;
                result = lhs;
                return;
            }

            if (abs(lhs.max) == std::numeric_limits<float_jt>::infinity()) {
                if (rhs.max < 0.0) {
                    flags = ResultFlags::ResultIsConst;
                    result.min = -rhs.max;
                    return;
                }
                if (rhs.min > 0.0) {
                    flags = ResultFlags::ResultIsConst;
                    result.min = rhs.max;
                    return;
                }
                if (rhs.is_nonzero()) {
                    result.min = -std::numeric_limits<float_jt>::infinity();
                    result.min = std::numeric_limits<float_jt>::infinity();
                    return;
                }
            }
        }

        if (lhs.is_infinite() && !rhs.is_nonzero()) {
            result.set_unbounded();
            return;
        }

        if (rhs.is_infinite() && !lhs.is_nonzero()) {
            result.set_unbounded();
            return;
        }

        float_jt lhs_min_rhs_min = lhs.min * rhs.min;
        float_jt lhs_min_rhs_max = lhs.min * rhs.max;
        float_jt lhs_max_rhs_min = lhs.max * rhs.min;
        float_jt lhs_max_rhs_max = lhs.max * rhs.max;

        auto min_max = std::minmax({ lhs_min_rhs_min, lhs_min_rhs_max, lhs_max_rhs_min, lhs_max_rhs_max });
        result.min = min_max.first;
        result.max = min_max.second;

        if (is_integral_result(lhs, rhs, result)) {
            result.set_integral();
        } else {
            result.min = round_down_error(result.min);
            result.max = round_up_error(result.max);
        }
    }

    static void divide(FloatBounds &result, ResultFlags &flags, const FloatBounds &lhs, FloatBounds &rhs) {
        if (check_nan(lhs, rhs, result)) return;

        // div 0

        // div -0

        // div +-Inf


    }

    static void greater_than(FloatBounds &result, ResultFlags &flags, FloatBounds &lhs, FloatBounds &rhs) {
        if (check_nan(lhs, rhs, result)) return;
    }

    static void greater_than_equal(FloatBounds &result, ResultFlags &flags, FloatBounds &lhs, FloatBounds &rhs) {
        if (check_nan(lhs, rhs, result)) return;
    }

    static void less_than(FloatBounds &result, ResultFlags &flags, FloatBounds &lhs, FloatBounds &rhs) {
        if (check_nan(lhs, rhs, result)) return;
    }

    static void less_than_equal(FloatBounds &result, ResultFlags &flags, FloatBounds &lhs, FloatBounds &rhs) {
        if (check_nan(lhs, rhs, result)) return;
    }

    static void equal(FloatBounds &result, ResultFlags &flags, FloatBounds &lhs, FloatBounds &rhs) {
        if (check_nan(lhs, rhs, result)) return;
    }

    static void not_equal(FloatBounds &result, ResultFlags &flags, FloatBounds &lhs, FloatBounds &rhs) {
        if (check_nan(lhs, rhs, result)) return;
    }
};

#define DISPATCH(METHOD_NAME, ...) if (is_int()) { bounds.int_bounds.METHOD_NAME(__VA_ARGS__); } else { bounds.float_bounds.METHOD_NAME(__VA_ARGS__); }

// defines the possible upper- and lower-bound that a variable might have
class VariableBounds {
    union Bounds {
        IntBounds int_bounds;
        FloatBounds float_bounds;

        Bounds(): int_bounds(0, 0) { }
        Bounds(IntBounds int_bounds): int_bounds(int_bounds) { }
        Bounds(FloatBounds float_bounds): float_bounds(float_bounds) { }
    } bounds;
    RuntimeValueType value_type;

public:

    static VariableBounds unbounded_float() {
        return { FloatBounds::NaN, FloatBounds::NaN };
    }

    static VariableBounds unbounded_int() {
        return { INTPTR_MIN, INTPTR_MAX };
    }

    static VariableBounds constant(float_jt value) {
        VariableBounds result = { value, value };
        if (trunc(value) == value && FloatBounds::in_int_bounds(value)) {
            result.bounds.float_bounds.value_flags |= ValueFlags::Integral;
        }
        if (value != 0) {
            result.bounds.float_bounds.value_flags |= ValueFlags::NonZero;
        }
        return result;
    }

    static VariableBounds constant(int_jt value) {
        return { value, value };
    }
    
    bool is_int() const {
        return this->value_type == RuntimeValueType::Int;
    }
    
    IntBounds &as_int() {
        return this->bounds.int_bounds;
    }

    bool is_float() const {
        return this->value_type == RuntimeValueType::Float;
    }

    FloatBounds &as_float() {
        return this->bounds.float_bounds;
    }

    VariableBounds get_default_result() const {
        return is_int()
            ? VariableBounds::unbounded_int()
            : VariableBounds::unbounded_float();
    }

    static VariableBounds get_bool_result() {
        return {0l, 1l};
    }

    static VariableBounds add(VariableBounds &lhs, VariableBounds &rhs, ResultFlags &flags) {
        VariableBounds result = lhs.get_default_result();
        flags = ResultFlags::None;
        
        if (lhs.is_int()) {
            IntBounds::add(result.as_int(), flags, lhs.as_int(), rhs.as_int());
        } else {
            FloatBounds::add(result.as_float(), flags, lhs.as_float(), rhs.as_float());
        }

        return result;
    }
    
    static VariableBounds subtract(VariableBounds &lhs, VariableBounds &rhs, ResultFlags &flags) {
        VariableBounds result = lhs.get_default_result();
        flags = ResultFlags::None;

        if (lhs.is_int()) {
            IntBounds::subtract(result.as_int(), flags, lhs.as_int(), rhs.as_int());
        } else {
            FloatBounds::subtract(result.as_float(), flags, lhs.as_float(), rhs.as_float());
        }

        return result;
    }

    static VariableBounds multiply(VariableBounds &lhs, VariableBounds &rhs, ResultFlags &flags) {
        VariableBounds result = lhs.get_default_result();
        flags = ResultFlags::None;

        if (lhs.is_int()) {
            IntBounds::multiply(result.as_int(), flags, lhs.as_int(), rhs.as_int());
        } else {
            FloatBounds::multiply(result.as_float(), flags, lhs.as_float(), rhs.as_float());
        }

        return result;
    }

    static VariableBounds divide(VariableBounds &lhs, VariableBounds &rhs, ResultFlags &flags) {
        VariableBounds result = lhs.get_default_result();
        flags = ResultFlags::None;

        if (lhs.is_int()) {
            IntBounds::divide(result.as_int(), flags, lhs.as_int(), rhs.as_int());
        } else {
            FloatBounds::divide(result.as_float(), flags, lhs.as_float(), rhs.as_float());
        }

        return result;
    }

    static VariableBounds mod(VariableBounds &lhs, VariableBounds &rhs, ResultFlags &flags) {
        VariableBounds result = lhs.get_default_result();
        flags = ResultFlags::None;

        if (lhs.is_int()) {
            IntBounds::divide(result.as_int(), flags, lhs.as_int(), rhs.as_int());
        } else {
            flags = ResultFlags::OpError;
        }

        return result;
    }

    static VariableBounds greater_than(VariableBounds &lhs, VariableBounds &rhs, ResultFlags &flags) {
        VariableBounds result = VariableBounds::get_bool_result();
        flags = ResultFlags::None;

        if (lhs.is_int()) {
            IntBounds::greater_than(result.as_int(), flags, lhs.as_int(), rhs.as_int());
        } else {
            FloatBounds::greater_than(result.as_float(), flags, lhs.as_float(), rhs.as_float());
        }

        return result;
    }

    static VariableBounds greater_than_equal(VariableBounds &lhs, VariableBounds &rhs, ResultFlags &flags) {
        VariableBounds result = VariableBounds::get_bool_result();
        flags = ResultFlags::None;

        if (lhs.is_int()) {
            IntBounds::greater_than_equal(result.as_int(), flags, lhs.as_int(), rhs.as_int());
        } else {
            FloatBounds::greater_than_equal(result.as_float(), flags, lhs.as_float(), rhs.as_float());
        }

        return result;
    }

    static VariableBounds less_than(VariableBounds &lhs, VariableBounds &rhs, ResultFlags &flags) {
        VariableBounds result = VariableBounds::get_bool_result();
        flags = ResultFlags::None;

        if (lhs.is_int()) {
            IntBounds::less_than(result.as_int(), flags, lhs.as_int(), rhs.as_int());
        } else {
            FloatBounds::less_than(result.as_float(), flags, lhs.as_float(), rhs.as_float());
        }

        return result;
    }

    static VariableBounds less_than_equal(VariableBounds &lhs, VariableBounds &rhs, ResultFlags &flags) {
        VariableBounds result = VariableBounds::get_bool_result();
        flags = ResultFlags::None;

        if (lhs.is_int()) {
            IntBounds::less_than_equal(result.as_int(), flags, lhs.as_int(), rhs.as_int());
        } else {
            FloatBounds::less_than_equal(result.as_float(), flags, lhs.as_float(), rhs.as_float());
        }

        return result;
    }

    static VariableBounds equal(VariableBounds &lhs, VariableBounds &rhs, ResultFlags &flags) {
        VariableBounds result = VariableBounds::get_bool_result();
        flags = ResultFlags::None;

        if (lhs.is_int()) {
            IntBounds::equal(result.as_int(), flags, lhs.as_int(), rhs.as_int());
        } else {
            FloatBounds::equal(result.as_float(), flags, lhs.as_float(), rhs.as_float());
        }

        return result;
    }

    static VariableBounds not_equal(VariableBounds &lhs, VariableBounds &rhs, ResultFlags &flags) {
        VariableBounds result = VariableBounds::get_bool_result();
        flags = ResultFlags::None;

        if (lhs.is_int()) {
            IntBounds::not_equal(result.as_int(), flags, lhs.as_int(), rhs.as_int());
        } else {
            FloatBounds::not_equal(result.as_float(), flags, lhs.as_float(), rhs.as_float());
        }

        return result;
    }

    static VariableBounds bit_or(VariableBounds &lhs, VariableBounds &rhs, ResultFlags &flags) {
        VariableBounds result = lhs.get_default_result();
        flags = ResultFlags::None;

        if (lhs.is_int()) {
            IntBounds::bit_or(result.as_int(), flags, lhs.as_int(), rhs.as_int());
        } else {
            flags = ResultFlags::OpError;
        }

        return result;
    }

    static VariableBounds bit_and(VariableBounds &lhs, VariableBounds &rhs, ResultFlags &flags) {
        VariableBounds result = lhs.get_default_result();
        flags = ResultFlags::None;

        if (lhs.is_int()) {
            IntBounds::bit_and(result.as_int(), flags, lhs.as_int(), rhs.as_int());
        } else {
            flags = ResultFlags::OpError;
        }

        return result;
    }

    static VariableBounds bit_xor(VariableBounds &lhs, VariableBounds &rhs, ResultFlags &flags) {
        VariableBounds result = lhs.get_default_result();
        flags = ResultFlags::None;

        if (lhs.is_int()) {
            IntBounds::divide(result.as_int(), flags, lhs.as_int(), rhs.as_int());
        } else {
            flags = ResultFlags::OpError;
        }

        return result;
    }

    /*

    static int_jt checked_shl(int_jt lhs, int_jt rhs) {
        rhs = std::min(rhs, INT_BIT_COUNT - 1);
        return (lhs << rhs) & ~((1 << rhs) - 1);
    }

    static int_jt leading_zeros(int_jt value) {
        int_jt n = 0;
        if (value == 0) return INT_BIT_COUNT;
        while (true) {
            if (value < 0) break;
            n++;
            value <<= 1;
        }
        return n;
    }

    static VariableBounds bit_shift_left(const VariableBounds &lhs, VariableBounds &rhs) {
        VariableBounds result = {};

        if (lhs.is_int()) {
            result.value_type = RuntimeValueType::Int;

            if (rhs.max.int_value < 0) {
                utils::raise("Illegal left shift by negative constant", {});
            }

            int_jt lhs_min_leading_zeros = leading_zeros(lhs.min.int_value);
            int_jt lhs_max_leading_zeros = leading_zeros(lhs.max.int_value);

            int_jt minimum_leading_zeros = std::min(lhs_min_leading_zeros, lhs_max_leading_zeros);

            if (rhs.min.int_value < 0) {
                rhs.min.int_value = 0;
            }

            // don't bother trying to narrow this if it possibly overflows into the sign bit
            if (minimum_leading_zeros <= rhs.max.int_value) {
                result.set_unbounded();
                return result;
            }

            int_jt lhs_min = lhs.min.int_value;
            int_jt lhs_max = lhs.max.int_value;
            int_jt rhs_min = rhs.min.int_value;
            int_jt rhs_max = rhs.max.int_value;

            int_jt lhs_min_rhs_min = checked_shl(lhs_min, rhs_min);
            int_jt lhs_max_rhs_min = checked_shl(lhs_max, rhs_min);
            int_jt lhs_min_rhs_max = checked_shl(lhs_min, rhs_max);
            int_jt lhs_max_rhs_max = checked_shl(lhs_max, rhs_max);

            result.min = std::min({ lhs_min_rhs_min, lhs_min_rhs_max, lhs_max_rhs_min, lhs_max_rhs_max });
            result.max = std::max({ lhs_min_rhs_min, lhs_min_rhs_max, lhs_max_rhs_min, lhs_max_rhs_max });

        } else {
            utils::raise("Illegal float operation << encountered while constant folding", {});
        }

        return result;
    }
     */
    VariableBounds(int_jt min, int_jt max): bounds(IntBounds(min, max)) {
        this->value_type = RuntimeValueType::Int;
    }

    VariableBounds(float_jt min, float_jt max): bounds(FloatBounds(min, max)) {
        this->value_type = RuntimeValueType::Float;
    }
};

#endif //JITJAM_VARIABLE_BOUNDS_H
