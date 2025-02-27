
//

#ifndef JITJAM_VARIABLE_BOUNDS_H
#define JITJAM_VARIABLE_BOUNDS_H

#include "runtime_values.h"

using namespace jt;

enum BoundType {
    SignedMax,
    Int,
    Float
};

class VariableBounds {
    RuntimeValue min;
    RuntimeValue max;
    BoundType min_type = BoundType::SignedMax;
    BoundType max_type = BoundType::SignedMax;

public:
    VariableBounds() = default;
    VariableBounds(int_jt min, int_jt max) {
        this->min_type = BoundType::Int;
        this->max_type = BoundType::Int;
        this->min = min;
        this->max = max;
    }
private:

    static BoundType get_dominant_type(const BoundType &lhs, const BoundType &rhs) {
        if (lhs == BoundType::SignedMax || rhs == BoundType::SignedMax) {
            // if either could be the max negative value, then we have to assume the worst
            return BoundType::SignedMax;
        }
        return lhs;
    }

    static bool adding_overflows(const int_jt l, const int_jt r) {
        return l > 0 && r > (INTPTR_MAX - l);
    }

    static bool adding_underflows(const int_jt l, const int_jt r) {
        return l < 0 && r < (INTPTR_MIN - l);
    }

    static bool subtracting_overflows(const int_jt l, const int_jt r) {
        // 2 > 0 && r < -13  (-16 + 1 + 2)
        // 1 > 0 && r < -14
        // 15 > 0 && r < 0   (-16 + 1 + 15)
        return l > 0 && r < (INTPTR_MIN + (l + 1));
    }

    static bool subtracting_underflows(const int_jt l, const int_jt r) {
        // -2 < 0 && r > 15 + (-2 + 1)
        // -1 < 0 && r > 15 + (-1 + 1)
        // -16 < 0 && r > 15 + (-16 + 1) -> -16 < 0 && r > 0
        return l < 0 && r > (INTPTR_MAX + (l + 1));
    }

    static int_jt saturating_multiply(const int_jt l, const int_jt r) {
        if (l == 0) {
            return 0;
        }
        int_jt product = r * l;
        int_jt quot = product / l;
        int_jt resulting_sign = sign_of(r) * sign_of(l);
        if (quot == r) {
            return product;
        }
        if (resulting_sign == 1) {
            return INTPTR_MAX;
        }
        return INTPTR_MIN;
    }

    static VariableBounds add(const VariableBounds &lhs, const VariableBounds &rhs) {
        VariableBounds result = {};

        // re-calc min
        // if the values are at least x and at least y, then their sum should be at least (x + y)
        // unless x + y would result in over/underflow
        // in which case,
        switch (get_dominant_type(lhs.min_type, rhs.min_type)) {
            case SignedMax: {
                result.min_type = BoundType::SignedMax;
                break;
            }
            case Int: {
                result.min_type = BoundType::Int;

                auto &l = lhs.min.int_value;
                auto &r = rhs.min.int_value;

                if (adding_overflows(l, r) || adding_underflows(l, r)) {
                    // we have no guarantees anymore, so reset the bounds and exit
                    result.min_type = BoundType::SignedMax;
                    result.max_type = BoundType::SignedMax;
                    return result;
                } else {
                    result.min = l + r;
                }

                break;
            }
            case Float: {
                result.min_type = BoundType::Float;

                auto &l = lhs.min.float_value;
                auto &r = rhs.min.float_value;

                auto sum = l + r;
                result.min = sum;
                if (!isfinite(sum)) {
                    result.min_type = BoundType::SignedMax;
                }
                break;
            }
        }

        // re-calc max
        switch (get_dominant_type(lhs.max_type, rhs.max_type)) {
            case SignedMax: {
                result.max_type = BoundType::SignedMax;
                break;
            }
            case Int: {
                result.max_type = BoundType::Int;

                auto &l = lhs.max.int_value;
                auto &r = rhs.max.int_value;

                if (adding_overflows(l, r) || adding_underflows(l, r)) {
                    // we have no guarantees anymore, so reset the bounds and exit
                    result.min_type = BoundType::SignedMax;
                    result.max_type = BoundType::SignedMax;
                    return result;
                } else {
                    result.max = l + r;
                }

                break;
            }
            case Float: {
                result.min_type = BoundType::Float;

                auto &l = lhs.max.float_value;
                auto &r = rhs.max.float_value;

                auto sum = l + r;
                result.max = sum;
                if (!isfinite(sum)) {
                    result.max_type = BoundType::SignedMax;
                }
                break;
            }
        }

        return result;
    }

    static VariableBounds subtract(const VariableBounds &lhs, const VariableBounds &rhs) {
        VariableBounds result = {};

        // re-calc min
        // here, the resulting min is smallest by doing rhs.min - lhs.max
        switch (get_dominant_type(lhs.min_type, rhs.max_type)) {
            case SignedMax: {
                result.min_type = BoundType::SignedMax;
                break;
            }
            case Int: {
                result.min_type = BoundType::Int;

                auto &l = lhs.min.int_value;
                auto &r = rhs.max.int_value;

                if (subtracting_overflows(l, r) || subtracting_underflows(l, r)) {
                    // we have no guarantees anymore, so reset the bounds and exit
                    result.min_type = BoundType::SignedMax;
                    result.max_type = BoundType::SignedMax;
                    return result;
                } else {
                    result.min = l - r;
                }

                break;
            }
            case Float: {
                result.min_type = BoundType::Float;

                auto &l = lhs.min.float_value;
                auto &r = rhs.max.float_value;

                auto sum = l - r;
                result.min = sum;
                if (!isfinite(sum)) {
                    result.min_type = BoundType::SignedMax;
                }
                break;
            }
        }

        // re-calc max
        // here, the resulting max is largest by doing lhs.max - rhs.min
        switch (get_dominant_type(lhs.max_type, rhs.min_type)) {
            case SignedMax: {
                result.max_type = BoundType::SignedMax;
                break;
            }
            case Int: {
                result.max_type = BoundType::Int;

                auto &l = lhs.max.int_value;
                auto &r = rhs.min.int_value;

                if (subtracting_overflows(l, r) || subtracting_underflows(l, r)) {
                    // we have no guarantees anymore, so reset the bounds and exit
                    result.min_type = BoundType::SignedMax;
                    result.max_type = BoundType::SignedMax;
                    return result;
                } else {
                    result.max = l - r;
                }

                break;
            }
            case Float: {
                result.min_type = BoundType::Float;

                auto &l = lhs.max.float_value;
                auto &r = rhs.min.float_value;

                auto sum = l - r;
                result.max = sum;
                if (!isfinite(sum)) {
                    result.max_type = BoundType::SignedMax;
                }
                break;
            }
        }

        return result;
    }

    static int_jt sign_of(int_jt value) {
        return value < 0
               ? -1
               : 1;
    }

    static VariableBounds multiply(const VariableBounds &lhs, const VariableBounds &rhs) {
        VariableBounds result = {};

        if (lhs.min_type == BoundType::Int
            || rhs.min_type == BoundType::Int
            || lhs.max_type == BoundType::Int
            || rhs.max_type == BoundType::Int) {

            int_jt lhs_min = lhs.min_type == BoundType::SignedMax ? INTPTR_MIN : lhs.min.int_value;
            int_jt lhs_max = lhs.min_type == BoundType::SignedMax ? INTPTR_MAX : lhs.max.int_value;
            int_jt rhs_min = rhs.min_type == BoundType::SignedMax ? INTPTR_MIN : rhs.min.int_value;
            int_jt rhs_max = rhs.min_type == BoundType::SignedMax ? INTPTR_MAX : rhs.max.int_value;

            int_jt lhs_min_rhs_min = saturating_multiply(lhs_min, rhs_min);
            int_jt lhs_min_rhs_max = saturating_multiply(lhs_min, rhs_max);
            int_jt lhs_max_rhs_min = saturating_multiply(lhs_max, rhs_min);
            int_jt lhs_max_rhs_max = saturating_multiply(lhs_max, rhs_max);

            int_jt min_product = std::min(
                std::min(lhs_min_rhs_min, lhs_min_rhs_max),
                std::min(lhs_max_rhs_min, lhs_max_rhs_max)
            );

            int_jt max_product = std::max(
                std::max(lhs_min_rhs_min, lhs_min_rhs_max),
                std::max(lhs_max_rhs_min, lhs_max_rhs_max)
            );

            if (min_product == INTPTR_MIN || max_product == INTPTR_MAX) {
                // overflow occurred
                result.min_type = BoundType::SignedMax;
                result.max_type = BoundType::SignedMax;
                return result;
            }
            result.min_type = BoundType::Int;
            result.min = min_product;

            result.max_type = BoundType::Int;
            result.max = max_product;

        } else if (lhs.min_type == BoundType::Float
                   || rhs.min_type == BoundType::Float
                   || lhs.max_type == BoundType::Float
                   || rhs.max_type == BoundType::Float) {

        } else {

        }

        return result;
    }
};

#endif //JITJAM_VARIABLE_BOUNDS_H
