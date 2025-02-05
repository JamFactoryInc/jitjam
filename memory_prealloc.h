#ifndef JITJAM_MEMORY_PREALLOC_H
#define JITJAM_MEMORY_PREALLOC_H


#include <list>
#include <algorithm>
#include <vector>
#include <set>
#include <cstdint>
#include <limits>
#include <ostream>
#include <iostream>

typedef int ReservationId;

struct MemReservation {
    const ReservationId id;
    const int start_iter;
    int end_iter = -1;
    // number of 8-byte words this reservation requires
    uint16_t lane_count;
    uint16_t reserved_count = 0;

    MemReservation(ReservationId id, int start_iter, int lane_count):
        id(id),
        start_iter(start_iter),
        lane_count(lane_count) { }

    bool operator==(const MemReservation &other) const {
        return id == other.id;
    }

    bool operator!=(const MemReservation &other) const {
        return id != other.id;
    }

    bool operator>(const MemReservation &other) const {
        return lane_count > other.lane_count;
    }
};
//std::ostream& operator<<(std::ostream& os, const MemReservation& res)
//{
//    return os << "MemReservation {"
//        << " id: " << res.id
//        << ", range: [" << res.start_iter << ", " << res.end_iter << "]"
//        << ", lane_count: " << res.lane_count
//        << ", reserved: " << res.reserved_count
//        << " }";
//}

struct MemReservationPtr {
    MemReservation *ptr = nullptr;

    MemReservationPtr(MemReservation *ptr): ptr(ptr) { }

    MemReservation &operator *() {
        return *ptr;
    }

    bool operator>(MemReservationPtr const &r) const {
        return *ptr > *r.ptr;
    }
};


struct IterData {
    std::multiset<MemReservationPtr, std::greater<MemReservationPtr>> reservations;
    int required_lanes = 0; // sort by this desc
    int iter;

    IterData() = default;
    IterData(int iter): iter(iter) {
        reservations = std::multiset<MemReservationPtr, std::greater<MemReservationPtr>>();
    }

    bool operator>(const IterData &other) const {
        return required_lanes > other.required_lanes;
    }
};
//std::ostream& operator<<(std::ostream& os, const IterData& data)
//{
//    os << "IterData {";
//    os << " required_lanes: " << data.required_lanes;
//    os << ", iter: " << data.iter;
//    os << ", reservations: {" ;
//    for (auto rsvn : data.reservations) {
//        os << std::endl << ">    " << *rsvn << ", ";
//    }
//    if (data.reservations.size()) {
//        os << std::endl;
//    }
//    return os << "}";
//}

struct LaneReservation {
    const MemReservation *reservation = nullptr;

    LaneReservation(const MemReservation &reservation): reservation(&reservation) { }

    bool operator<(const LaneReservation &other) const {
        return reservation->start_iter < other.reservation->start_iter;
    }

    bool operator>(const LaneReservation &other) const {
        return reservation->start_iter > other.reservation->start_iter;
    }
};
struct Lane {
    std::set<LaneReservation> lane_reservations = std::set<LaneReservation>();

    bool can_contain(const MemReservation &rsvn) const {
        if (lane_reservations.empty()) {
            return true;
        }

        auto next = lane_reservations.lower_bound(LaneReservation(rsvn));
        auto &next_res = (next == lane_reservations.end())
            ? *lane_reservations.rbegin()->reservation
            : *next->reservation;
        auto non_overlapping_with_previous = next_res == rsvn
            || next_res.end_iter < rsvn.start_iter
            || rsvn.end_iter < next_res.start_iter;

        if (non_overlapping_with_previous &&
            next != lane_reservations.begin()) {
            auto prev = --next;
            auto &prev_res = *prev->reservation;
            return rsvn.end_iter < prev_res.start_iter
                || prev_res.end_iter < rsvn.start_iter;
        }
        return non_overlapping_with_previous;
    }

    bool reserve(const MemReservation &rsvn) {
        return lane_reservations.insert(LaneReservation(rsvn)).second;
    }
};
//std::ostream& operator<<(std::ostream &os, const Lane &lane) {
//    os << "Lanes {";
//    for (auto rsvn : lane.lane_reservations) {
//        os << " [#" << rsvn.reservation->id << " " << rsvn.reservation->lane_count << "L: " << rsvn.reservation->start_iter << ", " << rsvn.reservation->end_iter << "],";
//    }
//    return os << " }";
//}

struct MemoryRequirement {
    std::vector<int> offsets;
    int total_bytes;
};

struct MemPacker {
    std::vector<MemReservation> reservations = std::vector<MemReservation>();
    // this is the same as the next iter id (i.e. ids are 0-based and are never greater than this value)
    int iter_count = 0;

    MemPacker() {
        reservations.reserve(8);
    }

    MemReservation reserve(int size) {
        auto lanes_divmod = std::div(size, 8);
        uint16_t lanes = lanes_divmod.quot + (lanes_divmod.rem != 0);
        auto rsvn = MemReservation(
            static_cast<int>(reservations.size()),
            iter_count++,
            lanes
        );
        reservations.push_back(rsvn);
        return rsvn;
    }

    void free(ReservationId rsvn_id) {
        reservations[rsvn_id].end_iter = iter_count++;
    }

    MemoryRequirement pack() {
        const auto reservation_count = reservations.size();

        for (auto &rsvn : reservations) {
            if (rsvn.end_iter == -1) {
                free(rsvn.id);
            }
        }

        auto iters = std::vector<IterData>(iter_count);
        for (int i = 0; i < iter_count; ++i) {
            iters[i] = IterData(i);
        }

        int lane_count = 0;
        for (auto &rsvn : reservations) {
            for (int i = rsvn.start_iter; i <= rsvn.end_iter; ++i) {
                auto &iter = iters[i];
                iter.required_lanes += rsvn.lane_count;
                lane_count = std::max(
                    lane_count,
                    iter.required_lanes
                );
                iter.reservations.insert(&rsvn);
                // some redundant assignments but we already accessed the memory, and it's probably pipelined anyway
                iter.iter = i;
            }
        }

        std::sort(iters.begin(), iters.end(), std::greater<IterData>());

        auto lanes = std::vector<Lane>(lane_count);
        for (int i = 0; i < lane_count; ++i) {
            lanes[i] = Lane();
        }

        // iterating from most concurrent reservations to fewest
        int reserved_count = 0;
        for (auto iter : iters) {
            for (auto rsvn : iter.reservations) {
                auto &reservation = *rsvn;
                if (reservation.reserved_count >= reservation.lane_count) continue;

                int lanes_to_reserve = reservation.lane_count;
                int consecutive_lanes_start = 0;
                for (int lane_idx = 0; lane_idx < lanes.size() && lanes_to_reserve > 0; ++lane_idx) {
                    auto &lane = lanes[lane_idx];

                    if (lane.can_contain(reservation)) {
                        --lanes_to_reserve;
                    } else {
                        consecutive_lanes_start = lane_idx + 1;
                        lanes_to_reserve = reservation.lane_count;
                        continue;
                    }
                }

                int end_idx = consecutive_lanes_start + reservation.lane_count;
                bool reserved_any = false;
                for (int lane_idx = consecutive_lanes_start; lane_idx < end_idx; ++lane_idx) {
                    auto &lane = lanes[lane_idx];
                    auto reserved = lane.reserve(reservation);
                    reservation.reserved_count += reserved;
                    reserved_any |= reserved;
                }
                reserved_count += reserved_any;

                if (reserved_count >= reservation_count) goto end;
            }
        }
        end:

        auto offsets = std::vector<int>(reservation_count, std::numeric_limits<int>::max());
        for (int i = 0; i < lanes.size(); ++i) {
            auto &lane = lanes[i];

            for (auto &rsvn : lane.lane_reservations) {
                auto id = rsvn.reservation->id;
                auto offset = i * 8; // each lane is 8 bytes
                auto &offset_val = offsets[id];
                offset_val = std::min(offset_val, offset);
            }
        }

        return MemoryRequirement {
            offsets,
            lane_count * 8,
        };
    }
};


#endif //JITJAM_MEMORY_PREALLOC_H
