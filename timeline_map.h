
//

#ifndef JITJAM_TIMELINE_MAP_H
#define JITJAM_TIMELINE_MAP_H

#include <vector>
#include <set>

// TODO:
// this is unfinished, but I'm trying to migrate the logic from the mem packer
// to a more generic timespan map, since the concept of using spans of time as keys is a repeating concept

struct TimeSpan {
    // inclusive
    int start_iter;
    // exclusive
    int end_iter;

    bool operator==(const TimeSpan& other) const {
        return start_iter == other.start_iter;
    }
};

struct TimeSpanEntry {
    TimeSpan timespan;

    TimeSpanEntry(const TimeSpan &timespan): timespan(timespan) { }

    bool operator<(const TimeSpanEntry &other) const {
        return timespan.start_iter < other.timespan.start_iter;
    }

    bool operator>(const TimeSpanEntry &other) const {
        return timespan.start_iter > other.timespan.start_iter;
    }
};

struct Lane {
    std::set<TimeSpanEntry> timespans = std::set<TimeSpanEntry>();

    bool is_in_bounds(TimeSpanEntry *entry) {

    }

    bool can_contain(const TimeSpan &rsvn) const {
        if (timespans.empty()) {
            return true;
        }

        auto next = timespans.lower_bound(TimeSpanEntry(rsvn));
        auto &next_res = (next == timespans.end())
                         ? timespans.rbegin()->timespan
                         : next->timespan;
        auto non_overlapping_with_previous = next_res == rsvn
                                             || next_res.end_iter < rsvn.start_iter
                                             || rsvn.end_iter < next_res.start_iter;

        if (non_overlapping_with_previous &&
            next != timespans.begin()) {
            auto prev = --next;
            auto &prev_res = prev->timespan;
            return rsvn.end_iter < prev_res.start_iter
                   || prev_res.end_iter < rsvn.start_iter;
        }
        return non_overlapping_with_previous;
    }

    bool reserve(const TimeSpan &rsvn) {
        return timespans.insert(TimeSpanEntry(rsvn)).second;
    }
};

#endif //JITJAM_TIMELINE_MAP_H
