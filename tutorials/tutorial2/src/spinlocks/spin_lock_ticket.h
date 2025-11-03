#ifndef SPIN_LOCK_TICKET_H
#define SPIN_LOCK_TICKET_H

#include <emmintrin.h>

#include <atomic>
#include <string>

class SpinLockTicket {
 public:
  SpinLockTicket() : next_ticket_(0), now_serving_(0) {}

  void Lock() {
    const auto ticket = next_ticket_.fetch_add(1);
    while (now_serving_.load() != ticket) {
      _mm_pause();
    }
  }

  void Unlock() { now_serving_.store(now_serving_.load() + 1); }

  [[nodiscard]] static std::string GetName() { return "Ticket"; }

 private:
  std::atomic<size_t> next_ticket_;
  std::atomic<size_t> now_serving_;
};

#endif  // SPIN_LOCK_TICKET_H
