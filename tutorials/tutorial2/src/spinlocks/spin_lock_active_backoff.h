#ifndef SPIN_LOCK_ACTIVE_BACKOFF_H
#define SPIN_LOCK_ACTIVE_BACKOFF_H

#include <atomic>
#include <string>

class SpinLockActiveBackoff {
 public:
  SpinLockActiveBackoff() : lock_bit_(false) {}

  void Lock() {
    while (lock_bit_.exchange(true)) {
      do {
        for (volatile size_t i = 0; i < 100; i = i + 1) {
          // Do nothing
        }
      } while (lock_bit_.load());
    }
  }

  void Unlock() { lock_bit_.store(false); }

  [[nodiscard]] static std::string GetName() { return "Active backoff"; }

 private:
  std::atomic<bool> lock_bit_;
};

#endif  // SPIN_LOCK_ACTIVE_BACKOFF_H
