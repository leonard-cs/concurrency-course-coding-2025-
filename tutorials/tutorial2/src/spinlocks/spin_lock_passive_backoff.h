#ifndef SPIN_LOCK_PASSIVE_BACKOFF_H
#define SPIN_LOCK_PASSIVE_BACKOFF_H

#include <emmintrin.h>

#include <atomic>
#include <string>

class SpinLockPassiveBackoff {
 public:
  SpinLockPassiveBackoff() : lock_bit_(false) {}

  void Lock() {
    while (lock_bit_.exchange(true)) {
      do {
        for (size_t i = 0; i < 4; i++) {
          _mm_pause();
        }
      } while (lock_bit_.load());
    }
  }

  void Unlock() { lock_bit_.store(false); }

  [[nodiscard]] static std::string GetName() { return "Passive backoff"; }

 private:
  std::atomic<bool> lock_bit_;
};

#endif  // SPIN_LOCK_PASSIVE_BACKOFF_H
