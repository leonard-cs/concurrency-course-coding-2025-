#ifndef SPIN_LOCK_EXPONENTIAL_BACKOFF_H
#define SPIN_LOCK_EXPONENTIAL_BACKOFF_H

#include <emmintrin.h>

#include <atomic>

class SpinLockExponentialBackoff {
 public:
  SpinLockExponentialBackoff() : lock_bit_(false) {}

  void Lock() {
    const size_t kMinBackoffIterations = 4u;
    const size_t kMaxBackoffIterations = 1u << 10u;
    size_t backoff_iterations = kMinBackoffIterations;
    while (lock_bit_.exchange(true)) {
      do {
        for (size_t i = 0; i < backoff_iterations; i++) {
          _mm_pause();
        }
        backoff_iterations =
            std::min(backoff_iterations * 2, kMaxBackoffIterations);
      } while (lock_bit_.load());
    }
  }

  void Unlock() { lock_bit_.store(false); }

  [[nodiscard]] static std::string GetName() { return "Exponential backoff"; }

 private:
  std::atomic<bool> lock_bit_;
};

#endif  // SPIN_LOCK_EXPONENTIAL_BACKOFF_H
