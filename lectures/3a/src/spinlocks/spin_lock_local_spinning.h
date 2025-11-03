#ifndef SPIN_LOCK_LOCAL_SPINNING_H
#define SPIN_LOCK_LOCAL_SPINNING_H

#include <atomic>
#include <string>

class SpinLockLocalSpinning {
 public:
  SpinLockLocalSpinning() : lock_bit_(false) {}

  void Lock() {
    while (lock_bit_.exchange(true, std::memory_order_acquire)) {
      // We didn't get the lock!
      // Rather than immediately trying again, spin until we see
      // that the lock is free.
      while (lock_bit_.load(std::memory_order_relaxed)) {
        // The lock is not ready for us.
      }
      // We see that the lock is free! Go back around the outer
      // loop and try to grab it.
    }
    // We only leave the loop if we have the lock
  }

  void Unlock() { lock_bit_.store(false, std::memory_order_release); }

  [[nodiscard]] static std::string GetName() { return "Local spinning"; }

 private:
  // Also see atomic_flag for interest
  std::atomic<bool> lock_bit_;
};

#endif  // SPIN_LOCK_LOCAL_SPINNING_H
