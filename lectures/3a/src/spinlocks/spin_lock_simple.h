#ifndef SPIN_LOCK_SIMPLE_H
#define SPIN_LOCK_SIMPLE_H

#include <atomic>
#include <string>

class SpinLockSimple {
 public:
  SpinLockSimple() : lock_bit_(false) {}

  void Lock() {
    while (lock_bit_.exchange(true)) {
      // We didn't get the lock!
    }
    // We only leave the loop if we have the lock
  }

  void Unlock() { lock_bit_.store(false); }

  [[nodiscard]] static std::string GetName() { return "Simple"; }

 private:
  // Also see atomic_flag for interest
  std::atomic<bool> lock_bit_;
};

#endif  // SPIN_LOCK_SIMPLE_H
