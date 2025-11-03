#ifndef SPIN_LOCK_LOCAL_SPINNING_H
#define SPIN_LOCK_LOCAL_SPINNING_H

#include <atomic>
#include <string>

class SpinLockLocalSpinning {
 public:
  SpinLockLocalSpinning() : lock_bit_(false) {}

  void Lock() {
    while (lock_bit_.exchange(true)) {
      while (lock_bit_.load()) {
      }
    }
  }

  void Unlock() { lock_bit_.store(false); }

  [[nodiscard]] static std::string GetName() { return "Local spinning"; }

 private:
  std::atomic<bool> lock_bit_;
};

#endif  // SPIN_LOCK_LOCAL_SPINNING_H
