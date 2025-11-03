#ifndef SPIN_LOCK_ALOCK_FALSE_SHARING_H
#define SPIN_LOCK_ALOCK_FALSE_SHARING_H

#include <array>
#include <atomic>
#include <string>

class SpinLockALockFalseSharing {
 public:
  SpinLockALockFalseSharing() {}

  void Lock() {
    // TODO
  }

  void Unlock() {
    // TODO
  }

  [[nodiscard]] static std::string GetName() { return "ALock false sharing"; }

 private:
  // TODO
};

#endif  // SPIN_LOCK_ALOCK_FALSE_SHARING_H
