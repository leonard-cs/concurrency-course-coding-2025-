#ifndef SPIN_LOCK_ALOCK_PADDED_H
#define SPIN_LOCK_ALOCK_PADDED_H

#include <array>
#include <atomic>
#include <string>

class SpinLockALockPadded {
 public:
  SpinLockALockPadded() {}

  void Lock() {
    // TODO
  }

  void Unlock() {
    // TODO
  }

  [[nodiscard]] static std::string GetName() { return "ALock padded"; }

 private:
  // TODO
};

#endif  // SPIN_LOCK_ALOCK_PADDED_H
