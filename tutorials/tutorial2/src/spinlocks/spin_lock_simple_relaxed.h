#ifndef SPIN_LOCK_SIMPLE_RELAXED_H
#define SPIN_LOCK_SIMPLE_RELAXED_H

#include <atomic>
#include <string>

class SpinLockSimpleRelaxed {
 public:
  SpinLockSimpleRelaxed() {}

  void Lock() {
    // TODO
  }

  void Unlock() {
    // TODO
  }

  [[nodiscard]] static std::string GetName() { return "Simple relaxed"; }

 private:
  // TODO
};

#endif  // SPIN_LOCK_SIMPLE_RELAXED_H
