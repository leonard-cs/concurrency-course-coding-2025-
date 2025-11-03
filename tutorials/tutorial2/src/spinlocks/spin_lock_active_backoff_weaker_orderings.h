#ifndef SPIN_LOCK_ACTIVE_BACKOFF_WEAKER_ORDERINGS_H
#define SPIN_LOCK_ACTIVE_BACKOFF_WEAKER_ORDERINGS_H

#include <atomic>
#include <string>

class SpinLockActiveBackoffWeakerOrderings {
 public:
  SpinLockActiveBackoffWeakerOrderings() {}

  void Lock() {
    // TODO
  }

  void Unlock() {
    // TODO
  }

  [[nodiscard]] static std::string GetName() {
    return "Active backoff weaker orderings";
  }

 private:
  // TODO
};

#endif  // SPIN_LOCK_ACTIVE_BACKOFF_WEAKER_ORDERINGS_H
