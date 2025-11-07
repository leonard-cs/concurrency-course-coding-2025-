#ifndef MUTEX_SMART_H
#define MUTEX_SMART_H

#include <linux/futex.h>
#include <sys/syscall.h>
#include <unistd.h>

#include <atomic>
#include <string>

class MutexSmart {
 public:
  MutexSmart() : state_(kFree) {}

  void Lock() {
    // Try to get the lock on the "fast path"
    int old_value = cmpxchg(kFree, kLockedNoWaiters);
    if (old_value == kFree) {
      // We got the lock with no contention. We have set it to kLockedNoWaiters
      // - this makes sense because we had no reason to believe it was
      // contended.
      return;
    }

    // We didn't manage to get the lock, soooo .... slow path:

    do {
      // We just failed to get the lock, so we may need to go to sleep.

      // If the lock state was already kLockedWaiters, the following will have
      // no effect, which is fine.
      // But if the lock state was actually kLockedNoWaiters, this records that
      // we are a waiter.
      if (old_value == kLockedWaiters ||
          cmpxchg(kLockedNoWaiters, kLockedWaiters) != kFree) {
        // Call futex_wait, blocking only if the futex word actually is
        // kLockedWaiters. If it not, then this will be a no-op.
        syscall(SYS_futex, reinterpret_cast<int*>(&state_), FUTEX_WAIT,
                kLockedWaiters, nullptr, nullptr, 0);
      }

      // Either we just got woken, because the lock became free, or we didn't
      // go to sleep because the lock was freed between our cmpxchg and the
      // futex call.

      // Either way try to get the lock again!
      // We need to change the lock state to kLockedWaiters if we get the lock:
      // this is because we had to fight for the lock, so there might be waiters
      // that we should wake in due course.
      old_value = cmpxchg(kFree, kLockedWaiters);
    } while (old_value != kFree);
  }

  void Unlock() {
    // Set the lock state to free, and find out its previous value
    if (state_.exchange(kFree) == kLockedWaiters) {
      // We have reason to believe there are waiters, so wake one of them.
      syscall(SYS_futex, reinterpret_cast<int*>(&state_), FUTEX_WAKE, 1,
              nullptr, nullptr, 0);
    }
  }

  [[nodiscard]] static std::string GetName() { return "Smart"; }

 private:
  int cmpxchg(int expected, int desired) {
    // If the current value of state_ is expected, this will set state_ to
    // desired, and leave expected alone.

    // Otherwise it will leave state_ alone, and set expected to the value
    // stored in state_.
    state_.compare_exchange_strong(expected, desired);
    return expected;
  }

  const int kFree = 0;

  // The mutex is locked, and we have no reason to believe there are waiters
  // (but there might actually be waiters)
  const int kLockedNoWaiters = 1;

  // The mutex is locked, and we some reason to believe there may be waiters
  // (but there might actually be no waiters)
  const int kLockedWaiters = 2;

  std::atomic<int> state_;
};
#endif  // MUTEX_SMART_H
