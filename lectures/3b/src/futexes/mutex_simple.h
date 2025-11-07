#ifndef MUTEX_SIMPLE_H
#define MUTEX_SIMPLE_H

#include <linux/futex.h>
#include <sys/syscall.h>
#include <unistd.h>

#include <atomic>
#include <string>

// T2 and T3 try to get the lock
// T2 gets the lock
// T3 fails to get the lock and gets ready to go sleep - it about to call
//   futex_wait, but hasn't called it yet
// T2 releases lock by:
//     - setting the lock bit to kFree
//     - calling futex_wake
// At this point, T3 still hasn't called futex_wait
// Finally T3 calls futex_wait(&state_, kLocked).
// The futex_wait call atomically compares state_ with kLocked and finds that
//   they are not equal, and immediately returns.
// T3 therefore does not go to sleep, and tries an exchange to get the lock

class MutexSimple {
 public:
  MutexSimple() : state_(kFree) {}

  void Lock() {
    while (state_.exchange(kLocked) != kFree) {
      syscall(SYS_futex, reinterpret_cast<int*>(&state_), FUTEX_WAIT, kLocked,
              nullptr, nullptr, 0);
    }
  }

  void Unlock() {
    state_.store(kFree);
    syscall(SYS_futex, reinterpret_cast<int*>(&state_), FUTEX_WAKE, 1, nullptr,
            nullptr, 0);
  }

  [[nodiscard]] static std::string GetName() { return "Simple"; }

 private:
  const int kFree = 0;
  const int kLocked = 1;
  std::atomic<int> state_;
};

#endif  // MUTEX_SIMPLE_H
