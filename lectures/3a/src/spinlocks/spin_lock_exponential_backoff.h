// #ifndef SPIN_LOCK_EXPONENTIAL_BACKOFF_H
// #define SPIN_LOCK_EXPONENTIAL_BACKOFF_H

// #include <emmintrin.h>

// #include <atomic>

// class SpinLockExponentialBackoff {
//  public:
//   SpinLockExponentialBackoff() : lock_bit_(false) {}

//   void Lock() {
//     const int kMinBackoffIterations = 4;
//     const int kMaxBackoffIterations = 1 << 10;

//     while (lock_bit_.exchange(true)) {
//       // We didn't get the lock!
//       // Rather than immediately trying again, spin until we see
//       // that the lock is free.
//       int backoffIterations = kMinBackoffIterations;
//       do {
//         // The lock is not ready for us.
//         // Passively do nothing.
//         for (int i = 0; i < backoffIterations; i++) {
//           _mm_pause();  // Low energy pause
//         }
//         backoffIterations =
//             std::min(kMaxBackoffIterations, backoffIterations * 2);
//       } while (lock_bit_.load());

//       // We see that the lock is free! Go back around the outer
//       // loop and try to grab it.
//     }
//     // We only leave the loop if we have the lock
//   }

//   void Unlock() { lock_bit_.store(false); }

//   [[nodiscard]] static std::string GetName() { return "Exponential backoff";
//   }

//  private:
//   // Also see atomic_flag for interest
//   std::atomic<bool> lock_bit_;
// };

// #endif  // SPIN_LOCK_EXPONENTIAL_BACKOFF_H
