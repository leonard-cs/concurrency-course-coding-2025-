// #ifndef SPIN_LOCK_PASSIVE_BACKOFF_H
// #define SPIN_LOCK_PASSIVE_BACKOFF_H

// #include <emmintrin.h>

// #include <atomic>
// #include <string>

// class SpinLockPassiveBackoff {
//  public:
//   SpinLockPassiveBackoff() : lock_bit_(false) {}

//   void Lock() {
//     while (lock_bit_.exchange(true)) {
//       // We didn't get the lock!
//       // Rather than immediately trying again, spin until we see
//       // that the lock is free.
//       do {
//         // The lock is not ready for us.
//         // Passively do nothing.
//         _mm_pause();  // Low energy pause
//         _mm_pause();  // Low energy pause
//         _mm_pause();  // Low energy pause
//         _mm_pause();  // Low energy pause
//       } while (lock_bit_.load());

//       // We see that the lock is free! Go back around the outer
//       // loop and try to grab it.
//     }
//     // We only leave the loop if we have the lock
//   }

//   void Unlock() { lock_bit_.store(false); }

//   [[nodiscard]] static std::string GetName() { return "Passive backoff"; }

//  private:
//   // Also see atomic_flag for interest
//   std::atomic<bool> lock_bit_;
// };

// #endif  // SPIN_LOCK_PASSIVE_BACKOFF_H
