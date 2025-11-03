// #ifndef SPIN_LOCK_TICKET_H
// #define SPIN_LOCK_TICKET_H

// #include <emmintrin.h>

// #include <atomic>
// #include <string>

// class SpinLockTicket {
//  public:
//   SpinLockTicket() : next_ticket_(0), now_serving_(0) {}

//   void Lock() {
//     // Get a ticket
//     unsigned my_ticket = next_ticket_.fetch_add(1);

//     // Wait until this ticket being served
//     while (my_ticket != now_serving_.load()) {
//       for (int i = 0; i < 4; i++) {
//         _mm_pause();
//       }
//     }
//     // At this point, my_ticket is being served - i.e., I have
//     // the lock!
//   }

//   void Unlock() { now_serving_.store(now_serving_.load() + 1); }

//   [[nodiscard]] static std::string GetName() { return "Ticket"; }

//  private:
//   std::atomic<unsigned> next_ticket_;
//   std::atomic<unsigned> now_serving_;
// };

// #endif  // SPIN_LOCK_TICKET_H
