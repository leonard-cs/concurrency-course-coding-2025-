#ifndef SPIN_LOCK_TICKET_OPTIMISED_H
#define SPIN_LOCK_TICKET_OPTIMISED_H

#include <emmintrin.h>

#include <atomic>
#include <string>

class SpinLockTicketOptimised {
 public:
  SpinLockTicketOptimised() {}

  void Lock() {
    // TODO
  }

  void Unlock() {
    // TODO
  }

  [[nodiscard]] static std::string GetName() { return "Ticket optimised"; }

 private:
  // TODO
};

#endif  // SPIN_LOCK_TICKET_OPTIMISED_H
