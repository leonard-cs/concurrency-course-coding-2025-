#ifndef SPIN_LOCK_TICKET_NON_ATOMIC_H
#define SPIN_LOCK_TICKET_NON_ATOMIC_H

#include <emmintrin.h>

#include <atomic>
#include <string>

class SpinLockTicketNonAtomic {
 public:
  SpinLockTicketNonAtomic() {}

  void Lock() {
    // TODO
  }

  void Unlock() {
    // TODO
  }

  [[nodiscard]] static std::string GetName() { return "Ticket nonatomic"; }

 private:
  // TODO
};

#endif  // SPIN_LOCK_TICKET_NON_ATOMIC_H
