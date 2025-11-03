#ifndef SPIN_LOCK_TICKET_VOLATILE_H
#define SPIN_LOCK_TICKET_VOLATILE_H

#include <emmintrin.h>

#include <atomic>
#include <string>

class SpinLockTicketVolatile {
 public:
  SpinLockTicketVolatile() {}

  void Lock() {
    // TODO
  }

  void Unlock() {
    // TODO
  }

  [[nodiscard]] static std::string GetName() { return "Ticket volatile"; }

 private:
  // TODO
};

#endif  // SPIN_LOCK_TICKET_VOLATILE_H
