#ifndef SENSE_REVERSING_BARRIER_H
#define SENSE_REVERSING_BARRIER_H

#include <atomic>

class SenseReversingBarrier {
 public:
  SenseReversingBarrier(size_t num_participants) {
    // Avoid warning about this parameter not being used; remove once you flesh
    // out your solution.
    (void)num_participants;
  }

  void Await() {
    // TODO
  }

 private:
  // TODO
};

#endif  // SENSE_REVERSING_BARRIER_H
