#ifndef SHARED_MUTEX_SIMPLE_H
#define SHARED_MUTEX_SIMPLE_H

#include <cassert>
#include <condition_variable>
#include <mutex>

#include "src/shared_mutexes/shared_mutex_base.h"

class SharedMutexSimple : public SharedMutexBase {
 public:
  void Lock() final {}

  void Unlock() final {
    // TODO
  }

  void LockShared() final {
    // TODO
  }

  void UnlockShared() final {
    // TODO
  }

 private:
  // TODO
};

#endif  // SHARED_MUTEX_SIMPLE_H
