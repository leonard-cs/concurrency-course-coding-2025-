#ifndef SHARED_MUTEX_NATIVE_H
#define SHARED_MUTEX_NATIVE_H

#include <shared_mutex>

#include "src/shared_mutexes/shared_mutex_base.h"

class SharedMutexNative : public SharedMutexBase {
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

#endif  // SHARED_MUTEX_NATIVE_H
