#ifndef RECURSIVE_MUTEX_BASE_H
#define RECURSIVE_MUTEX_BASE_H

#include <cassert>
#include <mutex>
#include <thread>

class RecursiveMutex {
 public:
  void Lock() {
    // TODO
    if (owner_ == std::this_thread::get_id()) {
      lock_count_++;
      return;
    }

    std::unique_lock<std::mutex> lock(mutex_);
    not_locked_.wait(lock, [this]() { return owner_ == std::thread::id(); });

    owner_ = std::this_thread::get_id();
    lock_count_ = 1;
  }

  void Unlock() {
    // TODO
    std::unique_lock<std::mutex> lock(mutex_);

    assert(owner_ != std::this_thread::get_id());

    lock_count_--;

    if (lock_count_ == 0) {
      owner_ = std::thread::id();
      not_locked_.notify_all();
    }
  }

 private:
  // TODO
  std::mutex mutex_;
  std::thread::id owner_;
  unsigned int lock_count_;
  std::condition_variable not_locked_;
};

#endif  // RECURSIVE_MUTEX_BASE_H
