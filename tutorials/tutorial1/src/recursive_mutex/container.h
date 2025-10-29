#ifndef CONTAINER_H
#define CONTAINER_H

#include "recursive_mutex.h"

template <typename T>
class Container {
 public:
  void Add(const T& elem) {
    // TODO
    mutex_.Lock();
    vector_.push_back(elem);
    mutex_.Unlock();
  }

  void AddAll(const std::vector<T>& elems) {
    // TODO
    mutex_.Lock();
    for (T elem : elems) {
      Add(elem);
    }
    mutex_.Unlock();
  }

  void Show() {
    // TODO
    mutex_.Lock();
    std::cout << "[";
    for (size_t i = 0; i < vector_.size() - 1; i++) {
      std::cout << vector_[i] << ", ";
    }
    std::cout << vector_[vector_.size()];
    std::cout << "]" << std::endl;
    mutex_.Unlock();
  }

 private:
  // TODO
  std::vector<T> vector_;
  RecursiveMutex mutex_;
};

#endif  // CONTAINER_H
