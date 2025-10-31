#include <condition_variable>
#include <iostream>
#include <mutex>
#include <ostream>
#include <thread>
#include <vector>

class LockedQueue {
 public:
  explicit LockedQueue(size_t capacity) { contents_.resize(capacity); }

  void Enq(int element) {
    // To enqueue an element, the queue cannot be full

    // Let us use a condition variable to wait until the queue is not full
    std::unique_lock<std::mutex> lock(mutex_);
    not_full_.wait(lock,
                   [this]() -> bool { return count_ < contents_.size(); });
    // When we get here we still hold the mutex and we know the queue is not
    // full
    contents_[tail_] = element;
    tail_ = (tail_ + 1) % contents_.size();
    count_++;
    // We have added something to the queue, so notify anyone waiting for it to
    // become non-empty!
    not_empty_.notify_one();
  }

  int Deq() {
    std::unique_lock<std::mutex> lock(mutex_);
    not_empty_.wait(lock, [this]() -> bool { return count_ > 0; });
    int result = contents_[head_];
    head_ = (head_ + 1) % contents_.size();
    count_--;
    not_full_.notify_one();
    return result;
  }

 private:
  std::vector<int> contents_;
  size_t count_ = 0;
  size_t head_ = 0;
  size_t tail_ = 0;
  std::mutex mutex_;
  std::condition_variable not_full_;
  std::condition_variable not_empty_;
};

int main() {
  const size_t NUM_CONSUMERS = 8;
  const size_t ELEMENTS_TO_PRODUCE = 1 << 10;
  const size_t ELEMENTS_PER_CONSUMER = ELEMENTS_TO_PRODUCE / NUM_CONSUMERS;

  LockedQueue producer_to_consumers(4);
  LockedQueue consumers_to_producer(NUM_CONSUMERS);

  int final_result = 0;

  std::thread producer([&consumers_to_producer, &final_result,
                        &producer_to_consumers]() -> void {
    // Write a whole load of integers to the producer-to-consumer-queue
    for (size_t i = 0; i < ELEMENTS_TO_PRODUCE; i++) {
      producer_to_consumers.Enq(1);
    }

    // Read a result per consumer from the consumers-to-producer queue,
    // adding them all up
    for (size_t i = 0; i < NUM_CONSUMERS; i++) {
      final_result += consumers_to_producer.Deq();
    }
  });

  std::vector<std::thread> consumers;
  consumers.reserve(NUM_CONSUMERS);
  for (size_t i = 0; i < NUM_CONSUMERS; i++) {
    consumers.emplace_back(
        [&consumers_to_producer, &producer_to_consumers]() -> void {
          int my_result = 0;
          for (size_t j = 0; j < ELEMENTS_PER_CONSUMER; j++) {
            my_result += producer_to_consumers.Deq();
          }
          consumers_to_producer.Enq(my_result);
        });
  }

  producer.join();
  for (auto& consumer : consumers) {
    consumer.join();
  }

  std::cout << "Final result: " << final_result << std::endl;

  return 0;
}
