
#include <atomic>
#include <iostream>
#include <ostream>
#include <thread>

int main() {
  int data = 0;

  std::atomic<bool> flag(false);

  auto t1 = std::thread([&data, &flag]() -> void {
    data = 42;
    flag.store(true, std::memory_order_relaxed);
  });

  auto t2 = std::thread([&data, &flag]() -> void {
    while (!flag.load(std::memory_order_relaxed)) {
      // spin
    }
    std::cout << data << std::endl;
  });

  t1.join();
  t2.join();
}
