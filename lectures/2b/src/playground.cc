
#include <atomic>
#include <iostream>
#include <ostream>
#include <thread>

static void memoryOrder1() {
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

static void memoryOrder2() {
  int data1 = 0;
  int data2 = 0;

  std::atomic<bool> flag1(false);
  std::atomic<bool> flag2(false);

  auto t1 = std::thread([&]() -> void {
    data1 = 42;                                    // Modify data1
    flag1.store(true, std::memory_order_release);  // Release flag1
    data2 = 99;                                    // Modify data2
    flag2.store(true, std::memory_order_release);  // Release flag2
  });

  auto t2 = std::thread([&]() -> void {
    while (!flag1.load(std::memory_order_acquire));  // Acquire flag1
    std::cout << "Data 1: " << data1 << std::endl;   // Should print 42

    while (!flag2.load(std::memory_order_acquire));  // Acquire flag2
    std::cout << "Data 2: " << data2 << std::endl;   // Should print 99
  });

  t1.join();
  t2.join();
}

int main() {
  memoryOrder2();
  return 0;
}
