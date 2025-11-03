#include <chrono>
#include <iostream>
#include <thread>

static void Foo(int& x) {
  std::this_thread::sleep_for(std::chrono::seconds(1));
  std::cout << "Waking up thread 2" << std::endl;
  x = 1;
}

static void Bar(int& x) {
  std::cout << "Thread 2 is waiting" << std::endl;
  while (x == 0) {
    // Do nothing in the loop body
  }
  std::cout << "Thread 2 got woken up!" << std::endl;
}

int main() {
  int x = 0;
  std::thread t1(Foo, std::ref(x));
  std::thread t2(Bar, std::ref(x));
  t1.join();
  t2.join();
  return 0;
}
