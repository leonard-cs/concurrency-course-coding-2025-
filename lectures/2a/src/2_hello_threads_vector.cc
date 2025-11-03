#include <iostream>
#include <thread>
#include <vector>

static void ThreadBody() {
  std::cout << "Hello, my name is " << std::this_thread::get_id() << std::endl;
}

int main() {
  std::vector<std::thread> threads;
  for (size_t i = 0; i < 8; ++i) {
    threads.push_back(std::thread(ThreadBody));
  }
  // At this point, any number between 0 and 8 threads will be running.
  for (size_t i = 0; i < 8; ++i) {
    threads[i].join();
  }
  // All the threads have now terminated, because we joined them all.
  return 0;
}
