#include <iostream>
#include <thread>

static void ThreadBody1() { std::cout << "Hello" << std::endl; }

int main() {
  std::thread t1(ThreadBody1);
  std::thread t2([/* no variables are captured */](
                     /* the lambda takes no arguments */) -> void {
    std::cout << "World" << std::endl;
  });
  // t1 and t2 might be running - they might have already terminated!
  t1.join();
  t2.join();
  // We know t1 and t2 have finished running
  return 0;
}
