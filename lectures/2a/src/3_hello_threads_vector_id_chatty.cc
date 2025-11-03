#include <iostream>
#include <string>
#include <thread>
#include <vector>

static void ThreadBody(size_t id, std::vector<std::string>& favourite_food) {
  std::cout << "Hello, my name is " << std::this_thread::get_id()
            << " but you can call me " << id << " and my favourite food is "
            << favourite_food[id] << std::endl;
}

int main() {
  std::vector<std::thread> threads;
  std::vector<std::string> favourite_foods = {
      "Pizza",          "Burger", "Ice cream",  "Chocolate cake",
      "Fish and chips", "Sushi",  "Bubble tea", "Jackfruit"};
  for (size_t i = 0; i < 8; ++i) {
    threads.push_back(std::thread(ThreadBody, i, std::ref(favourite_foods)));
  }
  // At this point, any number between 0 and 8 threads will be running.
  for (size_t i = 0; i < 8; ++i) {
    threads[i].join();
  }
  // All the threads have now terminated, because we joined them all.
  return 0;
}
