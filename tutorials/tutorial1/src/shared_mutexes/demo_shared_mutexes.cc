#include <chrono>
#include <iostream>

int main() {
  auto begin_time = std::chrono::high_resolution_clock::now();

  // The code you wish to benchmark.

  auto end_time = std::chrono::high_resolution_clock::now();
  auto duration = end_time - begin_time;
  auto millis =
      std::chrono::duration_cast<std::chrono::milliseconds>(duration).count();
  std::cout << "Duration: " << millis << " ms" << std::endl;
  return 0;
}
