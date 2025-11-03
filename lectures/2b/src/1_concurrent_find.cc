#include <atomic>
#include <functional>
#include <iostream>
#include <thread>
#include <vector>

template <typename T>
size_t Find(const std::vector<T>& data, std::function<bool(T)> predicate) {
  std::atomic<size_t> result(std::numeric_limits<size_t>::max());

  const size_t chunk_size = data.size() / 2;
  auto thread_body = [chunk_size, &predicate, &data,
                      &result](size_t id) -> void {
    // A thread will loop through a segment of the vector according to its ID.
    for (size_t index = id * chunk_size; index < (id + 1) * chunk_size;
         index++) {
      if (predicate(data[index])) {
        result.store(index);
        return;
      }
    }
  };

  auto t1 = std::thread(thread_body, 0);
  auto t2 = std::thread(thread_body, 1);

  t1.join();
  t2.join();
  return result.load();
}

int main() {
  std::vector<int> data;
  for (size_t count = 0; count < 2; count++) {
    for (int i = 0; i < (1 << 24); i++) {
      data.push_back(i);
    }
  }
  size_t result =
      Find<int>(data, [](int item) -> bool { return item == 16000000; });

  if (result != std::numeric_limits<size_t>::max()) {
    std::cout << "Found " << data[result] << " at index " << result
              << std::endl;
  } else {
    std::cout << "Not found" << std::endl;
  }
  return 0;
}
