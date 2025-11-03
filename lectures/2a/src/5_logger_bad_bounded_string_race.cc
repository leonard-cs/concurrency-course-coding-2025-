#include <iostream>
#include <mutex>
#include <sstream>
#include <string>
#include <thread>
#include <vector>

class Logger {
 public:
  explicit Logger(size_t max_size) : max_size_(max_size) {
    // Extra construction work would go here.
  }

  void LogMessage(const std::string& message) {
    if (log_.size() + message.size() > max_size_) {
      return;
    }
    // Multiple threads could get here, despite the fact that *cumulatively*
    // they would make the log too big!
    mutex_.lock();
    log_.append(message);
    mutex_.unlock();
  }

  const std::string& GetLogContents() const { return log_; }

 private:
  std::string log_;
  std::mutex mutex_;
  size_t max_size_;
};

static void ThreadBody(size_t id, std::vector<std::string>& favourite_food,
                       Logger& logger) {
  std::stringstream ss;
  ss << "Hello, my name is " << std::this_thread::get_id()
     << " but you can call me " << id << " and my favourite food is "
     << favourite_food[id] << std::endl;
  logger.LogMessage(ss.str());
}

int main() {
  std::vector<std::thread> threads;
  std::vector<std::string> favourite_foods = {
      "Pizza",          "Burger", "Ice cream",  "Chocolate cake",
      "Fish and chips", "Sushi",  "Bubble tea", "Jackfruit"};
  Logger logger(100);
  for (size_t i = 0; i < 8; ++i) {
    threads.push_back(std::thread(ThreadBody, i, std::ref(favourite_foods),
                                  std::ref(logger)));
  }
  // At this point, any number between 0 and 8 threads will be running.
  for (size_t i = 0; i < 8; ++i) {
    threads[i].join();
  }
  // All the threads have now terminated, because we joined them all.
  std::cout << logger.GetLogContents() << std::endl;
  std::cout << "Log size: " << logger.GetLogContents().size() << std::endl;
  return 0;
}
