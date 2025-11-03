
#include <iostream>
#include <random>
#include <thread>
#include <vector>

#include "src/repeatedblur/sense_reversing_barrier.h"
#include "src/repeatedblur/sense_reversing_barrier_no_tls.h"

static void RepeatedBlurBarrierNoTls(std::vector<double>& data,
                                     size_t num_blur_iterations,
                                     size_t num_threads) {
  // Avoid warning about parameters not being used; remove once you
  // flesh out your solution.
  (void)data;
  (void)num_blur_iterations;
  (void)num_threads;
  // TODO
}

static void RepeatedBlurBarrier(std::vector<double>& data,
                                size_t num_blur_iterations,
                                size_t num_threads) {
  // Avoid warning about parameters not being used; remove once you
  // flesh out your solution.
  (void)data;
  (void)num_blur_iterations;
  (void)num_threads;
  // TODO
}

static void RepeatedBlurForkJoin(std::vector<double>& data,
                                 size_t num_blur_iterations,
                                 size_t num_threads) {
  // Avoid warning about parameters not being used; remove once you
  // flesh out your solution.
  (void)data;
  (void)num_blur_iterations;
  (void)num_threads;
  // TODO
}

static void RepeatedBlurSequential(std::vector<double>& data,
                                   size_t num_blur_iterations) {
  std::vector<double> temporary_buffer(data.size());
  temporary_buffer.resize(data.size());
  temporary_buffer[0] = data[0];
  temporary_buffer[data.size() - 1] = data[data.size() - 1];

  std::vector<double>* current = &data;
  std::vector<double>* next = &temporary_buffer;

  for (size_t blur_iteration = 0; blur_iteration < num_blur_iterations;
       blur_iteration++) {
    for (size_t index = 1; index < data.size() - 1; index++) {
      (*next)[index] =
          ((*current)[index - 1] + (*current)[index] + (*current)[index + 1]) /
          3.0;
    }
    auto temp = current;
    current = next;
    next = temp;
  }

  // If the final data has ended up in |temporary_buffer|, move it to |data|.
  if ((num_blur_iterations % 2) != 0) {
    data = std::move(temporary_buffer);
  }
}

int main(int argc, char** argv) {
  if (argc != 5) {
    std::cout << "Usage: " << argv[0]
              << R"( execution_kind num_threads num_blur_iterations vector_size
  execution_kind values:
     sequential (num_threads is then ignored)
     fork_join
     barrier
     barrier_no_tls
)";
    return 1;
  }

  std::string execution_kind(argv[1]);
  size_t num_threads = std::stoul(std::string(argv[2]));
  size_t num_blur_iterations = std::stoul(std::string(argv[3]));
  size_t vector_size = std::stoul(std::string(argv[4]));

  if (execution_kind != "sequential" && (vector_size % num_threads) != 0) {
    std::cerr << "Number of threads must divide vector size" << std::endl;
    return 1;
  }

  std::vector<double> data;
  data.reserve(vector_size);
  std::random_device device;
  std::mt19937 generator(device());
  std::uniform_int_distribution<size_t> distribution(0, 100);

  // The data array is populated with values whose average is likely to be
  // very close to 50.
  data.push_back(50.0);
  for (size_t i = 1; i < vector_size - 1; i++) {
    data.push_back(static_cast<double>(distribution(generator)));
  }
  data.push_back(50.0);

  auto begin_time = std::chrono::high_resolution_clock::now();
  if (execution_kind == "sequential") {
    RepeatedBlurSequential(data, num_blur_iterations);
  } else if (execution_kind == "fork_join") {
    RepeatedBlurForkJoin(data, num_blur_iterations, num_threads);
  } else if (execution_kind == "barrier") {
    RepeatedBlurBarrier(data, num_blur_iterations, num_threads);
  } else if (execution_kind == "barrier_no_tls") {
    RepeatedBlurBarrierNoTls(data, num_blur_iterations, num_threads);
  }
  auto end_time = std::chrono::high_resolution_clock::now();

  for (size_t i = 1; i < vector_size - 1; i++) {
    if (data[i] < 47.0 || data[i] > 53.0) {
      std::cerr << "Found value too far from average: " << data[i] << std::endl;
      return 1;
    }
  }
  std::cout << "All values close to the average" << std::endl;
  auto duration = end_time - begin_time;
  auto millis =
      std::chrono::duration_cast<std::chrono::milliseconds>(duration).count();
  std::cout << millis << " ms" << std::endl;
  return 0;
}
