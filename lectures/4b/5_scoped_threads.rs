// Thanks to former student Zak Stasa for providing this code.

use std::thread;

fn main() {
    let max: usize = 1 << 24;
    // Rough number of splits
    let n = 10;
    // Create a vector of max number of ones
    let data = vec![1; max];

    assert!(n != 0, "Cannot split into 0 chunks");
    assert!(max / n > 0, "Size of each chunk cannot be empty");

    // Separate our data into chunks
    // Note that we may have more than n chunks depending on max and n
    let data_chunks: Vec<&[usize]> = data.chunks(max / n).collect();
    let num_threads = data_chunks.len();

    // Vector of partial results from each thread initialised to 0
    let mut results = vec![0; num_threads];

    // Using thread::scope avoids having to manually join the threads, and allows
    // us to have references to non-'static data, such as the data and results
    // vectors (i.e. Rust can guarantee the spawned threads will not access them
    // after they have been freed)
    thread::scope(|s| {
        // Use the slice [..] notation to make it type-check
        let mut rest = &mut results[..];

        for i in 0..num_threads {
            // Split the reference to the vector into 2 separate ones (one for
            // the thread to be spawned and for the future threads to be spawned)
            // This means each thread can mutate a unique index in the results
            // vector at the same time
            let (curr, rest_tmp) = rest.split_at_mut(1);
            rest = rest_tmp;

            // Shadow data_chunks by its reference to avoid moving the actual
            // vector in the closure below
            let data_chunks = &data_chunks;

            // Use move to move i
            s.spawn(move || {
                curr[0] = data_chunks[i].iter().sum::<usize>();
            });
        }
    });

    println!("{}", results.into_iter().sum::<usize>());
}
