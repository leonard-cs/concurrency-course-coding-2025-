use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::Arc;
use std::sync::Mutex;
use std::thread;

fn main() {
    let max = 16777216;

    let mut data = Vec::<u32>::new();
    for _ in 0..max {
        data.push(1);
    }

    let data_mutex_arc_t1: Arc<Mutex<Vec<u32>>> = Arc::new(Mutex::new(data));
    let data_mutex_arc_t2: Arc<Mutex<Vec<u32>>> = data_mutex_arc_t1.clone();

    let result_arc_main: Arc<AtomicU32> = Arc::new(AtomicU32::new(0));
    let result_arc_t1: Arc<AtomicU32> = result_arc_main.clone();
    let result_arc_t2: Arc<AtomicU32> = result_arc_main.clone();

    let t1 = thread::spawn(move || {
        let mut my_result: u32 = 0;
        for i in 0..max / 2 {
            my_result += data_mutex_arc_t1.lock().unwrap()[i];
        }
        result_arc_t1.fetch_add(my_result, Ordering::Relaxed);
    });

    let t2 = thread::spawn(move || {
        let mut my_result: u32 = 0;
        for i in max / 2..max {
            my_result += data_mutex_arc_t2.lock().unwrap()[i];
        }
        result_arc_t2.fetch_add(my_result, Ordering::Relaxed);
    });

    t1.join().unwrap();
    t2.join().unwrap();

    println!("{}", result_arc_main.load(Ordering::Relaxed));
}
