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

    let result_mutex_arc_main = Arc::new(Mutex::new(0));
    let result_mutex_arc_t1 = result_mutex_arc_main.clone();
    let result_mutex_arc_t2 = result_mutex_arc_main.clone();

    let t1 = thread::spawn(move || {
        let mut my_result: u32 = 0;
        for i in 0..max / 2 {
            my_result += data_mutex_arc_t1.lock().unwrap()[i];
        }
        *result_mutex_arc_t1.lock().unwrap() += my_result;
    });

    let t2 = thread::spawn(move || {
        let mut my_result: u32 = 0;
        for i in max / 2..max {
            my_result += data_mutex_arc_t2.lock().unwrap()[i];
        }
        *result_mutex_arc_t2.lock().unwrap() += my_result;
    });

    t1.join().unwrap();
    t2.join().unwrap();

    println!("{}", result_mutex_arc_main.lock().unwrap());
}
