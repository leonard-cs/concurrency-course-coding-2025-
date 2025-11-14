use std::thread;

fn main() {
    let t1 = thread::spawn(|| {
        println!("I am thread 1");
    });

    let t2 = thread::spawn(|| {
        println!("I am thread 2");
    });

    t1.join().unwrap();
    t2.join().unwrap();
}
