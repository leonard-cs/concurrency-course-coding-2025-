use std::thread;

fn main() {
    let max = 16777216;

    let mut data = Vec::<u32>::new();
    for _ in 0..max {
        data.push(1);
    }

    let mut result: u32 = 0;

    let t1 = thread::spawn(|| {
        let mut my_result: u32 = 0;
        for i in 0..max / 2 {
            my_result += data[i];
        }
        result += my_result;
    });

    let t2 = thread::spawn(|| {
        let mut my_result: u32 = 0;
        for i in max / 2..max {
            my_result += data[i];
        }
        result += my_result;
    });

    t1.join().unwrap();
    t2.join().unwrap();

    println!("{}", result);
}
