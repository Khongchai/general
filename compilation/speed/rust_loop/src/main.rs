use std::time::Instant;

fn main() {
    let start = Instant::now();
    let mut foo = 0;
    for i in 0..100000000 {
        foo = i;
        for j in 0..20 {
            foo = j;
        }
    }
    let duration = start.elapsed();
    println!("{:?}, {}", duration, foo);
}
