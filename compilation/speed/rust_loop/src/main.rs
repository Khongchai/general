use std::time::Instant;

fn main() {
    let start = Instant::now();
    for _i in 0..100000000 {
        for _j in 0..20 {}
    }
    let duration = start.elapsed();
    println!("{:?}", duration)
}
