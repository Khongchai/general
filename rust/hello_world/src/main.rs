// An overly-complicated hello world :p

fn main() {
    println!("{}", hello_world("Hello", "world"));
}

// returns a String or &str?
fn hello_world(greeting: &str, object: &str) -> String {
    // Using tuples
    let _x: (i32, &str) = (1, greeting);
    let _y: (i32, &str) = (2, object);

    return format!("{greeting}, {object}", greeting = _x.1, object = _y.1);
}
