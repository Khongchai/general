use core::panic;

use std::f64::consts::PI;

use std::time::Instant;

fn main() {
    let start = Instant::now();

    let result = calc_lines(1000000, 0.0, 0.0006283185307179586, 100.0);

    let duration = start.elapsed();

    println!("result length: {}", result.len());
    println!("Time elapsed in calc_lines() is: {:?}", duration);
}

pub fn calc_lines(points: usize, mut theta: f64, step: f64, rod_length: f64) -> Vec<f64> {
    let mut arr: Vec<f64> = Vec::with_capacity(points * 4 - 4);

    let mut first_time = true;
    let mut prev_point: [f64; 2] = [0.0, 0.0];
    let mut new_point: [f64; 2] = [0.0, 0.0];

    // Use vec because in the actual code (where this came from),
    // we do not know the size of the outer array at compile time.
    let parsed_data: *const [f64; 4] = vec![
        [300.0, 100.0, 0.0, 1.0],
        [100.0, 50.0, 1.0, 5.11],
        [50.0, 25.0, 1.0, 5.11],
        [25.0, 50.0, 0.0, 7.0],
    ].as_ptr();
    let inner_data_size: usize = 4;

    if inner_data_size < 2 {
        panic!("Provide at least 2 cycloids");
    }

    for _ in 0..points {
        compute_epitrochoid(parsed_data, theta, rod_length, &mut new_point, inner_data_size);

        if first_time {
            first_time = false;
        } else {
            arr.push(prev_point[0]);
            arr.push(prev_point[1]);
            arr.push(new_point[0]);
            arr.push(new_point[1]);
        }

        prev_point = new_point.clone();

        theta += step;
    }

    arr
}

pub fn compute_epitrochoid(
    data: *const [f64; 4],
    theta: f64,
    rod_length: f64,
    new_point: &mut [f64; 2],
    inner_data_size: usize,
) {
    unsafe {
        for i in 0..inner_data_size {
            let d = data.add(i).as_ref().unwrap();
            new_point[0] += (d[0] + d[1]) * (theta * d[3] - PI * 0.5 * d[2]).cos();
            new_point[1] += (d[0] + d[1]) * (theta * d[3] + PI * 0.5 * d[2]).sin();
        }

        new_point[0] += rod_length * theta.cos();
        new_point[1] += rod_length * theta.sin();

    }
}
