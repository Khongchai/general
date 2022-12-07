use core::panic;

use std::f64::consts::PI;

use std::time::Instant;

fn main() {
    let start = Instant::now();

    let results = calc_lines(100, 0.0, 0.0006283185307179586, 100.0);

    let duration = start.elapsed();

    println!("result length: {}", results.len());
    println!(
        "Sample results: {}, {}, {}, {}",
        results[0], results[1], results[99], results[1000]
    );
    println!("Time elapsed in calc_lines() is: {:?}", duration);
}

pub fn calc_lines(points: usize, mut theta: f64, step: f64, rod_length: f64) -> Vec<f64> {
    let mut arr: Vec<f64> = Vec::with_capacity(points * 4 - 4);

    let mut first_time = true;
    let mut prev_point: [f64; 2] = [0.0, 0.0];
    let mut new_point: [f64; 2] = [0.0, 0.0];

    let parsed_data: Vec<[f64; 4]> = vec![
        [300.0, 100.0, 0.0, 1.0],
        [100.0, 50.0, 1.0, 5.11],
        [50.0, 25.0, 1.0, 5.11],
        [25.0, 50.0, 0.0, 7.0],
    ];
    let parsed_data_len = parsed_data.len();
    if parsed_data_len < 2 {
        panic!("Provide at least 2 cycloids");
    }

    let parsed_data_crunched: Vec<[f64; 3]> = parsed_data
        .iter()
        .map(|a| [a[0] + a[1], PI * 0.5 * a[2], a[3]])
        .collect();

    let parsed_data_crunched_ptr: *const [f64; 3] = parsed_data_crunched.as_ptr();

    for _ in 0..points {
        compute_epitrochoid(
            parsed_data_crunched_ptr,
            parsed_data_len,
            theta,
            rod_length,
            &mut new_point,
        );

        if first_time {
            first_time = false;
        } else {
            arr.extend(
                [prev_point[0], prev_point[1], new_point[0], new_point[1]]
                    .iter()
                    .cloned(),
            );
        }

        prev_point = new_point;

        theta += step;
    }

    arr
}

pub fn compute_epitrochoid(
    data: *const [f64; 3],
    data_len: usize,
    theta: f64,
    rod_length: f64,
    new_point: &mut [f64; 2],
) {
    *new_point = [0.0, 0.0];
    unsafe {
        for i in 0..data_len {
            let d = *data.add(i);
            new_point[0] = new_point[0] + d[0] * (theta * d[2] - d[1]).cos();
            new_point[1] = new_point[1] + d[0] * (theta * d[2] + d[1]).sin();
        }

        new_point[0] += rod_length * theta.cos();
        new_point[1] += rod_length * theta.sin();
    }
}
