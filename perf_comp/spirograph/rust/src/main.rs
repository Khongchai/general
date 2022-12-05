use core::panic;

use std::f64::consts::PI;

use std::time::Instant;

fn main() {
    let start = Instant::now();

    calc_lines(1000001, 0.0, 0.0006283185307179586, 100.0);

    let duration = start.elapsed();

    println!("Time elapsed in calc_lines() is: {:?}", duration);
}

pub fn calc_lines(points: usize, mut theta: f64, step: f64, rod_length: f64) -> Vec<f64> {
    let mut arr: Vec<f64> = Vec::with_capacity(points * 4);
    let mut first_time = true;
    let mut prev_point: [f64; 2] = [0.0, 0.0];
    let mut new_point: [f64; 2] = [0.0, 0.0];

    let parsed_data: Vec<Vec<f64>> = vec![
        vec![300.0, 100.0, 0.0, 1.0],
        vec![100.0, 50.0, 1.0, 5.11],
        vec![50.0, 25.0, 1.0, 5.11],
        vec![25.0, 50.0, 0.0, 7.0],
    ];

    for _ in 0..points {
        compute_epitrochoid(&parsed_data, theta, rod_length, &mut new_point);

        if first_time {
            first_time = false;
        } else {
            arr.push(prev_point[0]);
            arr.push(prev_point[1]);
            arr.push(new_point[0]);
            arr.push(new_point[1]);
        }

        prev_point = new_point;

        theta += step;
    }

    arr
}

pub fn compute_epitrochoid(
    data: &Vec<Vec<f64>>,
    theta: f64,
    rod_length: f64,
    new_point: &mut [f64; 2],
) {
    if data.len() < 2 {
        panic!("Provide at least 2 cycloids");
    }

    for current_data in data {
        new_point[0] = (current_data[0] + current_data[1])
            * (theta * current_data[3] - PI * 0.5 * current_data[2]).cos();
        new_point[1] = (current_data[0] + current_data[1])
            * (theta * current_data[3] + PI * 0.5 * current_data[2]).sin();
    }

    new_point[0] = new_point[0] + rod_length * theta.cos();
    new_point[1] = new_point[1] + rod_length * theta.sin();
}
