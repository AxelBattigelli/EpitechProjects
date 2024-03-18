use std::process::{ExitCode, Termination};

pub fn magic_function(k: f64, n: f64, i: usize) -> f64 {
    if i == 1 {
        return n;
    }
    let xi1 = magic_function(k, n, i - 1);
    k * xi1 * (1000.0 - xi1) / 1000.0
}

fn two_args(n: f64, k: f64) {
    for generation in 1..=100 {
        println!("{generation} {:.2}", magic_function(k, n, generation));
    }
}

fn three_args(n: f64, i0: usize, i1: usize) {
    for gr in (100..=400).map(|n| n as f64 / 100.0) {
        for gen in i0..=i1 {
            println!("{gr:.2} {:.2}", magic_function(gr, n, gen));
        }
    }
}

fn run() -> Result<(), ()> {
    let mut args = std::env::args();
    args.next();

    let Some(n) = args.next() else {
        return Err(());
    };
    if n == "-h" {
        println!(
            "USAGE
    ./106bombyx n [k | i0 i1]
DESCRIPTION
    n number of first generation individuals
    k growth rate from 1 to 4
    i0 initial generation (included)
    i1 final generation (included)
"
        );
        return Ok(());
    }
    let Ok(n) = n.trim().parse::<f64>() else {
        return Err(());
    };
    if n <= 0.0 {
        return Err(());
    }

    match args.len() {
        1 => {
            let Some(k) = args.next() else {
                return Err(());
            };
            let Ok(k) = k.trim().parse::<f64>() else {
                return Err(());
            };
            if !(1.0..=4.0).contains(&k) {
                return Err(());
            }
            two_args(n, k);
        }
        2 => {
            let Some(i0) = args.next() else {
                return Err(());
            };
            let Ok(i0) = i0.trim().parse::<usize>() else {
                return Err(());
            };
            if i0 == 0 {
                return Err(());
            }

            let Some(i1) = args.next() else {
                return Err(());
            };
            let Ok(i1) = i1.trim().parse::<usize>() else {
                return Err(());
            };
            if i1 == 0 {
                return Err(());
            }

            if i1 < i0 {
                return Err(());
            }

            three_args(n, i0, i1);
        }
        _ => return Err(()),
    }
    Ok(())
}

enum MyResult {
    Ok,
    Err,
}

impl Termination for MyResult {
    fn report(self) -> ExitCode {
        match self {
            MyResult::Ok => 0.into(),
            MyResult::Err => 84.into(),
        }
    }
}

fn main() -> MyResult {
    if let Ok(()) = run() {
        MyResult::Ok
    } else {
        println!("ERROR");
        MyResult::Err
    }
}
