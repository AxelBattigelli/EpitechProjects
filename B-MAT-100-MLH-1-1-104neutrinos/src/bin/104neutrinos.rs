use std::{io::Write, process::{ExitCode, Termination}};

fn read_line_from_stdin(line: &mut String) -> std::io::Result<()> {
    line.clear();
    std::io::stdin().read_line(line)?;
    Ok(())
}

fn arithmetic_mean(n: usize, ar_m: f64, new_v: f64) -> f64 {
    let somme = (0..n).map(|_| ar_m).sum::<f64>();
    let new = somme + new_v;
    let div = n + 1;
    new / (div as f64)
}

fn har_mean(n: usize, h: f64, new_v: f64) -> f64 {
    let inv = h.recip();
    let mul = inv * n as f64;
    let new = mul + new_v.recip();
    (n + 1) as f64 / new
}

fn std_dev_from_rms(a: f64, rms: f64) -> f64 {
    let a_square = a * a;
    let rms_square = rms * rms;
    (rms_square - a_square).sqrt()
}

fn root_mean_square(n: usize, rms: f64, new_v: f64) -> f64 {
    let square = rms * rms;
    let somme = square * n as f64;
    let new = somme + (new_v * new_v);
    let n = n + 1;
    (new / (n as f64)).sqrt()
}

fn rms_from_data(a: f64, sd: f64) -> f64 {
    let a_square = a * a;
    let sd_square = sd * sd;
    (a_square + sd_square).sqrt()
}

fn run() -> Result<(), ()> {
    let mut args = std::env::args();
    args.next();

    // n
    let Some(n) = args.next() else {
        return Err(());
    };
    let Ok(mut n) = n.trim().parse() else {
        return Err(());
    };

    // a
    let Some(a) = args.next() else {
        return Err(());
    };
    let Ok(mut a) = a.trim().parse() else {
        return Err(());
    };

    // h
    let Some(h) = args.next() else {
        return Err(());
    };
    let Ok(mut h) = h.trim().parse() else {
        return Err(());
    };

    // sd
    let Some(sd) = args.next() else {
        return Err(());
    };
    let Ok(sd) = sd.trim().parse() else {
        return Err(());
    };

    if args.next().is_some() {
        return Err(());
    }

    let mut line = String::new();
    let mut rms = rms_from_data(a, sd);

    loop {
        print!("Enter next value: ");
        std::io::stdout().flush().unwrap();
        read_line_from_stdin(&mut line).unwrap();

        if line.trim() == "END" {
            break;
        }

        let Ok(new_nbr) = line.trim().parse() else {
            return Err(());
        };

        a = arithmetic_mean(n, a, new_nbr);
        h = har_mean(n, h, new_nbr);
        rms = root_mean_square(n, rms, new_nbr);
        let sd = std_dev_from_rms(a, rms);
        n += 1;
        println!(
            "    Number of values:   {n}
    Standard deviation: {sd:.2}
    Arithmetic mean:    {a:.2}
    Root mean square:   {rms:.2}
    Harmonic mean:      {h:.2}
"
        );
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
