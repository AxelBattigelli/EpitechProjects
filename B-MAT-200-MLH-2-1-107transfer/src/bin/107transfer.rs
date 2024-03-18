use std::process::{ExitCode, Termination};

#[derive(Debug)]
struct Term {
    coeff: Vec<f64>,
}

impl Term {
    #[must_use]
    fn calculate(&self, x: f64) -> f64 {
        self.coeff
            .iter()
            .enumerate()
            .map(|(n, &c)| c * (x.powi(n as i32)))
            .sum()
    }

    fn from_arg(arg: &str) -> Result<Self, ()> {
        Ok(Self {
            coeff: arg
                .split('*')
                .map(|s| s.trim().parse().map_err(|_| ()))
                .collect::<Result<Vec<_>, _>>()?,
        })
    }
}

#[derive(Debug)]
struct Program {
    terms: Vec<Term>,
}

impl Program {
    fn calculate(&self, x: f64) -> Result<f64, ()> {
        self.terms
            .chunks(2)
            .map(|v| {
                Ok(v[0].calculate(x)
                    / v.get(1)
                        .map(|t| t.calculate(x))
                        .filter(|&c| c != 0.0)
                        .ok_or(())?)
            })
            .product()
    }

    fn from_args<S: AsRef<str>>(args: impl IntoIterator<Item = S>) -> Result<Self, ()> {
        Ok(Self {
            terms: args
                .into_iter()
                .map(|s| Term::from_arg(s.as_ref()))
                .collect::<Result<Vec<_>, _>>()?,
        })
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
    ./107transfer [num den]+
DESCRIPTION
    num    polynomial numerator defined by its coefficients
    den    polynomial denominator defined by its coefficients
"
        );
        return Ok(());
    }
    let prog = Program::from_args(std::iter::once(n).chain(args))?;

    for x in (0..=1000).map(|x| (x as f64) / 1000.0) {
        println!("{x:.3} -> {:.5}", prog.calculate(x)?);
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
            Self::Ok => 0.into(),
            Self::Err => 84.into(),
        }
    }
}

fn main() -> MyResult {
    if run() == Ok(()) {
        MyResult::Ok
    } else {
        println!("ERROR");
        MyResult::Err
    }
}
