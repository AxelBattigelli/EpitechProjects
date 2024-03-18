use maths_102_architect::Matrix;

#[derive(Debug)]
struct CliArgs {
    x: f64,
    y: f64,
    transfos: Vec<(char, f64, Option<f64>)>,
}

#[derive(Debug)]
struct Args {
    source: Matrix<3, 1>,
    end_transfo: Matrix<3, 3>,
}

impl CliArgs {
    fn from_args<S: AsRef<str>, I: ExactSizeIterator + Iterator<Item = S>>(
        arg: impl IntoIterator<Item = S, IntoIter = I>,
    ) -> Result<Self, String> {
        let mut args = arg.into_iter();
        let x = args
            .next()
            .ok_or_else(|| "missing x".to_string())?
            .as_ref()
            .parse::<f64>()
            .map_err(|e| format!("invalid x: {}", e))?;
        let y = args
            .next()
            .ok_or_else(|| "missing y".to_string())?
            .as_ref()
            .parse::<f64>()
            .map_err(|e| format!("invalid y: {}", e))?;
        let mut transfos = Vec::new();
        while args.len() > 0 {
            let transfo = args
                .next()
                .map(|tr| match tr.as_ref() {
                    "-t" | "-z" | "-r" | "-s" => Ok(tr.as_ref().chars().nth(1).unwrap()),
                    other => Err(format!("invalid transformation: {}", other)),
                })
                .unwrap_or(Err("Missing transformation".to_string()))?;
            let arg1 = args
                .next()
                .ok_or_else(|| "missing arg1".to_string())?
                .as_ref()
                .parse::<f64>()
                .map_err(|e| format!("invalid arg1: {}", e))?;
            let arg2 = match transfo {
                't' | 'z' => Some(
                    args.next()
                        .ok_or_else(|| "missing arg2".to_string())?
                        .as_ref()
                        .parse::<f64>()
                        .map_err(|e| format!("invalid arg2: {}", e))?,
                ),
                _ => None,
            };
            transfos.push((transfo, arg1, arg2));
        }
        if args.next().is_some() {
            return Err("too many arguments".to_string());
        }
        if transfos.is_empty() {
            return Err("missing transformation".to_string());
        }

        Ok(Self { x, y, transfos })
    }
}

impl From<CliArgs> for Args {
    fn from(cliargs: CliArgs) -> Self {
        let source = Matrix::from_2d_coordinates(cliargs.x, cliargs.y);
        let mut end_transfo = Matrix::identity();
        for (transfo, arg1, arg2) in cliargs.transfos {
            let transfo = match transfo {
                't' => {
                    println!("Translation along vector ({}, {})", arg1, arg2.unwrap());
                    Matrix::translation(arg1, arg2.unwrap())
                }
                'z' => {
                    println!("Scaling by factors {} and {}", arg1, arg2.unwrap());
                    Matrix::scaling(arg1, arg2.unwrap())
                }
                'r' => {
                    println!("Rotation by a {} degree angle", arg1);
                    Matrix::rotation(arg1.to_radians())
                }
                's' => {
                    println!(
                        "Reflection over an axis with an inclination angle of {} degrees",
                        arg1
                    );
                    Matrix::reflection(arg1.to_radians())
                }
                _ => unreachable!(),
            };
            end_transfo = transfo.mat_mul(end_transfo);
        }
        Self {
            source,
            end_transfo,
        }
    }
}

fn main() {
    let args = if let Ok(cliargs) = CliArgs::from_args(std::env::args().skip(1)) {
        Args::from(cliargs)
    } else {
        println!(
            r"USAGE
    ./102architect x y transfo1 arg11 [arg12] [transfo2 arg21 [arg22]] ...
    
DESCRIPTION
    x abscissa of the original point
    y ordinate of the original point

    transfo arg1 [arg2]
    -t i j translation along vector (i, j)
    -z m n scaling by factors m (x-axis) and n (y-axis)
    -r d   rotation centered in O by a d degree angle
    -s d   reflection over the axis passing through O with an inclination
           angle of d degrees"
        );
        std::process::exit(84);
    };
    println!("{:.2}", args.end_transfo);
    let result = args.end_transfo.mat_mul(args.source);
    println!("{:#.2} => {:#.2}", args.source, result);
}
