use std::f64::consts::{FRAC_PI_2, PI};

use maths_101_pong::{Args, CliArgs, Vector3};

fn main() {
    let args = if let Ok(cliargs) = CliArgs::from_args(std::env::args().skip(1)) {
        Args::from(cliargs)
    } else {
        println!(
            r"USAGE
    ./101pong x0 y0 z0 x1 y1 z1 n

DESCRIPTION
    x0 ball abscissa at time t - 1
    y0 ball ordinate at time t - 1
    z0 ball altitude at time t - 1
    x1 ball abscissa at time t
    y1 ball ordinate at time t
    z1 ball altitude at time t
    n  time shift (greater than or equal to zero, integer)"
        );
        std::process::exit(84);
    };
    let velocity = args.p0.to(args.p1);
    println!("The velocity vector of the ball is:");
    println!("({:.2}, {:.2}, {:.2})", velocity.x, velocity.y, velocity.z);
    println!("At time t + {}, ball coordinates will be:", args.n);
    println!(
        "({:.2}, {:.2}, {:.2})",
        args.p1.x + velocity.x * args.n as f64,
        args.p1.y + velocity.y * args.n as f64,
        args.p1.z + velocity.z * args.n as f64
    );
    let angle_rad = velocity.angle(Vector3::new(0.0, 0.0, 1.0)).abs();
    let range = if args.p1.z < 0.0 {
        0.0..FRAC_PI_2
    } else {
        FRAC_PI_2..PI
    };
    if range.contains(&angle_rad) {
        println!(
            r"The incidence angle is:
{:.2} degrees",
            angle_rad.to_degrees() % 90.0
        );
    } else {
        println!("The ball won't reach the paddle.");
    }
}
