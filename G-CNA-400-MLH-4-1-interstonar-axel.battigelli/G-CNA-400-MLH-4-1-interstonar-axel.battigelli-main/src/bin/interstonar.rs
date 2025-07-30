use std::process::{ExitCode, Termination};

use interstonar::{parse_global, parse_local, BodyGlobal, Global, Local, Vec3};

const G: f64 = 6.674e-11; // gravitational constant

fn is_collision_with_body(rock_position: &Vec3, body: &BodyGlobal) -> bool {
    let distance = (
        rock_position.x - body.position.x,
        rock_position.y - body.position.y,
        rock_position.z - body.position.z,
    );

    let distance_squared = distance.2.mul_add(
        distance.2,
        distance.0.mul_add(distance.0, distance.1.powi(2)),
    );

    let collision_distance = body.radius;
    distance_squared <= collision_distance.powi(2)
}

fn run_simulation_global(
    body: &BodyGlobal,
    bodies: &[BodyGlobal],
    rock: &BodyGlobal,
) -> BodyGlobal {
    let mut new_body = body.to_owned();

    new_body.position.x += new_body.direction.x * 3600.0;
    new_body.position.y += new_body.direction.y * 3600.0;
    new_body.position.z += new_body.direction.z * 3600.0;
    for other_body in bodies {
        if other_body.name == body.name {
            continue;
        }
        let distance = (
            other_body.position.x - body.position.x,
            other_body.position.y - body.position.y,
            other_body.position.z - body.position.z,
        );

        let distance_squared = distance.2.mul_add(
            distance.2,
            distance.0.mul_add(distance.0, distance.1 * distance.1),
        );
        let force = G * ((body.mass * other_body.mass) / distance_squared);
        let diff = force / body.mass;
        let distance_ = distance_squared.sqrt();
        new_body.direction.x += diff * (distance.0 / distance_) * 3600.0;
        new_body.direction.y += diff * (distance.1 / distance_) * 3600.0;
        new_body.direction.z += diff * (distance.2 / distance_) * 3600.0;
    }
    if rock.name != body.name {
        let distance = (
            rock.position.x - body.position.x,
            rock.position.y - body.position.y,
            rock.position.z - body.position.z,
        );

        let distance_squared = distance.2.mul_add(
            distance.2,
            distance.0.mul_add(distance.0, distance.1 * distance.1),
        );
        let force = G * ((body.mass * rock.mass) / distance_squared);
        let diff = force / body.mass;
        let distance_ = distance_squared.sqrt();
        new_body.direction.x += diff * (distance.0 / distance_) * 3600.0;
        new_body.direction.y += diff * (distance.1 / distance_) * 3600.0;
        new_body.direction.z += diff * (distance.2 / distance_) * 3600.0;
    }

    new_body // return
}

fn global_part(
    mut data: Global,
    px: f64,
    py: f64,
    pz: f64,
    vx: f64,
    vy: f64,
    vz: f64,
) -> Result<(), ()> {
    if false {
        return Err(());
    }
    let mut rock = BodyGlobal {
        name: "Rock".to_string(),
        position: Vec3 {
            x: px,
            y: py,
            z: pz,
        },
        direction: Vec3 {
            x: vx,
            y: vy,
            z: vz,
        },
        mass: 1.0,
        radius: f64::EPSILON,
        goal: false,
    };

    for t in 1..=(365 * 24) {
        rock = run_simulation_global(&rock, &data.bodies, &rock);
        println!(
            "At time t = {}: rock is ({:.3}, {:.3}, {:.3})",
            t, rock.position.x, rock.position.y, rock.position.z
        );
        let mut new_bodies: Vec<BodyGlobal> = vec![];
        for body in &data.bodies {
            if is_collision_with_body(&rock.position, body) {
                println!("Collision between rock and {}", body.name);
                if body.goal {
                    println!("Mission success");
                } else {
                    println!("Mission failure");
                }
                return Ok(());
            }
            new_bodies.push(run_simulation_global(body, &data.bodies, &rock));
        }

        data.bodies.clear();
        
        'outer: for body_left in &new_bodies {
            for body_right in &data.bodies.clone() {
                let distance_squared = (body_right.position - body_left.position).abs().norm();
                
                if distance_squared < (body_left.radius + body_right.radius) {
                    let total_mass = body_left.mass + body_right.mass;
                    // create new
                    data.bodies.push(BodyGlobal {
                        name: if body_left.name < body_right.name {
                            format!("{}{}", body_left.name, body_right.name)
                        } else {
                            format!("{}{}", body_right.name, body_left.name)
                        },
                        position: Vec3 {
                            x:(body_left.position.x + body_right.position.x) / 2.0,
                            y:(body_left.position.y + body_right.position.y) / 2.0,
                            z:(body_left.position.z + body_right.position.z) / 2.0,
                        },
                        direction: body_left.direction / total_mass * body_left.mass + body_right.direction / total_mass * body_right.mass,
                        mass: body_left.mass + body_right.mass,
                        radius: {
                            let left_volume = (std::f64::consts::FRAC_PI_3 * 4.0) * body_left.radius.powi(3);
                            let right_volume =(std::f64::consts::FRAC_PI_3 * 4.0) * body_right.radius.powi(3);
                            ((left_volume + right_volume) / (std::f64::consts::FRAC_PI_3 * 4.0)).cbrt()
                        },
                        goal: body_left.goal || body_right.goal,
                    });
                    data.bodies.retain(|v| v.name != body_right.name);
                    continue 'outer;
                    // new_bodies.push(run_simulation_global(body, &data.bodies, &rock));
                }
            }
            data.bodies.push(body_left.clone());
        }
        
        data.bodies = new_bodies;
    }

    println!("Mission failure");
    Ok(())
}

fn local_part(data: Local, px: f64, py: f64, pz: f64, vx: f64, vy: f64, vz: f64) -> Result<(), ()> {
    if false {
        return Err(());
    }

    let mut rock_position = Vec3 {
        x: px,
        y: py,
        z: pz,
    };
    let rock_direction = Vec3 {
        x: vx,
        y: vy,
        z: vz,
    };

    println!(
        "Rock thrown at the point ({px:.2}, {py:.2}, {pz:.2}) and parallel to the vector ({vx:.2}, {vy:.2}, {vz:.2})"
    );
    for body in &data.bodies {
        println!("{body}");
    }
    println!();
    for t in 1..=1000 {
        let mut min_sdf = f64::MAX;
        for body in &data.bodies {
            let sdf = body.distance(rock_position);
            min_sdf = min_sdf.min(sdf);
        }
        rock_position += rock_direction / rock_direction.norm() * min_sdf;
        println!(
            "Step {}: ({:.2}, {:.2}, {:.2})",
            t, rock_position.x, rock_position.y, rock_position.z
        );
        if min_sdf <= 0.1 {
            println!("Result: Intersection");
            return Ok(());
        }
        if min_sdf > 1000.0 {
            println!("Result: Out of scene");
            return Ok(());
        }
    }
    println!("Result: Time out");
    return Ok(());
}

fn run() -> Result<(), ()> {
    let mut args = std::env::args();
    args.next();

    let Some(select) = args.next() else {
        return Err(());
    };
    if select == "-h" || select == "--help" {
        println!(
            r"USAGE
    ./interstonar [--global | --local] CONFIG_FILE Px Py Pz Vx Vy Vz
DESCRIPTION
    --global Launch program in global scene mode. The CONFIG_FILE will describe a scene
containing massive spherical moving bodies of which at least one is a goal.
    --local Launch program in local scene mode. The CONFIG_FILE will describe a scene
containing massless motionless shapes.
    Pi Position coordinates of the rock
    Vi Velocity vector of the rock
    CONFIG_FILE TOML configuration file describing a scene"
        );
        return Ok(());
    }

    let Some(conf_file) = args.next() else {
        return Err(());
    };
    let Ok(px) = args.next().ok_or(())?.parse::<f64>() else {
        return Err(());
    };
    let Ok(py) = args.next().ok_or(())?.parse::<f64>() else {
        return Err(());
    };
    let Ok(pz) = args.next().ok_or(())?.parse::<f64>() else {
        return Err(());
    };
    let Ok(vx) = args.next().ok_or(())?.parse::<f64>() else {
        return Err(());
    };
    let Ok(vy) = args.next().ok_or(())?.parse::<f64>() else {
        return Err(());
    };
    let Ok(vz) = args.next().ok_or(())?.parse::<f64>() else {
        return Err(());
    };
    if args.next().is_some() {
        return Err(());
    }

    if select == "--global" {
        let data = parse_global(conf_file)?;
        return global_part(data, px, py, pz, vx, vy, vz);
    } else if select == "--local" {
        let data = parse_local(conf_file)?;
        return local_part(data, px, py, pz, vx, vy, vz);
    }

    Err(())
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
