use serde::Deserialize;
use std::{
    fmt,
    ops::{Add, AddAssign, Div, Mul, Sub},
    string::String,
};

#[derive(Deserialize)]
pub struct Global {
    pub bodies: Vec<BodyGlobal>,
}

#[derive(Deserialize)]
pub struct Local {
    pub bodies: Vec<BodyLocal>,
}

#[derive(Deserialize, Clone, Copy)]
pub struct Vec2 {
    pub x: f64,
    pub y: f64,
}

impl Sub for Vec2 {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
        }
    }
}

impl Vec2 {
    #[must_use]
    pub fn norm(self) -> f64 {
        self.x.hypot(self.y)
    }

    #[must_use]
    pub fn abs(self) -> Self {
        Self {
            x: self.x.abs(),
            y: self.y.abs(),
        }
    }

    #[must_use]
    pub fn max(self, rhs: Self) -> Self {
        Self {
            x: self.x.max(rhs.x),
            y: self.y.max(rhs.y),
        }
    }
}

#[derive(Deserialize, Clone, Copy)]
pub struct Vec3 {
    pub x: f64,
    pub y: f64,
    pub z: f64,
}

impl Vec3 {
    #[must_use]
    pub fn norm(self) -> f64 {
        (self.x * self.x + self.y * self.y + self.z * self.z).sqrt()
    }

    #[must_use]
    pub fn norm_squared(self) -> f64 {
        self.x * self.x + self.y * self.y + self.z * self.z
    }

    #[must_use]
    pub fn abs(self) -> Self {
        Self {
            x: self.x.abs(),
            y: self.y.abs(),
            z: self.z.abs(),
        }
    }

    #[must_use]
    pub fn max(self, rhs: Self) -> Self {
        Self {
            x: self.x.max(rhs.x),
            y: self.y.max(rhs.y),
            z: self.z.max(rhs.z),
        }
    }
}

impl Sub for Vec3 {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
            z: self.z - rhs.z,
        }
    }
}

impl Div<f64> for Vec3 {
    type Output = Self;

    fn div(self, rhs: f64) -> Self::Output {
        Self {
            x: self.x / rhs,
            y: self.y / rhs,
            z: self.z / rhs,
        }
    }
}

impl Mul<f64> for Vec3 {
    type Output = Self;

    fn mul(self, rhs: f64) -> Self::Output {
        Self {
            x: self.x * rhs,
            y: self.y * rhs,
            z: self.z * rhs,
        }
    }
}

impl AddAssign for Vec3 {
    fn add_assign(&mut self, rhs: Self) {
        self.x += rhs.x;
        self.y += rhs.y;
        self.z += rhs.z;
    }
}

impl Add for Vec3 {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
            z: self.z + rhs.z,
        }
    }
}

const fn default_infinity() -> f64 {
    f64::INFINITY
}

#[derive(Deserialize, Clone)]
pub struct BodyGlobal {
    pub name: String,
    pub position: Vec3,
    pub direction: Vec3,
    pub mass: f64,
    pub radius: f64,
    #[serde(default)]
    pub goal: bool,
}

#[derive(Deserialize)]
pub struct Sphere {
    pub radius: f64,
}

impl Sphere {
    #[must_use]
    pub fn distance(&self, pos: Vec3, point: Vec3) -> f64 {
        (pos - point).norm() - self.radius
    }
}

impl fmt::Display for Sphere {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Sphere of radius {:.2}", self.radius)
    }
}

#[derive(Deserialize)]
pub struct Cylinder {
    pub radius: f64,
    #[serde(default = "default_infinity")]
    pub height: f64,
}

impl Cylinder {
    #[must_use]
    pub fn distance(&self, pos: Vec3, point: Vec3) -> f64 {
        let p = point - pos;
        let (a, b, c) = (p.x, p.y, p.z);
        if self.height.is_infinite() {
            Vec2 { x: a, y: b }.norm() - self.radius
        } else {
            let d = Vec2 {
                x: Vec2 { x: a, y: b }.norm(),
                y: c,
            }
            .abs()
                - Vec2 {
                    x: self.radius,
                    y: self.height / 2.0,
                };
            d.x.max(d.y).min(0.0) + d.max(Vec2 { x: 0.0, y: 0.0 }).norm()
        }
    }
}

impl fmt::Display for Cylinder {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Cylinder of radius {:.2} and {}",
            self.radius, if self.height.is_infinite() {
                "infinite height".to_string()
            } else {
                format!("height {:.2}", self.height)
            }
        )
    }
}

#[derive(Deserialize)]
pub struct Bokx {
    pub sides: Vec3,
}

impl Bokx {
    #[must_use]
    pub fn distance(&self, pos: Vec3, point: Vec3) -> f64 {
        let p = point - pos;
        let d = p.abs() - self.sides / 2.0;
        d.max(Vec3 {
            x: 0.0,
            y: 0.0,
            z: 0.0,
        })
        .norm()
            + d.y.max(d.z).max(d.x).min(0.0)
    }
}

impl fmt::Display for Bokx {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Box of dimensions ({:.2}, {:.2}, {:.2})",
            self.sides.x, self.sides.y, self.sides.z
        )
    }
}
#[derive(Deserialize)]
pub struct Torus {
    pub inner_radius: f64,
    pub outer_radius: f64,
}

impl Torus {
    #[must_use]
    pub fn distance(&self, pos: Vec3, point: Vec3) -> f64 {
        let p = point - pos;
        let q = Vec2 {
            x: Vec2 { x: p.x, y: p.y }.norm() - self.inner_radius,
            y: p.z,
        };
        q.norm() - self.outer_radius
    }
}

impl fmt::Display for Torus {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Torus of inner radius {:.2} and outer radius {:.2}",
            self.inner_radius, self.outer_radius
        )
    }
}

#[derive(Deserialize)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum Shape {
    Sphere(Sphere),
    Cylinder(Cylinder),
    #[serde(rename = "box")]
    Bokx(Bokx),
    Torus(Torus),
}

impl fmt::Display for Shape {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Sphere(i) => fmt::Display::fmt(i, f),
            Self::Cylinder(i) => fmt::Display::fmt(i, f),
            Self::Bokx(i) => fmt::Display::fmt(i, f),
            Self::Torus(i) => fmt::Display::fmt(i, f),
        }
    }
}

impl Shape {
    #[must_use]
    pub fn distance(&self, pos: Vec3, point: Vec3) -> f64 {
        match self {
            Self::Sphere(sp) => sp.distance(pos, point),
            Self::Cylinder(cy) => cy.distance(pos, point),
            Self::Bokx(bo) => bo.distance(pos, point),
            Self::Torus(to) => to.distance(pos, point),
        }
    }
}

#[derive(Deserialize)]
pub struct BodyLocal {
    pub position: Vec3,
    #[serde(flatten)]
    pub shape: Shape,
}

impl fmt::Display for BodyLocal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} at position ({:.2}, {:.2}, {:.2})",
            &self.shape, self.position.x, self.position.y, self.position.z
        )
    }
}

impl BodyLocal {
    #[must_use]
    pub fn distance(&self, point: Vec3) -> f64 {
        self.shape.distance(self.position, point)
    }
}

pub fn parse_global(conf_file: String) -> Result<Global, ()> {
    let global: Global =
        toml::from_str(&std::fs::read_to_string(conf_file).map_err(|_| ())?).map_err(|_| ())?;
    if global.bodies.is_empty() {
        return Err(());
    }
    if global.bodies.iter().map(|v| v.name.clone()).collect::<std::collections::BTreeSet<_>>().len() != global.bodies.len() {
        return Err(());
    }
    Ok(global)
}

pub fn parse_local(conf_file: String) -> Result<Local, ()> {
    let local: Local =
        toml::from_str(&std::fs::read_to_string(conf_file).map_err(|_| ())?).map_err(|_| ())?;
    if local.bodies.is_empty() {
        return Err(());
    }
    Ok(local)
}
