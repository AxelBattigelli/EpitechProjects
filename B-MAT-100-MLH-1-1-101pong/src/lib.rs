/// Contains the CLI args
#[derive(Debug, Clone)]
pub struct CliArgs {
    /// ball abscissa at time t - 1
    x0: f64,
    /// ball ordinate at time t - 1
    y0: f64,
    /// ball altitude at time t - 1
    z0: f64,
    /// ball abscissa at time t
    x1: f64,
    /// ball ordinate at time t
    y1: f64,
    /// ball altitude at time t
    z1: f64,
    /// time shift (greater than or equal to zero, integer)
    n: usize,
}

impl CliArgs {
    /// Parses the CLI args
    ///
    /// # Errors
    ///
    /// Returns an error if the number of args does not equal 7 or if the args are not valid numbers
    ///
    /// # Examples
    ///
    /// ```
    /// use maths_101_pong::CliArgs;
    /// assert!(CliArgs::from_args(&["1", "2", "3", "4", "5", "6", "7"]).is_ok());
    /// assert!(CliArgs::from_args(&["1", "2", "3", "4", "5", "6"]).is_err());
    /// assert!(CliArgs::from_args(&["1", "2", "3", "4", "5", "6", "7", "8"]).is_err());
    /// ```
    pub fn from_args<S: AsRef<str>>(
        args: impl IntoIterator<Item = S>,
    ) -> Result<Self, &'static str> {
        let mut args = args.into_iter();
        let x0 = args
            .next()
            .ok_or("missing x0")?
            .as_ref()
            .parse()
            .map_err(|_| "invalid x0")?;
        let y0 = args
            .next()
            .ok_or("missing y0")?
            .as_ref()
            .parse()
            .map_err(|_| "invalid y0")?;
        let z0 = args
            .next()
            .ok_or("missing z0")?
            .as_ref()
            .parse()
            .map_err(|_| "invalid z0")?;
        let x1 = args
            .next()
            .ok_or("missing x1")?
            .as_ref()
            .parse()
            .map_err(|_| "invalid x1")?;
        let y1 = args
            .next()
            .ok_or("missing y1")?
            .as_ref()
            .parse()
            .map_err(|_| "invalid y1")?;
        let z1 = args
            .next()
            .ok_or("missing z1")?
            .as_ref()
            .parse()
            .map_err(|_| "invalid z1")?;
        let n = args
            .next()
            .ok_or("missing n")?
            .as_ref()
            .parse()
            .map_err(|_| "invalid n")?;
        let None = args.next() else {
            return Err("too many args");
        };
        Ok(Self {
            x0,
            y0,
            z0,
            x1,
            y1,
            z1,
            n,
        })
    }
}

/// Contains the args
#[derive(Debug, Clone)]
pub struct Args {
    /// ball position at time t - 1
    pub p0: Vector3,
    /// ball position at time t
    pub p1: Vector3,
    /// time shift (greater than or equal to zero, integer)
    pub n: usize,
}

/// Converts the CLI args into the args
impl From<CliArgs> for Args {
    fn from(cli_args: CliArgs) -> Self {
        Self {
            p0: Vector3::new(cli_args.x0, cli_args.y0, cli_args.z0),
            p1: Vector3::new(cli_args.x1, cli_args.y1, cli_args.z1),
            n: cli_args.n,
        }
    }
}

/// Contains a 3D vector of [`f64`]s
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Vector3 {
    pub x: f64,
    pub y: f64,
    pub z: f64,
}

impl Vector3 {
    /// Creates a new [`Vector3`]
    ///
    /// # Examples
    ///
    /// ```
    /// # use maths_101_pong::Vector3;
    /// let v = Vector3::new(1.0, 2.0, 3.0);
    /// assert_eq!(v.x, 1.0);
    /// assert_eq!(v.y, 2.0);
    /// assert_eq!(v.z, 3.0);
    /// ```
    pub fn new(x: f64, y: f64, z: f64) -> Self {
        Self { x, y, z }
    }

    /// Returns the vector from `self` to `other`
    ///
    /// # Examples
    /// ```
    /// # use maths_101_pong::Vector3;
    /// let v1 = Vector3::new(1.0, 2.0, 3.0);
    /// let v2 = Vector3::new(4.0, 5.0, 6.0);
    /// let v3 = Vector3::new(3.0, 3.0, 3.0);
    /// assert_eq!(v1.to(v2), v3);
    /// ```
    pub fn to(self, other: Self) -> Self {
        Self {
            x: other.x - self.x,
            y: other.y - self.y,
            z: other.z - self.z,
        }
    }

    /// Returns the norm of the vector
    ///
    /// # Examples
    /// ```
    /// # use maths_101_pong::Vector3;
    /// let v = Vector3::new(3.0, 4.0, 0.0);
    /// assert_eq!(v.norm(), 5.0);
    /// ```
    pub fn norm(self) -> f64 {
        self.dot(self).sqrt()
    }

    /// Returns the dot product of `self` and `other`
    ///
    /// # Examples
    /// ```
    /// # use maths_101_pong::Vector3;
    /// let v1 = Vector3::new(1.0, 2.0, 3.0);
    /// let v2 = Vector3::new(4.0, 5.0, 6.0);
    /// assert_eq!(v1.dot(v2), 32.0);
    /// ```
    pub fn dot(mut self, other: Self) -> f64 {
        self.x *= other.x;
        self.y *= other.y;
        self.z *= other.z;
        self.x + self.y + self.z
    }

    /// Returns the angle between `self` and `other`
    ///
    /// # Examples
    /// ```
    /// # use maths_101_pong::Vector3;
    /// let v1 = Vector3::new(1.0, 2.0, 3.0);
    /// let v2 = Vector3::new(4.0, 5.0, 6.0);
    /// assert_eq!(v1.angle(v2), 0.2257261285527342);
    /// ```
    pub fn angle(self, other: Self) -> f64 {
        (self.dot(other) / (self.norm() * other.norm())).acos()
    }
}
