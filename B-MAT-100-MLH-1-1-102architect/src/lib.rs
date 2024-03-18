trait FloatExt {
    fn abs_zero(self) -> Self;
}

impl FloatExt for f64 {
    fn abs_zero(self) -> Self {
        if self.abs() < 1e-6 {
            0.0
        } else {
            self
        }
    }
}

#[derive(Clone, Copy)]
pub struct Matrix<const M: usize, const N: usize> {
    pub data: [[f64; N]; M],
}

impl<const M: usize, const N: usize> std::fmt::Debug for Matrix<M, N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Matrix")
            .field("width", &N)
            .field("height", &M)
            .field("data", &self.data)
            .finish()
    }
}

impl<const M: usize, const N: usize> std::fmt::Display for Matrix<M, N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            write!(f, "(")?;
            self.data[0][0].abs_zero().fmt(f)?;
            write!(f, ", ")?;
            self.data[1][0].abs_zero().fmt(f)?;
            write!(f, ")")?;
        } else {
            for (rn, row) in self.data.iter().enumerate() {
                if rn > 0 {
                    writeln!(f)?;
                }
                for (ln, &col) in row.iter().enumerate() {
                    if ln > 0 {
                        write!(f, "\t")?;
                    }
                    col.abs_zero().fmt(f)?;
                }
            }
        }
        Ok(())
    }
}

impl Matrix<3, 1> {
    /// Matrix from 2D coordinates
    ///
    /// # Example
    /// ```
    /// # use maths_102_architect::Matrix;
    /// let m = Matrix::from_2d_coordinates(1.0, 2.0);
    /// assert_eq!(format!("{:#}", m), "(1, 2)");
    /// ```
    pub fn from_2d_coordinates(x: f64, y: f64) -> Self {
        Self {
            data: [[x], [y], [1.0]],
        }
    }
}

impl Matrix<3, 3> {
    /// Translation matrix for a given x and y offset
    ///
    /// # Example
    /// ```
    /// # use maths_102_architect::Matrix;
    /// let m = Matrix::translation(1.0, 2.0);
    /// assert_eq!(format!("{}", m), "1\t0\t1\n0\t1\t2\n0\t0\t1");
    /// ```
    pub fn translation(x: f64, y: f64) -> Self {
        Self {
            data: [[1.0, 0.0, x], [0.0, 1.0, y], [0.0, 0.0, 1.0]],
        }
    }

    /// Scaling matrix for a given x and y factor
    ///
    /// # Example
    /// ```
    /// # use maths_102_architect::Matrix;
    /// let m = Matrix::scaling(1.0, 2.0);
    /// assert_eq!(format!("{}", m), "1\t0\t0\n0\t2\t0\n0\t0\t1");
    /// ```
    pub fn scaling(x: f64, y: f64) -> Self {
        Self {
            data: [[x, 0.0, 0.0], [0.0, y, 0.0], [0.0, 0.0, 1.0]],
        }
    }

    /// Rotation matrix for a given angle in radians
    ///
    /// # Example
    /// ```
    /// # use maths_102_architect::Matrix;
    /// let m = Matrix::rotation(std::f64::consts::PI);
    /// let p1 = Matrix::from_2d_coordinates(1.0, 0.0);
    /// assert_eq!(format!("{:.2}", m.mat_mul(p1)), "-1.00\n0.00\n1.00");
    /// ```
    pub fn rotation(theta: f64) -> Self {
        let (sin, cos) = theta.sin_cos();
        Self {
            data: [[cos, -sin, 0.0], [sin, cos, 0.0], [0.0, 0.0, 1.0]],
        }
    }

    /// Reflection matrix for a given angle in radians
    ///
    /// # Example
    /// ```
    /// # use maths_102_architect::Matrix;
    /// let m = Matrix::reflection(std::f64::consts::PI);
    /// let p1 = Matrix::from_2d_coordinates(1.0, 0.0);
    /// assert_eq!(format!("{:.2}", m.mat_mul(p1)), "1.00\n-0.00\n1.00");
    /// ```
    pub fn reflection(theta: f64) -> Self {
        let (sin, cos) = (2.0 * theta).sin_cos();
        Self {
            data: [[cos, sin, 0.0], [sin, -cos, 0.0], [0.0, 0.0, 1.0]],
        }
    }

    /// Identity matrix
    ///
    /// # Example
    /// ```
    /// # use maths_102_architect::Matrix;
    /// let m = Matrix::identity();
    /// assert_eq!(format!("{}", m), "1\t0\t0\n0\t1\t0\n0\t0\t1");
    /// ```
    pub fn identity() -> Self {
        Self {
            data: [[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0., 0., 1.0]],
        }
    }
}

impl<const M: usize, const N: usize> Matrix<M, N> {
    /// Matrix multiplication
    pub fn mat_mul<const P: usize>(self, rhs: Matrix<N, P>) -> Matrix<M, P> {
        let mut result = Matrix::zero();
        for i in 0..M {
            for j in 0..P {
                for k in 0..N {
                    result.data[i][j] += self.data[i][k] * rhs.data[k][j];
                }
            }
        }
        result
    }

    /// Zero matrix
    pub fn zero() -> Self {
        Self {
            data: [[0.0; N]; M],
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_from_2d_coordinates() {
        let m = Matrix::from_2d_coordinates(1.0, 2.0);
        assert_eq!(m.data, [[1.0], [2.0], [1.0]]);
    }

    #[test]
    fn test_matrix_display() {
        let m = Matrix::translation(1.0, 2.0);
        assert_eq!(format!("{:#}", m.clone()), "(1, 0)");
        assert_eq!(format!("{}", m), "1\t0\t1\n0\t1\t2\n0\t0\t1");
    }

    #[test]
    fn test_matrix_debug() {
        let m = Matrix::translation(1.0, 2.0);
        assert_eq!(
            format!("{:?}", m),
            "Matrix { width: 3, height: 3, data: [[1.0, 0.0, 1.0], [0.0, 1.0, 2.0], [0.0, 0.0, 1.0]] }"
        );
    }

    #[test]
    fn test_translation() {
        let m = Matrix::translation(1.0, 2.0);
        assert_eq!(m.data, [[1.0, 0.0, 1.0], [0.0, 1.0, 2.0], [0.0, 0.0, 1.0]]);
    }

    #[test]
    fn test_scaling() {
        let m = Matrix::scaling(1.0, 2.0);
        assert_eq!(m.data, [[1.0, 0.0, 0.0], [0.0, 2.0, 0.0], [0.0, 0.0, 1.0]]);
    }

    #[test]
    fn test_rotation() {
        let m = Matrix::rotation(0.0);
        assert_eq!(m.data, [[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0., 0., 1.0]])
    }

    #[test]
    fn test_reflection() {
        let m = Matrix::reflection(0.0);
        assert_eq!(m.data, [[1.0, 0.0, 0.0], [0.0, -1.0, 0.0], [0., 0., 1.0]])
    }

    #[test]
    fn test_identity() {
        let m = Matrix::identity();
        assert_eq!(m.data, [[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0., 0., 1.0]])
    }

    #[test]
    fn test_mat_mul() {
        let m = Matrix::from_2d_coordinates(1.0, 2.0);
        assert_eq!(Matrix::identity().mat_mul(m).data, [[1.0], [2.0], [1.0]]);
    }
}
