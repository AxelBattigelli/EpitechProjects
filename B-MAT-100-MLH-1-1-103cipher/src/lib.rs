use std::{fmt::Display, ops::Mul};

#[derive(Debug)]
pub struct Matrix {
    data: Box<[Box<[f64]>]>,
}

impl Display for Matrix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            for (i, row) in self.data.iter().enumerate() {
                for col in row.iter() {
                    write!(f, "{}", col)?;
                    if i != self.data.len() {
                        write!(f, " ")?;
                    }
                }
            }
        } else {
            for (i, row) in self.data.iter().enumerate() {
                for (j, col) in row.iter().enumerate() {
                    write!(f, "{:.3}", col)?;
                    if j != row.len() - 1 {
                        write!(f, "\t")?;
                    }
                }
                if i != self.data.len() - 1 {
                    writeln!(f)?;
                }
            }
        }
        Ok(())
    }
}

impl Mul<f64> for Matrix {
    type Output = Self;

    fn mul(self, rhs: f64) -> Self::Output {
        let data = self
            .data
            .iter()
            .map(|row| {
                row.iter()
                    .map(|col| col * rhs)
                    .collect::<Vec<_>>()
                    .into_boxed_slice()
            })
            .collect::<Vec<_>>()
            .into_boxed_slice();

        Self { data }
    }
}

impl Mul<Matrix> for f64 {
    type Output = Matrix;

    fn mul(self, rhs: Matrix) -> Self::Output {
        rhs * self
    }
}

impl Matrix {
    pub fn new_square_from_string(s: impl AsRef<str>) -> Self {
        let string_length = s.as_ref().len();
        let square_size = (string_length as f64).sqrt().ceil() as usize;
        let mut data =
            vec![vec![0.0; square_size].into_boxed_slice(); square_size].into_boxed_slice();
        for (index, character) in s.as_ref().chars().enumerate() {
            let row = index / square_size;
            let col = index % square_size;
            data[row][col] = character as u32 as f64;
        }
        Self { data }
    }

    pub fn new_from_str_and_width(s: impl AsRef<str>, width: usize) -> Self {
        let string_length = s.as_ref().len();
        let height = (string_length as f64 / width as f64).ceil() as usize;
        let mut data = vec![vec![0.0; width].into_boxed_slice(); height].into_boxed_slice();
        let mut char_iter = s.as_ref().chars();
        
        for row in data.iter_mut().take(height) {
            for col in row.iter_mut().take(width) {
                if let Some(character) = char_iter.next() {
                    *col = character as u32 as f64;
                } else {
                    break;
                }
            }
        }
        Self { data }
    }

    pub fn new_from_floats_and_width(data: Vec<f64>, width: usize) -> Self {
        let height = (data.len() as f64 / width as f64).ceil() as usize;
        let mut chunks = data.chunks_exact(width);
        let mut data_vec: Vec<Box<[f64]>> = chunks
            .by_ref()
            .map(|row| row.to_vec().into_boxed_slice())
            .collect();
    
        if !chunks.remainder().is_empty() {
            let mut row = chunks.remainder().to_vec();
            row.resize(width, 0.0);
            data_vec.push(row.into_boxed_slice());
        }
    
        assert_eq!(data_vec.len(), height);
    
        Self {
            data: data_vec.into_boxed_slice(),
        }
    }    

    pub fn width(&self) -> usize {
        self.data[0].len()
    }

    pub fn height(&self) -> usize {
        self.data.len()
    }

    pub fn mat_mul(&self, other: &Self) -> Result<Self, String> {
        if self.width() != other.height() {
            return Err(format!(
                "Cannot multiply matrices of sizes {}x{} and {}x{}",
                self.height(),
                self.width(),
                other.height(),
                other.width()
            ));
        }
        let mut data =
            vec![vec![0.0; other.width()].into_boxed_slice(); self.height()].into_boxed_slice();
        for i in 0..self.height() {
            for j in 0..other.width() {
                for k in 0..self.width() {
                    data[i][j] += self.data[i][k] * other.data[k][j];
                }
            }
        }
        Ok(Self { data })
    }

    pub fn transpose(&self) -> Self {
        let mut data =
            vec![vec![0.0; self.height()].into_boxed_slice(); self.width()].into_boxed_slice();
        for i in 0..self.height() {
            for j in 0..self.width() {
                data[j][i] = self.data[i][j];
            }
        }
        Self { data }
    }    

    pub fn cofactors(&self) -> Self {
        let mut data = vec![vec![0.0; self.height()].into_boxed_slice(); self.width()].into_boxed_slice();
        for i in 0..self.height() {
            for j in 0..self.width() {
                let f = -(((i + j) % 2 * 2) as isize - 1);
                let mut sub = vec![];
                for ii in 0..self.height() {
                    if ii == i {
                        continue;
                    }
                    let mut row = vec![];
                    for jj in 0..self.width() {
                        if jj == j {
                            continue;
                        }
                        row.push(self.data[ii][jj]);
                    }
                    sub.push(row.into_boxed_slice());
                }
                let sub = Self { data: sub.into_boxed_slice() };
                data[i][j] = f as f64 * sub.det();
            }
        }
        Self { data }
    }
    

    pub fn det(&self) -> f64 {
        if self.width() != self.height() {
            panic!("Cannot calculate determinant of non-square matrix");
        }
        if self.width() == 1 {
            return self.data[0][0];
        }
        if self.width() == 2 {
            return self.data[0][0] * self.data[1][1] - self.data[0][1] * self.data[1][0];
        }
        let mut det = 0.0;
        let cofactor = self.cofactors();
        for i in 0..self.width() {
            det += self.data[0][i] * cofactor.data[0][i];
        }
        det
    }

    pub fn inverse(&self) -> Self {
        let det = self.det();
        if det == 0.0 {
            panic!("Cannot invert matrix with determinant 0");
        }
        let cofactors = self.cofactors().transpose();
        cofactors * det.recip()
    }

    pub fn extract_string(&self) -> String {
        let mut s = String::new();
        'outer: for row in self.data.iter() {
            for &col in row.iter() {
                if col.round() == 0.0 {
                    break 'outer;
                }
                s.push(char::from_u32(col.round() as u32).unwrap());
            }
        }
        s
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_square_from_string() {
        // test new square
        let input_string = "abcdefgh12345678";
        let expected_data: [[f64; 4]; 4] = [
            [97.0, 98.0, 99.0, 100.0],
            [101.0, 102.0, 103.0, 104.0],
            [49.0, 50.0, 51.0, 52.0],
            [53.0, 54.0, 55.0, 56.0],
        ];
        let square = Matrix::new_square_from_string(input_string);
        assert_eq!(square.data.len(), 4);
        assert_eq!(square.data[0].len(), 4);
        for i in 0..4 {
            for j in 0..4 {
                assert_eq!(square.data[i][j], expected_data[i][j]);
            }
        }
    }
    #[test]
    fn test_new_from_str_len_equal() {
        // test if string len equal width
        let matrix = Matrix::new_from_str_and_width("abcd", 2);
        assert_eq!(matrix.data.len(), 2);
        assert_eq!(matrix.data[0].len(), 2);
    }
    #[test]
    fn test_new_from_str_len_sup() {
        // test if string len superior at width
        let matrix = Matrix::new_from_str_and_width("abcdefg", 3);
        assert_eq!(matrix.data.len(), 3);
        assert_eq!(matrix.data[0].len(), 3);
    }
    #[test]
    fn test_new_from_str_len_inf() {
        // test if string len inferior at width
        let matrix = Matrix::new_from_str_and_width("a", 5);
        assert_eq!(matrix.data.len(), 1);
        assert_eq!(matrix.data[0].len(), 5);
    }
    #[test]
    fn test_new_from_str_null() {
        // test if string is null
        let matrix = Matrix::new_from_str_and_width("", 3);
        assert!(matrix.data.is_empty());
    }
    // #[test]
    // fn test_det_square_matrix() {
    //     // Create a square matrix
    //     let matrix = Matrix {
    //         data: vec![vec![1.0, 2.0], vec![3.0, 4.0]],
    //     };
    //     // Calculate the determinant of the matrix
    //     let determinant = matrix.det();
    //     // Ensure determinant calculation is correct for a 2x2 matrix
    //     assert_eq!(determinant, -2.0);
    // }

    // #[test]
    // #[should_panic(expected = "Cannot calculate determinant of non-square matrix")]
    // fn test_det_non_square_matrix() {
    //     // Create a non-square matrix
    //     let matrix = Matrix {
    //         data: vec![vec![1.0, 2.0, 3.0], vec![4.0, 5.0, 6.0]],
    //     };
    //     let _ = matrix.det(); // This should panic
    // }

    // #[test]
    // fn test_inverse_non_singular_matrix() {
    //     // Create a non-singular matrix
    //     let matrix = Matrix {
    //         data: vec![vec![4.0, 7.0], vec![2.0, 6.0]],
    //     };
    //     let inverse_matrix = matrix.inverse();
    //     // Ensure the inverse calculation is correct for a known non-singular matrix
    //     assert_eq!(
    //         inverse_matrix.data,
    //         vec![vec![0.6, -0.7], vec![-0.2, 0.4]]
    //     );
    // }

    // #[test]
    // #[should_panic(expected = "Cannot invert matrix with determinant 0")]
    // fn test_inverse_singular_matrix() {
    //     // Create a singular matrix
    //     let matrix = Matrix {
    //         data: vec![vec![1.0, 2.0], vec![2.0, 4.0]],
    //     };
    //     let _ = matrix.inverse(); // This should panic
    // }

    // #[test]
    // fn test_extract_string() {
    //     // Create a matrix with integer values
    //     let matrix = Matrix {
    //         data: vec![vec![65.0, 66.0], vec![67.0, 68.0]],
    //     };
    //     assert_eq!(matrix.extract_string(), "ABCD"); // Ensure correct string extraction
    // }
}
