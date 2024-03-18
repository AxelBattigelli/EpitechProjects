use std::process::exit;
use std::{collections::HashMap, process::ExitCode};

trait VecExt {
    fn sorted(self) -> Self;
}

impl<T: Ord> VecExt for Vec<T> {
    fn sorted(mut self) -> Self {
        self.sort();
        self
    }
}

fn main() -> ExitCode {
    let country_codes = std::env::args().skip(1).collect::<Vec<_>>().sorted();
    if country_codes.is_empty() {
        println!("Usage: ./105demography country_code [...]");
        return 84.into();
    }
    let map = demography::load_data();
    let (_, country, values) = country_codes.into_iter().fold(
        (true, String::new(), HashMap::<usize, (f64, f64)>::new()),
        |(first, countries, values), country_code| {
            let (country, new_values) = map.get(&country_code).or_else(|| exit(84)).unwrap();
            let values = new_values
                .iter()
                .filter_map(|&(x, y)| {
                    if first {
                        Some((x as usize, (x, y)))
                    } else {
                        values
                            .get(&(x as usize))
                            .map(|&(i_x, i_y)| (x as usize, (i_x, i_y + y)))
                    }
                })
                .collect::<HashMap<_, _>>();
            if countries.is_empty() {
                (false, country.clone(), values)
            } else {
                (false, format!("{}, {}", countries, country), values)
            }
        },
    );
    let values = values
        .iter()
        .map(|(_, &(i_x, i_y))| (i_x, i_y))
        .collect::<Vec<_>>();
    let (a, b) = demography::linear_coefficients(&values);
    let rms = demography::rms_deviation(&values, (a, b));
    println!("Country: {}", country);
    println!("Fit1");
    if b < 0.0 {
        println!("    Y = {:.2} X - {:.2}", a, b * -1.0);
    } else {
        println!("    Y = {:.2} X + {:.2}", a, b);
    }
    println!("    Root-mean-square deviation: {:.2}", rms);
    println!("    Population in 2050: {:.2}", a * 2050. + b);
    let values = values.iter().map(|&(x, y)| (y, x)).collect::<Vec<_>>();
    let (a, b) = demography::linear_coefficients(&values);
    let rms2 = demography::rms_deviation2(&values, (a, b));
    println!("Fit2");
    if b < 0.0 {
        println!("    X = {:.2} Y - {:.2}", a, b * -1.0);
    } else {
        println!("    X = {:.2} Y + {:.2}", a, b);
    }
    println!("    Root-mean-square deviation: {:.2}", rms2);
    println!("    Population in 2050: {:.2}", (2050. - b) / a);
    println!("Correlation: {:.4}", demography::correlation(a, b, &values));
    0.into()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sorted() {
        let input = vec![3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5];
        let result = input.clone().sorted();
        let expected = vec![1, 1, 2, 3, 3, 4, 5, 5, 5, 6, 9];
        assert_eq!(result, expected);
    }
}
