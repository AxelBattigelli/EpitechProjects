use std::collections::HashMap;

pub fn x_sum(values: &[(f64, f64)]) -> f64 {
    values.iter().map(|&(x, _)| x).sum()
}

pub fn y_sum(values: &[(f64, f64)]) -> f64 {
    values.iter().map(|&(_, y)| y).sum()
}

pub fn xy_sum(values: &[(f64, f64)]) -> f64 {
    values.iter().map(|&(x, y)| x * y).sum()
}

pub fn x2_sum(values: &[(f64, f64)]) -> f64 {
    values.iter().map(|&(x, _)| x * x).sum()
}

pub fn y2_sum(values: &[(f64, f64)]) -> f64 {
    values.iter().map(|&(_, y)| y * y).sum()
}

pub fn correlation(a: f64, b: f64, values: &[(f64, f64)]) -> f64 {
    let y_chap_values: Vec<f64> = values.iter().map(|&(x, _)| a * x + b).collect();
    let y_values: Vec<f64> = values.iter().map(|&(_, y)| y).collect();
    let mean_y_chap = y_chap_values.iter().sum::<f64>() / y_chap_values.len() as f64;
    let mean_y = y_values.iter().sum::<f64>() / y_values.len() as f64;
    let numerator = y_chap_values
        .iter()
        .zip(y_values.iter())
        .map(|(y_chap_val, y_val)| (y_chap_val - mean_y_chap) * (y_val - mean_y))
        .sum::<f64>();
    let denominator_y_chap = y_chap_values
        .iter()
        .map(|y_chap_val| (y_chap_val - mean_y_chap).powi(2))
        .sum::<f64>()
        .sqrt();
    let denominator_y = y_values
        .iter()
        .map(|y_val| (y_val - mean_y).powi(2))
        .sum::<f64>()
        .sqrt();
    numerator / (denominator_y_chap * denominator_y)
}

pub fn linear_coefficients(values: &[(f64, f64)]) -> (f64, f64) {
    let n = values.len() as f64;
    let x_sum = x_sum(values);
    let y_sum = y_sum(values);
    let xy_sum = xy_sum(values);
    let x2_sum = x2_sum(values);
    let a = (n * xy_sum - x_sum * y_sum) / (n * x2_sum - x_sum * x_sum);
    let b = (y_sum * x2_sum - x_sum * xy_sum) / (n * x2_sum - x_sum * x_sum);
    (a, b)
}

pub fn rms_deviation(values: &[(f64, f64)], (a, b): (f64, f64)) -> f64 {
    let n = values.len() as f64;
    let rms = values
        .iter()
        .map(|&(x, y)| (y - (a * x + b)).powi(2))
        .sum::<f64>()
        / n;
    rms.sqrt()
}

pub fn rms_deviation2(values: &[(f64, f64)], (a, b): (f64, f64)) -> f64 {
    let n = values.len() as f64;
    let rms = values
        .iter()
        .map(|&(x, y)| (x - ((y - b) / a)).powi(2))
        .sum::<f64>()
        / n;
    rms.sqrt()
}

pub fn load_data() -> HashMap<String, (String, Vec<(f64, f64)>)> {
    let mut map = HashMap::new();
    let file = include_str!("../data.csv");
    let mut reader = file.lines();
    let years = reader
        .next()
        .unwrap()
        .split(';')
        .skip(2)
        .map(|s| s.parse().unwrap())
        .collect::<Vec<_>>();
    for line in reader {
        let mut line = line.split(';');
        let country = line.next().unwrap().to_string();
        let country_code = line.next().unwrap().to_string();
        let values = line
            .map(|s| s.parse::<f64>().ok())
            .enumerate()
            .filter_map(|(i, v)| v.map(|v| (years[i], v / 1_000_000.0)))
            .collect();
        map.insert(country_code, (country, values));
    }
    map
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_x_sum() {
        let values = vec![(1.0, 2.0), (3.0, 4.0), (5.0, 6.0)];
        assert_eq!(x_sum(&values), 9.0);
    }

    #[test]
    fn test_y_sum() {
        let values = vec![(1.0, 2.0), (3.0, 4.0), (5.0, 6.0)];
        assert_eq!(y_sum(&values), 12.0);
    }

    #[test]
    fn test_xy_sum() {
        let values = vec![(1.0, 2.0), (3.0, 4.0), (5.0, 6.0)];
        assert_eq!(xy_sum(&values), 44.0);
    }

    #[test]
    fn test_x2_sum() {
        let values = vec![(1.0, 2.0), (3.0, 4.0), (5.0, 6.0)];
        assert_eq!(x2_sum(&values), 35.0);
    }

    #[test]
    fn test_y2_sum() {
        let values = vec![(1.0, 2.0), (3.0, 4.0), (5.0, 6.0)];
        assert_eq!(y2_sum(&values), 56.0);
    }

    const EPSILON: f64 = 1e-6;

    #[test]
    fn test_correlation() {
        let values = vec![(1.0, 2.0), (3.0, 4.0), (5.0, 6.0)];
        assert!(
            (correlation(1.0, 0.0, &values) - 1.0).abs() < EPSILON,
            "Correlation test failed"
        );
    }

    #[test]
    fn test_linear_coefficients() {
        let values = vec![(1.0, 2.0), (3.0, 4.0), (5.0, 6.0)];
        let (a, b) = linear_coefficients(&values);

        let epsilon_check_a = (a - 1.0).abs() < EPSILON;
        let epsilon_check_b = (b - 0.0).abs() < EPSILON;

        println!("{} &&& {}", epsilon_check_a, epsilon_check_b);
        assert!(
            epsilon_check_a && epsilon_check_b,
            "Linear coefficients test failed. a: {}, b: {}. Difference: a={}, b={}",
            a,
            b,
            (a - 1.0),
            (b - 0.0)
        );
    }

    // #[test]
    // fn test_rms_deviation() {
    //     let values = vec![(1.0, 2.0), (3.0, 4.0), (5.0, 6.0)];
    //     assert!(
    //         rms_deviation(&values, (1.0, 0.0)) < EPSILON,
    //         "RMS deviation test failed"
    //     );
    // }

    // #[test]
    // fn test_rms_deviation2() {
    //     let values = vec![(1.0, 2.0), (3.0, 4.0), (5.0, 6.0)];
    //     assert!(
    //         rms_deviation2(&values, (1.0, 0.0)) < EPSILON,
    //         "RMS deviation2 test failed"
    //     );
    // }
}
