use std::process::ExitCode;
use ep103cipher::Matrix;

fn main() -> ExitCode {
    let message = match std::env::args().nth(1) {
        Some(msg) => msg,
        None => return ExitCode::from(84),
    };

    let key = match std::env::args().nth(2) {
        Some(k) => k,
        None => return ExitCode::from(84),
    };

    let decrypt = match std::env::args().nth(3) {
        Some(s) => s.parse::<u8>().ok().map(|n| n != 0),
        None => return ExitCode::from(84),
    };

    if let Some(decrypt) = decrypt {
        let key_matrix = Matrix::new_square_from_string(&key);
        let message_matrix = if decrypt {
            let floats: Result<Vec<_>, _> = message
                .split(' ')
                .map(|s| s.parse().map_err(|_| std::process::exit(84)))
                .collect();
            match floats {
                Ok(floats) => Matrix::new_from_floats_and_width(floats, key_matrix.height()),
                Err(_) => return ExitCode::from(84),
            }
        } else {
            Matrix::new_from_str_and_width(&message, key_matrix.height())
        };

        let key_matrix = if decrypt { key_matrix.inverse() } else { key_matrix };
        let result = message_matrix.mat_mul(&key_matrix).unwrap();
        println!("Key matrix:\n{}", key_matrix);

        if !decrypt {
            println!("\nEncrypted message:\n{:#}", result);
        } else {
            println!("\nDecrypted message:\n{}", result.extract_string());
        }
    }

    ExitCode::SUCCESS
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_main_success() {
        let args = vec![
            String::from("program_name"),
            String::from("message"),
            String::from("key"),
            String::from("0"),
        ];
        assert_eq!(simulate_main(args), 0);
    }

    #[test]
    fn test_main_missing_args() {
        let args = vec![String::from("program_name")];
        assert_eq!(simulate_main(args), 84);
    }

    #[test]
    fn test_main_invalid_args() {
        let args = vec![
            String::from("program_name"),
            String::from("message"),
            String::from("key"),
            String::from("invalid_input"),
        ];
        assert_eq!(simulate_main(args), 84);
    }

    // fn simulate_main(args: Vec<String>) -> ExitCode {
    //     let mut arguments = args.iter();
    //     let program_name = arguments.next().unwrap_or(&String::new()).clone();
    //     let message = arguments.next().unwrap_or(&String::new()).clone();
    //     let key = arguments.next().unwrap_or(&String::new()).clone();
    //     let decrypt = arguments.next().map(|s| s.parse::<u8>().ok().map(|n| n != 0)).flatten();

    //     if message.is_empty() || key.is_empty() || decrypt.is_none() {
    //         return ExitCode::from(84);
    //     }
    }
}
