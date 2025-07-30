use std::process::{ExitCode, Termination};

use stone_analysis::{read_file, analyze_top_frequencies, cypher_wav, decipher_wav};

fn run() -> Result<(), String> {
    let mut args = std::env::args();
    args.next();

    let Some(select) = args.next() else {
        return Err("Not enough args (select)".into());
    };
    if select == "-h" || select == "--help" {
        println!(
            r"USAGE
    ./stone_analysis [--analyze IN_FILE N | --cypher IN_FILE OUT_FILE MESSAGE | --decypher IN_FILE]

IN_FILE         An audio file to be analyzed
OUT_FILE        Output audio file of the cypher mode
MESSAGE         The message to hide in the audio file
N               Number of top frequencies to display"
        );
        return Ok(());
    }

    let Some(file) = args.next() else {
        return Err("Not enough args (file)".into());
    };
    
    let data = read_file(&file)?;

    if select == "--analyze" {
        let Some(n) = args.next() else {
            return Err("Not enough args (n)".into());
        };

        let n: usize = match n.parse() {
            Ok(val) => val,
            Err(e) => return Err(format!("Not a usize: {}", e)),
        };

        let res = analyze_top_frequencies(&data, 48000, n);
        println!("Top {n} frequencies:");
        for item in res {
            println!("{item:.1} Hz");
        }
    } else if select == "--cypher" {
        let Some(file_out) = args.next() else {
            return Err("Not enough arguments (file_out)".into());
        };
        let Some(msg) = args.next() else {
            return Err("Not enoug arguments (msg)".into());
        };
        cypher_wav(&file, &file_out, data, &msg)?;
    } else if select == "--decypher" {
        let message = decipher_wav(&file)?;
        println!("{}", message);
    }
    if args.next().is_some() {
        return Err("Too many arguments".into());
    }

    Ok(())
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
    if let Err(val) = run() {
        eprintln!("ERROR: {}", val);
        MyResult::Err
    } else {
        MyResult::Ok
    }
}
