use std::{
    f64::consts::PI, fs::File, io::{Read, Write}, ops::{Add, Mul, Sub}
};

pub fn read_file(filename: &str) -> Result<Vec<i16>, String> {
    let mut file = File::open(filename).map_err(|e| format!("Open file: {e}"))?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).map_err(|e| format!("Read file: {e}"))?;
    
    if buffer.len() < 44 || &buffer[0..4] != b"RIFF" || &buffer[8..12] != b"WAVE" {
        return Err("Not a WAVE".into());
    }
    
    let channels = u16::from_le_bytes([buffer[22], buffer[23]]);
    let sample_rate = u32::from_le_bytes([buffer[24], buffer[25], buffer[26], buffer[27]]);
    let bits_per_sample = u16::from_le_bytes([buffer[34], buffer[35]]);
    if channels != 1 || bits_per_sample != 16 {
        return Err("Not mono 16 bps".into());
    }

    if sample_rate != 48000 {
        return Err(format!("Sample rate is {sample_rate}, expected 48000"));
    }

    let data_offset = buffer
        .windows(4)
        .position(|w| w == b"data")
        .ok_or("No data offset")?
        + 8;

    if data_offset >= buffer.len() {
        return Err("Data offset out of file".into());
    }

    let data_bytes = &buffer[data_offset..];
    if data_bytes.len() % 2 != 0 {
        return Err("Data chunk has incomplete sample".into());
    }

    let mut samples = Vec::new();
    for chunk in buffer[data_offset..].chunks_exact(2) {
        samples.push(i16::from_le_bytes([chunk[0], chunk[1]]));
    }

    Ok(samples)
}

#[derive(Clone, Copy, Debug)]
struct Complex {
    re: f64,
    im: f64,
}

impl Mul for Complex {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Complex {
            re: self.re.mul_add(rhs.re, -(self.im * rhs.im)),
            im: self.re.mul_add(rhs.im, self.im * rhs.re),
        }
    }
}

impl Add for Complex {
    type Output = Self;

    fn add(self, rhs: Self) -> Complex {
        Complex {
            re: self.re + rhs.re,
            im: self.im + rhs.im,
        }
    }
}

impl Sub for Complex {
    type Output = Self;

    fn sub(self, rhs: Self) -> Complex {
        Complex {
            re: self.re - rhs.re,
            im: self.im - rhs.im,
        }
    }
}

impl Complex {
    fn new(re: f64, im: f64) -> Self {
        Self { re, im }
    }

    fn magnitude(&self) -> f64 {
        self.re.hypot(self.im)
    }
}

fn apply_hann_window(samples: &[i16]) -> Vec<Complex> {
    let n = samples.len();
    samples.iter().enumerate().map(|(i, &s)| {
        let w = 0.5 * (1.0 - (2.0 * PI * i as f64 / (n - 1) as f64).cos());
        Complex::new(s as f64 * w, 0.0)
    }).collect()
}

fn fft(input: &mut [Complex]) {
    let n = input.len();
    if n <= 1 {
        return;
    }

    let mut even: Vec<_> = input.iter().step_by(2).copied().collect();
    let mut odd: Vec<_> = input.iter().skip(1).step_by(2).copied().collect();

    fft(&mut even);
    fft(&mut odd);

    for k in 0..n / 2 {
        let twiddle = Complex::new(
            (2.0 * PI * k as f64 / n as f64).cos(),
            -(2.0 * PI * k as f64 / n as f64).sin(),
        );
        let t = twiddle * odd[k];
        input[k] = even[k] + t;
        input[k + n / 2] = even[k] - t;
    }
}

pub fn analyze_top_frequencies(samples: &[i16], sample_rate: usize, n: usize) -> Vec<f64> {
    let len = samples.len().next_power_of_two();
    let mut buffer = apply_hann_window(samples);
    buffer.resize(len, Complex::new(0.0, 0.0));

    fft(&mut buffer);

    let spectrum: Vec<(usize, f64)> = buffer.iter()
        .take(len / 2)
        .enumerate()
        .map(|(k, c)| (k, c.magnitude() / len as f64))
        .collect();

    let mut sorted = spectrum;
    sorted.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap());

    sorted.iter()
        .take(n)
        .map(|&(k, _)| k as f64 * sample_rate as f64 / len as f64)
        .collect()
}

fn encode_message(samples: &mut [i16], message: &str) {
    let message_bytes = message.as_bytes();
    let mut bits = Vec::new();

    let msg_len = message_bytes.len() as u16;
    for i in (0..16).rev() {
        bits.push(((msg_len >> i) & 1) as u8);
    }

    for &byte in message_bytes {
        for i in (0..8).rev() {
            bits.push(((byte >> i) & 1) as u8);
        }
    }

    let step = 97;
    let mut index = 0;

    for bit in bits {
        if index >= samples.len() {
            break;
        }
        if (samples[index] & 1) != bit as i16 {
            samples[index] ^= 1;
        }
        index += step;
    }
}

pub fn cypher_wav(original_path: &str, output_path: &str, mut samples: Vec<i16>, message: &str) -> Result<(), String> {
    encode_message(&mut samples, message);

    let mut file = File::open(original_path).map_err(|e| format!("Open file: {e}"))?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).map_err(|e| format!("Read file: {e}"))?;

    let data_offset = buffer
        .windows(4)
        .position(|w| w == b"data")
        .ok_or("No data offset")?
        + 8;

    if data_offset > buffer.len() {
        return Err("Data offset out of file".into());
    }

    let mut out = File::create(output_path).map_err(|e| format!("Create file: {e}"))?;
    out.write_all(&buffer[..data_offset]).map_err(|e| format!("Write file: {e}"))?;

    for sample in samples {
        out.write_all(&sample.to_le_bytes()).map_err(|e| format!("Write samples: {e}"))?;
    }

    Ok(())
}

fn decode_message(samples: &[i16]) -> String {
    let mut bits = Vec::new();
    let step = 97;
    let mut index = 0;

    for _ in 0..16 { 
        if index >= samples.len() {
            break;
        }
        bits.push((samples[index] & 1) as u8);
        index += step;
    }

    let mut msg_len = 0u16;
    for (i, &bit) in bits.iter().enumerate().take(16) {
        msg_len |= (bit as u16) << (15 - i);
    }

    bits.clear();
    for _ in 0..(msg_len as usize * 8) {
        if index >= samples.len() {
            break;
        }
        bits.push((samples[index] & 1) as u8);
        index += step;
    }

    let mut message_bytes = Vec::new();
    for chunk in bits.chunks(8) {
        let mut byte = 0u8;
        for (i, &bit) in chunk.iter().enumerate() {
            byte |= bit << (7 - i);
        }
        message_bytes.push(byte);
    }

    String::from_utf8(message_bytes).unwrap_or_else(|_| String::from("Invalid message"))
}

pub fn decipher_wav(input_path: &str) -> Result<String, String> {
    let mut file = File::open(input_path).map_err(|e| format!("Opening file: {e}"))?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).map_err(|e| format!("Reading file: {e}"))?;

    if buffer.len() < 44 || &buffer[0..4] != b"RIFF" || &buffer[8..12] != b"WAVE" {
        return Err("Not a RIFF file".into());
    }

    let data_offset = buffer
        .windows(4)
        .position(|w| w == b"data")
        .ok_or("No data offset")?
        + 8;

    if data_offset >= buffer.len() {
        return Err("Data offset out of file!".into());
    }

    let audio_data = &buffer[data_offset..];
    let samples = audio_data
        .chunks_exact(2)
        .map(|chunk| i16::from_le_bytes([chunk[0], chunk[1]]))
        .collect::<Vec<_>>();

    Ok(decode_message(&samples))
}
