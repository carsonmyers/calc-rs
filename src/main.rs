mod input;
mod run;
mod translate;

use std::io;
use std::io::prelude::*;

use eyre::Result;
use itertools::Itertools;

fn main() -> Result<()> {
    let mut exe = run::Executor::new();
    loop {
        if let Err(err) = input(&mut exe) {
            println!("error: {}", err);
        }
    }
}

fn input(exe: &mut run::Executor) -> Result<()> {
    print!("> ");
    io::stdout().flush()?;

    let mut input = String::new();
    io::stdin().read_line(&mut input)?;

    let Some(program) = translate::read_to_ir(&input)? else {
        return Ok(())
    };

    let result = exe.run(program)?;
    if result.scale() == 0 {
        let num = result.mantissa();
        let nums = vec![
            ("(dec)", format_dec(num)),
            ("(hex)", format_hex(num)),
            ("(oct)", format_oct(num)),
            ("(bin)", format_bin(num)),
        ];

        let max_len = nums
            .iter()
            .map(|(_, formatted)| formatted.len())
            .max()
            .unwrap_or_default();

        for (prefix, formatted) in nums {
            println!("{} {:>max_len$}", prefix, formatted);
        }
    } else {
        println!("{}", result);
    }

    Ok(())
}

fn format_dec(num: i128) -> String {
    chunk_number(num.to_string(), 3, &",")
}

fn format_hex(num: i128) -> String {
    chunk_number(format!("{:0X}", num), 4, &"_")
}

fn format_oct(num: i128) -> String {
    chunk_number(format!("{:0o}", num), 4, &"_")
}

fn format_bin(num: i128) -> String {
    chunk_number(format!("{:0b}", num), 8, &"_")
}

fn chunk_number(num: String, size: usize, sep: &str) -> String {
    num.chars()
        .rev()
        .chunks(size)
        .into_iter()
        .map(|chunk| chunk.collect::<String>())
        .join(sep)
        .chars()
        .rev()
        .collect()
}
