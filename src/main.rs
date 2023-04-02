mod ir;
mod parse;
mod run;

use std::io;
use std::io::prelude::*;

use eyre::Result;

use ir::Translate;
use run::Executor;

fn main() -> Result<()> {
    let mut exe = run::Executor::new();
    loop {
        input(&mut exe)?;
    }
}

fn input(exe: &mut Executor) -> Result<()> {
    print!("> ");
    io::stdout().flush()?;

    let mut input = String::new();
    io::stdin().read_line(&mut input)?;

    let mut parser = parse::Parser::new(&input);
    let Some(ast) = parser.parse()? else {
        return Ok(())
    };

    let program = ast.translate();
    let res = exe.run(program)?;
    println!("{}", res);

    Ok(())
}
