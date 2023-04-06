mod ast;
mod error;
mod ir;
mod parser;
mod token;
mod tokenizer;

pub use ast::Expr;
pub use error::Error;
use eyre::Result;
pub use ir::Instruction;
use ir::ToIr;
pub use parser::Parser;

use crate::input::Input;

pub fn read_to_ast(mut input: Input) -> Result<Option<Expr>> {
    let mut tokenizer = tokenizer::Tokenizer::new(&mut input);
    let mut parser = Parser::new(tokenizer.into());
    parser.parse()
}

pub fn read_to_ir(input: Input) -> Result<Option<Vec<Instruction>>> {
    if let Some(ast) = read_to_ast(input)? {
        Ok(Some(ast.to_ir()))
    } else {
        Ok(None)
    }
}
