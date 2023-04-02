mod ast;
mod error;
mod ir;
mod parser;

use eyre::Result;

pub use ast::Expr;
pub use error::Error;
pub use ir::Instruction;
use ir::ToIr;
pub use parser::Parser;

pub fn read_to_ast(input: &str) -> Result<Option<Expr>> {
    let mut parser = Parser::new(input);
    parser.parse()
}

pub fn read_to_ir(input: &str) -> Result<Option<Vec<Instruction>>> {
    if let Some(ast) = read_to_ast(input)? {
        Ok(Some(ast.to_ir()))
    } else {
        Ok(None)
    }
}
