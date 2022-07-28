use crate::parse::ast::*;
use crate::parse::error::ParseError;

use anyhow::Result;

pub enum InfixOp {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
}

pub enum StateKind {
    Group,
    Expr(Expr),
    Infix(InfixOp, Expr),
    Fn(String, Vec<Expr>),
    Stmt(Stmt),
    Assign(String),
    FnDecl(String, Vec<String>, Vec<Item>),
}

pub struct State {
    stack: Vec<StateKind>,
}

impl State {
    pub fn new() -> State {
        State {
            stack: vec![StateKind::Group],
        }
    }

    pub fn get(&self) -> &StateKind {
        self.stack.last().unwrap()
    }

    pub fn push(&mut self, kind: StateKind) {
        self.stack.push(kind);
    }

    pub fn pop(&mut self) -> Result<StateKind> {
        if self.stack.len() == 1 {
            Err(ParseError::InvalidParserState.into())
        } else {
            Ok(self.stack.pop().unwrap())
        }
    }
}
