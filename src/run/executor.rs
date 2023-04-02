use eyre::Result;
use rust_decimal::Decimal;
use rust_decimal::MathematicalOps;

use crate::run::context::ExecutionContext;
use crate::translate::Instruction;

pub struct Executor {
    pub context: ExecutionContext,
}

impl Executor {
    pub fn new() -> Self {
        Self {
            context: ExecutionContext::new(),
        }
    }

    pub fn run(&mut self, program: Vec<Instruction>) -> Result<Decimal> {
        for instruction in program {
            self.execute(instruction)?;
        }

        self.context.get_result()
    }

    pub fn execute(&mut self, instruction: Instruction) -> Result<()> {
        match instruction {
            Instruction::PushNumber(number) => self.execute_push_number(number),
            Instruction::Add => self.execute_add(),
            Instruction::Sub => self.execute_sub(),
            Instruction::Mul => self.execute_mul(),
            Instruction::Div => self.execute_div(),
            Instruction::Pow => self.execute_pow(),
            Instruction::Neg => self.execute_neg(),
        }
    }

    fn execute_push_number(&mut self, number: Decimal) -> Result<()> {
        self.context.stack.push_dec(&number);
        Ok(())
    }

    fn execute_add(&mut self) -> Result<()> {
        self.execute_binop(|lhs, rhs| lhs + rhs)
    }

    fn execute_sub(&mut self) -> Result<()> {
        self.execute_binop(|lhs, rhs| lhs - rhs)
    }

    fn execute_mul(&mut self) -> Result<()> {
        self.execute_binop(|lhs, rhs| lhs * rhs)
    }

    fn execute_div(&mut self) -> Result<()> {
        self.execute_binop(|lhs, rhs| lhs / rhs)
    }

    fn execute_pow(&mut self) -> Result<()> {
        self.execute_binop(|lhs, rhs| lhs.powd(rhs))
    }

    fn execute_neg(&mut self) -> Result<()> {
        let mut num = self.context.stack.pop_dec()?;
        num.set_sign_negative(!num.is_sign_negative());
        self.context.stack.push_dec(&num);

        Ok(())
    }

    fn execute_binop<F>(&mut self, f: F) -> Result<()>
    where
        F: FnOnce(Decimal, Decimal) -> Decimal,
    {
        let rhs = self.context.stack.pop_dec()?;
        let lhs = self.context.stack.pop_dec()?;
        let res = f(lhs, rhs);
        self.context.stack.push_dec(&res);

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use rust_decimal_macros::dec;

    use super::*;

    #[test]
    fn add() {
        let program = vec![
            Instruction::PushNumber(dec!(4)),
            Instruction::PushNumber(dec!(5)),
            Instruction::Add,
            Instruction::PushNumber(dec!(10)),
            Instruction::Add,
        ];

        let mut exe = Executor::new();
        let res = exe.run(program);
        assert_eq!(res.unwrap(), dec!(19));
    }

    #[test]
    fn sub() {
        let program = vec![
            Instruction::PushNumber(dec!(2)),
            Instruction::PushNumber(dec!(6)),
            Instruction::PushNumber(dec!(3)),
            Instruction::Sub,
            Instruction::Sub,
            Instruction::PushNumber(dec!(4)),
            Instruction::Sub,
        ];

        let mut exe = Executor::new();
        let res = exe.run(program);
        assert_eq!(res.unwrap(), dec!(-5));
    }

    #[test]
    fn mul() {
        let program = vec![
            Instruction::PushNumber(dec!(3)),
            Instruction::PushNumber(dec!(3)),
            Instruction::Mul,
            Instruction::PushNumber(dec!(2)),
            Instruction::PushNumber(dec!(4)),
            Instruction::Mul,
            Instruction::Mul,
        ];

        let mut exe = Executor::new();
        let res = exe.run(program);
        assert_eq!(res.unwrap(), dec!(72));
    }

    #[test]
    fn div() {
        let program = vec![
            Instruction::PushNumber(dec!(60)),
            Instruction::PushNumber(dec!(30)),
            Instruction::PushNumber(dec!(6)),
            Instruction::Div,
            Instruction::Div,
            Instruction::PushNumber(dec!(3)),
            Instruction::Div,
        ];

        let mut exe = Executor::new();
        let res = exe.run(program);
        assert_eq!(res.unwrap(), dec!(4));
    }

    #[test]
    fn pow() {
        let program = vec![
            Instruction::PushNumber(dec!(8)),
            Instruction::PushNumber(dec!(2)),
            Instruction::Pow,
        ];

        let mut exe = Executor::new();
        let res = exe.run(program);
        assert_eq!(res.unwrap(), dec!(64));
    }

    #[test]
    fn neg() {
        let program = vec![Instruction::PushNumber(dec!(24)), Instruction::Neg];

        let mut exe = Executor::new();
        let res = exe.run(program);
        assert_eq!(res.unwrap(), dec!(-24));
    }
}
