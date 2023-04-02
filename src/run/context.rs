use eyre::Result;
use rust_decimal::Decimal;

use crate::run::error::Error;

pub struct ExecutionContext {
    pub stack: Stack,
}

impl ExecutionContext {
    pub fn new() -> Self {
        Self {
            stack: Stack::new(),
        }
    }

    pub fn get_result(&mut self) -> Result<Decimal> {
        let result = self.stack.pop_dec()?;
        if self.stack.stack.len() > 0 {
            return Err(Error::ExecutionNotFinished(self.stack.stack.len()).into());
        }

        Ok(result)
    }
}

pub struct Stack {
    stack: Vec<u8>,
}

impl Stack {
    pub fn new() -> Self {
        Self {
            stack: Vec::with_capacity(1024),
        }
    }

    pub fn push_dec(&mut self, dec: &Decimal) {
        self.push_chunk(&dec.scale().to_le_bytes());
        self.push_chunk(&dec.mantissa().to_le_bytes());
    }

    pub fn pop_dec(&mut self) -> Result<Decimal> {
        let mantissa_bytes = self.pop_chunk(16)?.as_slice().try_into()?;
        let mantissa = i128::from_le_bytes(mantissa_bytes);

        let scale_bytes = self.pop_chunk(4)?.as_slice().try_into()?;
        let scale = u32::from_le_bytes(scale_bytes);

        let res = Decimal::try_from_i128_with_scale(mantissa, scale)?;
        Ok(res)
    }

    fn push_chunk(&mut self, bytes: &[u8]) {
        for byte in bytes {
            self.stack.push(*byte)
        }
    }

    fn pop_chunk(&mut self, bytes: usize) -> Result<Vec<u8>> {
        if self.stack.len() == 0 {
            return Err(Error::NoValuesOnStack.into());
        }
        if self.stack.len() < bytes {
            return Err(Error::PartialDataOnStack(bytes, self.stack.len()).into());
        }

        let mut res = vec![0u8; bytes];
        res.clone_from_slice(&self.stack[self.stack.len() - bytes..]);

        unsafe {
            self.stack.set_len(self.stack.len() - bytes);
        }

        Ok(res)
    }
}
