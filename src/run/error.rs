#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("no values on stack")]
    NoValuesOnStack,
    #[error("partial data on stack: expected {0} bytes, got {1}")]
    PartialDataOnStack(usize, usize),
    #[error("execution not finished: {0} bytes left on stack")]
    ExecutionNotFinished(usize),
}
