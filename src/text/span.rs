use std::fmt;

#[derive(Clone)]
pub struct Pos {
    idx: usize,
    line: usize,
    col: usize,
}

impl Pos {
    pub fn new() -> Pos {
        Pos {
            idx: 0,
            line: 1,
            col: 1,
        }
    }

    pub fn from(&self, count: usize) -> Pos {
        Pos {
            idx: self.idx + count,
            line: self.line,
            col: self.col + count,
        }
    }

    pub fn span_to(&self, end: Pos) -> Span {
        Span {
            start: self.clone(),
            end: end.clone(),
        }
    }

    pub fn next(&mut self, c: char) {
        if c == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }

        self.idx += 1;
    }
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

impl fmt::Debug for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}({}:{})", self.idx, self.line, self.col)
    }
}

#[derive(Clone)]
pub struct Span {
    start: Pos,
    end: Pos,
}

impl Span {
    pub fn from(&self, count: usize) -> Span {
        Span {
            start: self.end,
            end: self.end.from(count),
        }
    }

    pub fn from_pos(start: Pos, count: usize) -> Span {
        Span {
            start: start,
            end: start.from(count),
        }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}-{}", self.start, self.end)
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}-{:?}", self.start, self.end)
    }
}
