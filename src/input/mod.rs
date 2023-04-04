/// Peekable iterator for calculator input
///
/// Input handles the position (including line and column) in the input text,
/// and allows the caller to extract the most recently matched lexeme all at
/// once with its span attached.
pub struct Input {
    data:   Vec<char>,
    len:    usize,
    next:   Position,
    lexeme: Position,
}

impl Input {
    /// Construct a new input from something stringy
    pub fn new<T: AsRef<str>>(input: T) -> Self {
        let data = input.as_ref().chars().collect::<Vec<_>>();
        let len = data.len();

        Self {
            data,
            len,
            next: Position::new(),
            lexeme: Position::new(),
        }
    }

    /// Check the next available character without consuming it
    pub fn peek(&mut self) -> Option<char> {
        if self.next.index >= self.len {
            None
        } else {
            Some(self.data[self.next.index])
        }
    }

    /// Get all the consumed text along with its span from either the beginning
    /// of the input or since the last time `take_lexeme` was called
    pub fn take_lexeme(&mut self) -> StringSpan {
        let value = self.data[self.lexeme.index..self.next.index]
            .iter()
            .collect::<String>();
        let span = Span::from(&self.lexeme, &self.next);

        self.lexeme = self.next.clone();

        StringSpan::new(value, span)
    }
}

impl Iterator for Input {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        if self.next.index >= self.len {
            None
        } else {
            let c = self.data[self.next.index];
            self.next.advance(c);
            Some(c)
        }
    }
}

/// A position in the input text (string index, line number, and column number)
#[derive(Debug, Clone, Eq)]
pub struct Position {
    index: usize,
    line:  usize,
    col:   usize,
}

impl Position {
    /// Create a new default position (idx 0, line and col 1)
    pub fn new() -> Self {
        Default::default()
    }

    /// Advance the position by one character, increasing the line number and
    /// resetting the column number if the character being consumed is a
    /// newline
    pub fn advance(&mut self, c: char) {
        if c == '\n' {
            self.line += 1;
            self.col = 1;
        }

        self.index += 1;
    }
}

impl Default for Position {
    fn default() -> Self {
        Self {
            index: 0,
            line:  1,
            col:   1,
        }
    }
}

impl PartialEq for Position {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl Ord for Position {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.index.cmp(&other.index)
    }
}

impl PartialOrd for Position {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(&other))
    }
}

/// A span in the input text (a start and end [`Position`](Position))
#[derive(Debug, Default, Clone, Eq)]
pub struct Span {
    start: Position,
    end:   Position,
}

impl Span {
    /// Create a new default span (two default [`Position`](Position)s, zero
    /// length)
    pub fn new() -> Self {
        Default::default()
    }

    /// Create a new span from a start and end position
    pub fn from(start: &Position, end: &Position) -> Self {
        Self {
            start: start.clone(),
            end:   end.clone(),
        }
    }
}

impl PartialEq for Span {
    fn eq(&self, other: &Self) -> bool {
        self.start.eq(&other.start)
    }
}

impl Ord for Span {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.start.cmp(&other.start)
    }
}

impl PartialOrd for Span {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(&other))
    }
}

/// A string of input text along with the span information about where it was
/// located
#[derive(Debug, Eq)]
pub struct StringSpan {
    value: String,
    span:  Span,
}

impl StringSpan {
    /// Create a new `StringSpan` from a string and its span
    pub fn new(value: String, span: Span) -> Self {
        Self { value, span }
    }

    /// Get a reference to the string value
    pub fn value(&self) -> &str {
        &self.value.as_str()
    }

    /// Get a reference to the span of the data
    pub fn span(&self) -> &Span {
        &self.span
    }

    /// Drop the span information and get just the string data
    pub fn to_string(self) -> String {
        self.value
    }

    pub fn to_span(self) -> Span {
        self.span
    }
}

impl PartialEq for StringSpan {
    fn eq(&self, other: &Self) -> bool {
        self.value.eq(&other.value) && self.span.eq(&other.span)
    }
}
