mod parse;
mod text;

use parse::Parser;

fn main() {
    let input = "2 + 3 * 4";
    let parser = Parser::new(input);
    parser.parse();
}
