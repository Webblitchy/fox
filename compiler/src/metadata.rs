#[derive(Debug, Clone)]
pub struct Metadata {
    pub line: usize,   // start from 1
    pub column: usize, // start from the line (index from 1)
    pub start: usize,  // absolute start (without line)
    pub end: usize,    // after end
}

impl Metadata {
    pub fn empty() -> Metadata {
        return Metadata {
            line: 1,
            column: 1,
            start: 0,
            end: 0,
        };
    }

    pub fn print(&self) {
        println!(" - line: {}", self.line);
        println!(" - column: {}", self.column);
        println!(" - start: {}", self.start);
        println!(" - end: {}", self.end);
    }
}
