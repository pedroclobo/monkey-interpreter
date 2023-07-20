#[derive(Debug, PartialEq, Clone)]
pub struct Location {
	pub file: String,
	pub line: usize,
	pub column: usize,
}

impl Default for Location {
	fn default() -> Self {
		Location {
			file: "stdin".to_string(),
			line: 1,
			column: 0,
		}
	}
}

impl std::fmt::Display for Location {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}:{}:{}", self.file, self.line, self.column)
	}
}
