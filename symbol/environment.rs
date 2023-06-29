use std::collections::HashMap;

use Symbol;

pub struct Environment {
	store: HashMap<String, Symbol>,
}

impl Environment {
	pub fn new() -> Self {
		Environment {
			store: HashMap::new(),
		}
	}

	pub fn set(&mut self, key: String, value: Symbol) {
		self.store.insert(key, value);
	}

	pub fn get(&self, key: &str) -> Option<&Symbol> {
		self.store.get(key)
	}
}
