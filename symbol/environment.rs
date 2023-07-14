use std::collections::HashMap;
use std::rc::Rc;

use Symbol;

pub struct Environment {
	store: HashMap<String, Rc<Symbol>>,
}

impl Environment {
	pub fn new() -> Self {
		Environment {
			store: HashMap::new(),
		}
	}

	pub fn set(&mut self, key: String, value: Rc<Symbol>) {
		self.store.insert(key, value);
	}

	pub fn get(&self, key: &str) -> Option<Rc<Symbol>> {
		match self.store.get(key) {
			Some(obj) => Some(Rc::clone(obj)),
			None => None,
		}
	}
}
