use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use Symbol;

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    store: HashMap<String, Rc<Symbol>>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        let mut store = HashMap::new();
        store.insert(
            "len".to_string(),
            Rc::new(Symbol::BuiltInFunction {
                function: |args| {
                    if args.len() != 1 {
                        return Rc::new(Symbol::Integer(0));
                    }

                    match &*args[0] {
                        Symbol::StringLiteral(value) => {
                            Rc::new(Symbol::Integer(value.len() as i32))
                        }
                        Symbol::Array { elements } => {
                            Rc::new(Symbol::Integer(elements.len() as i32))
                        }
                        _ => Rc::new(Symbol::Integer(0)),
                    }
                },
            }),
        );

        Environment { store, outer: None }
    }

    pub fn new_enclosed(outer: Rc<RefCell<Environment>>) -> Self {
        Environment {
            store: HashMap::new(),
            outer: Some(Rc::clone(&outer)),
        }
    }

    pub fn set(&mut self, key: String, value: Rc<Symbol>) {
        self.store.insert(key, value);
    }

    pub fn get(&self, key: &str) -> Option<Rc<Symbol>> {
        match self.store.get(key) {
            Some(obj) => Some(Rc::clone(obj)),
            None => {
                if let Some(outer) = &self.outer {
                    outer.borrow().get(key)
                } else {
                    None
                }
            }
        }
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}
