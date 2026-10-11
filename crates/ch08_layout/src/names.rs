//! Compact spellings and session-local unique names; renaming arrives in Chapter 9.
use std::collections::HashMap;
/// An interned spelling, independent of binding identity.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Symbol(pub u32);
/// A spelling paired with a distinct binding identity.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Name {
    pub sym: Symbol,
    pub unique: u32,
}
#[derive(Default)]
/// A small handwritten interner with stable vector indices.
pub struct Interner {
    strings: Vec<String>,
    symbols: HashMap<String, Symbol>,
}
impl Interner {
    pub fn intern(&mut self, text: &str) -> Symbol {
        if let Some(symbol) = self.symbols.get(text) {
            return *symbol;
        }
        let symbol = Symbol(
            self.strings.len().try_into().expect("too many symbols"),
        );
        self.strings.push(text.into());
        self.symbols.insert(text.into(), symbol);
        symbol
    }
    pub fn resolve(&self, symbol: Symbol) -> &str {
        &self.strings[symbol.0 as usize]
    }
}
#[derive(Default)]
/// Allocates distinct names without global mutable state.
pub struct Supply {
    next: u32,
}
impl Supply {
    pub fn fresh(&mut self, sym: Symbol) -> Name {
        let unique = self.next;
        self.next =
            self.next.checked_add(1).expect("name supply exhausted");
        Name { sym, unique }
    }
}
