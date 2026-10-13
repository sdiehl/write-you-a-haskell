//! Compact spellings and session-local unique names.
use std::collections::HashMap;

/// An interned spelling, independent of binding identity.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Symbol(pub u32);

/// A spelling paired with a distinct binding identity.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
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

/// Keep a diagnostic spelling beside a unique name after the session is gone.
#[derive(Clone, Debug)]
pub struct Id {
    pub name: Name,
    pub text: String,
}

// The spelling is diagnostic metadata, never part of map-key comparison.
impl PartialEq for Id {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for Id {}

impl PartialOrd for Id {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Id {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.name.cmp(&other.name)
    }
}

impl std::hash::Hash for Id {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl std::fmt::Display for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}#{}", self.text, self.name.unique)
    }
}

impl crate::session::Session {
    pub fn fresh_id(&mut self, text: &str) -> Id {
        let sym = self.interner.intern(text);
        Id {
            name: self.supply.fresh(sym),
            text: text.into(),
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn diagnostic_spelling_is_not_binding_identity() {
        let mut session = crate::session::Session::default();
        let first = session.fresh_id("x");
        let mut renamed = first.clone();
        renamed.text = "printed differently".into();
        assert_eq!(first, renamed);
        assert_eq!(
            std::collections::BTreeSet::from([
                first.clone(),
                renamed.clone()
            ])
            .len(),
            1
        );
        assert_eq!(
            std::collections::HashSet::from([first, renamed]).len(),
            1
        );
    }
}
