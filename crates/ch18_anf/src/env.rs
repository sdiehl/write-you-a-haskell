//! Persistent AVL environments. Capturing a scope shares its root in O(1).
//! Updates copy only a logarithmic search path, using an explicit work stack.
use std::{cmp::Ordering, rc::Rc};

type Link<K, V> = Option<Rc<Node<K, V>>>;
#[derive(Clone, Debug)]
struct Node<K, V> {
    key: K,
    value: V,
    left: Link<K, V>,
    right: Link<K, V>,
    height: u32,
}
/// An immutable scope root with path-copying updates. Cloning shares all bindings.
#[derive(Clone, Debug)]
pub struct Env<K, V> {
    root: Link<K, V>,
}
impl<K, V> Default for Env<K, V> {
    fn default() -> Self {
        Self { root: None }
    }
}
fn height<K, V>(node: &Link<K, V>) -> u32 {
    node.as_ref().map_or(0, |n| n.height)
}
impl<K: Clone, V: Clone> Node<K, V> {
    fn refresh(&mut self) {
        self.height = 1 + height(&self.left).max(height(&self.right));
    }
    fn left(mut self) -> Self {
        let mut root = (**self
            .right
            .as_ref()
            .expect("left rotation requires a right child"))
        .clone();
        self.right = root.left.take();
        self.refresh();
        root.left = Some(Rc::new(self));
        root.refresh();
        root
    }
    fn right(mut self) -> Self {
        let mut root = (**self
            .left
            .as_ref()
            .expect("right rotation requires a left child"))
        .clone();
        self.left = root.right.take();
        self.refresh();
        root.right = Some(Rc::new(self));
        root.refresh();
        root
    }
    fn balance(mut self) -> Self {
        self.refresh();
        if height(&self.left) > height(&self.right) + 1 {
            let child = self.left.as_ref().unwrap();
            if height(&child.right) > height(&child.left) {
                self.left = Some(Rc::new((**child).clone().left()));
            }
            self.right()
        } else if height(&self.right) > height(&self.left) + 1 {
            let child = self.right.as_ref().unwrap();
            if height(&child.left) > height(&child.right) {
                self.right = Some(Rc::new((**child).clone().right()));
            }
            self.left()
        } else {
            self
        }
    }
}
impl<K, V> Env<K, V> {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn is_empty(&self) -> bool {
        self.root.is_none()
    }
    pub fn clear(&mut self) {
        self.root = None;
    }
    /// Release unique nodes iteratively, allowing values with captured scopes to
    /// be drained by the evaluator's own destruction worklist.
    pub fn drain_unique(&mut self, values: &mut Vec<V>) {
        let mut todo = self.root.take().into_iter().collect::<Vec<_>>();
        while let Some(node) = todo.pop() {
            if let Ok(node) = Rc::try_unwrap(node) {
                todo.extend(node.left);
                todo.extend(node.right);
                values.push(node.value);
            }
        }
    }
}
impl<K: Ord + Clone, V: Clone> Env<K, V> {
    pub fn get(&self, key: &K) -> Option<&V> {
        let mut next = self.root.as_deref();
        while let Some(node) = next {
            match key.cmp(&node.key) {
                Ordering::Less => next = node.left.as_deref(),
                Ordering::Greater => next = node.right.as_deref(),
                Ordering::Equal => return Some(&node.value),
            }
        }
        None
    }
    pub fn insert(&mut self, key: K, value: V) {
        let mut path = vec![];
        let mut next = self.root.clone();
        let mut node = loop {
            let Some(current) = next else {
                break Node {
                    key,
                    value,
                    left: None,
                    right: None,
                    height: 1,
                };
            };
            let order = key.cmp(&current.key);
            if order == Ordering::Equal {
                let mut node = (*current).clone();
                node.value = value;
                break node;
            }
            next = if order == Ordering::Less {
                current.left.clone()
            } else {
                current.right.clone()
            };
            path.push((current, order));
        };
        while let Some((parent, order)) = path.pop() {
            let mut parent = (*parent).clone();
            if order == Ordering::Less {
                parent.left = Some(Rc::new(node));
            } else {
                parent.right = Some(Rc::new(node));
            }
            node = parent.balance();
        }
        self.root = Some(Rc::new(node));
    }
    pub fn push(&mut self, (key, value): (K, V)) {
        self.insert(key, value);
    }
    pub fn extend(&mut self, entries: impl IntoIterator<Item = (K, V)>) {
        for (key, value) in entries {
            self.insert(key, value);
        }
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn updates_share_untouched_scopes_and_keep_logarithmic_depth() {
        let mut env = Env::new();
        for key in 0..10_000 {
            env.insert(key, key);
        }
        let saved = env.clone();
        assert!(Rc::ptr_eq(
            env.root.as_ref().unwrap(),
            saved.root.as_ref().unwrap()
        ));
        for key in (0..10_000).rev() {
            env.insert(key, key + 1);
        }
        for key in 0..10_000 {
            assert_eq!(saved.get(&key), Some(&key));
            assert_eq!(env.get(&key), Some(&(key + 1)));
        }
        assert!(height(&env.root) < 20);
        assert!(height(&saved.root) < 20);
    }
}
