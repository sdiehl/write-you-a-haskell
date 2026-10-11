//! Tarjan's depth-first strongly connected components, in dependency order.
/// An edge `v -> w` means that binding v needs binding w. Each vertex appears
/// once; dependencies precede their users, and source order breaks ties.
pub fn components(edges: &[Vec<usize>]) -> Vec<Vec<usize>> {
    struct Search<'a> {
        edges: &'a [Vec<usize>],
        next: usize,
        index: Vec<Option<usize>>,
        low: Vec<usize>,
        stack: Vec<usize>,
        active: Vec<bool>,
        result: Vec<Vec<usize>>,
    }
    impl Search<'_> {
        // ANCHOR: tarjan
        fn visit(&mut self, v: usize) {
            self.index[v] = Some(self.next);
            self.low[v] = self.next;
            self.next += 1;
            self.stack.push(v);
            self.active[v] = true;
            for &w in &self.edges[v] {
                if self.index[w].is_none() {
                    self.visit(w);
                    self.low[v] = self.low[v].min(self.low[w]);
                } else if self.active[w] {
                    self.low[v] = self.low[v].min(self.index[w].unwrap());
                }
            }
            if self.low[v] == self.index[v].unwrap() {
                let mut group = Vec::new();
                loop {
                    let w = self.stack.pop().unwrap();
                    self.active[w] = false;
                    group.push(w);
                    if w == v {
                        break;
                    }
                }
                group.sort_unstable();
                self.result.push(group);
            }
        }
        // ANCHOR_END: tarjan
    }
    let n = edges.len();
    let mut search = Search {
        edges,
        next: 0,
        index: vec![None; n],
        low: vec![0; n],
        stack: vec![],
        active: vec![false; n],
        result: vec![],
    };
    for v in 0..n {
        if search.index[v].is_none() {
            search.visit(v);
        }
    }
    search.result
}
