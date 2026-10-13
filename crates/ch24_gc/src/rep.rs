//! Type evidence survives erasure. Storage choices belong exclusively to codegen.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Rep {
    Int,
    Double,
    Word,
}

impl std::fmt::Display for Rep {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Int => "Int",
            Self::Double => "Double",
            Self::Word => "Word",
        })
    }
}

use crate::{
    anf::{Anf, Atom, Rhs, Var},
    core,
    names::Id,
};
use std::collections::BTreeMap;

pub type Table = BTreeMap<Id, Rep>;

/// Missing evidence is an internal error, never an invitation to infer a storage tag.
pub fn get(table: &Table, id: &Id) -> Result<Rep, String> {
    table
        .get(id)
        .copied()
        .ok_or_else(|| format!("missing representation evidence for {id}"))
}

pub fn atom(table: &Table, a: &Atom) -> Result<Rep, String> {
    match a {
        Atom::Var(v) | Atom::Global(v) => get(table, v),
        Atom::Lit(l) => Ok(crate::erase::literal_rep(l)),
    }
}

/// Numeric operands are checked against the primitive's declared signature.
pub fn primitive(
    table: &Table,
    op: crate::ast::Op,
    args: &[Atom],
    result: &Var,
    metadata: &core::Program,
) -> Result<(), String> {
    use crate::primitives::Prim;
    let generic = matches!(
        op,
        crate::ast::Op::Runtime(Prim::Trace | Prim::Error | Prim::Exit)
    );
    let types = if generic {
        vec![core::Type::Var(0)]
    } else {
        vec![]
    };
    let (mut ty, _) = crate::primitives::core_signature(
        op,
        &types,
        &metadata.data_types,
    )?;
    for arg in args {
        let core::Type::Arrow(a, b) = ty else {
            return Err("primitive arity evidence".into());
        };
        if !matches!(a.as_ref(), core::Type::Var(_))
            && atom(table, arg)? != crate::erase::representation(&a)
        {
            return Err(format!(
                "contradictory operand evidence for {}: {:?}",
                op.text(),
                arg
            ));
        }
        ty = *b;
    }
    let expected = if matches!(op, crate::ast::Op::Runtime(Prim::Trace)) {
        atom(table, &args[1])?
    } else if matches!(ty, core::Type::Var(_)) {
        get(table, result)?
    } else {
        crate::erase::representation(&ty)
    };
    if get(table, result)? != expected {
        return Err(format!("contradictory result evidence for {result}"));
    }
    Ok(())
}

/// Copy evidence to the beta continuation's fresh result. No type classification occurs here.
pub fn beta_results(program: &mut crate::anf::Program) {
    let mut todo = program
        .globals
        .iter()
        .map(|(_, b)| b.as_ref())
        .collect::<Vec<_>>();
    let mut joins = BTreeMap::new();
    let mut pending = vec![];
    while let Some(b) = todo.pop() {
        match b {
            Anf::Let(v, r, rest) => {
                if !program.reps.contains_key(v) {
                    let Anf::Jump(j, _) = rest.as_ref() else {
                        panic!("beta result continuation")
                    };
                    pending.push((v.clone(), j.clone()));
                }
                if let Rhs::Lambda(l) = r {
                    todo.push(&l.body);
                }
                todo.push(rest);
            }
            Anf::LetRec(bs, r) => {
                todo.push(r);
                for (_, l) in bs {
                    todo.push(&l.body);
                }
            }
            Anf::Join(j, ps, b, r) => {
                if ps.len() == 1 {
                    joins.insert(j.clone(), program.reps[&ps[0]]);
                }
                todo.push(b);
                todo.push(r);
            }
            Anf::Case(_, alts) => {
                for (_, _, b) in alts {
                    todo.push(b);
                }
            }
            _ => {}
        }
    }
    for (v, j) in pending {
        program.reps.insert(v, joins[&j]);
    }
}

pub fn lint_anf(program: &crate::anf::Program) -> Result<(), String> {
    let mut todo = vec![];
    for (v, b) in &program.globals {
        get(&program.reps, v)?;
        todo.push(b.as_ref());
    }
    while let Some(b) = todo.pop() {
        match b {
            Anf::Let(v, r, rest) => {
                get(&program.reps, v)?;
                match r {
                    Rhs::Prim(p, args, _) => primitive(
                        &program.reps,
                        *p,
                        args,
                        v,
                        &program.metadata,
                    )?,
                    Rhs::Lambda(l) => todo.push(&l.body),
                    _ => {}
                }
                todo.push(rest);
            }
            Anf::LetRec(bs, r) => {
                todo.push(r);
                for (_, l) in bs {
                    todo.push(&l.body);
                }
            }
            Anf::Join(_, _, b, r) => {
                todo.push(b);
                todo.push(r);
            }
            Anf::Case(_, alts) => {
                for (_, _, b) in alts {
                    todo.push(b);
                }
            }
            _ => {}
        }
    }
    Ok(())
}

pub fn lint_flat(program: &crate::flat::Program) -> Result<(), String> {
    use crate::flat::{Flat, Rhs};
    let mut todo = program
        .funcs
        .iter()
        .map(|f| f.body.as_ref())
        .collect::<Vec<_>>();
    for g in &program.globals {
        get(&program.reps, &g.name)?;
        todo.push(g.body.as_ref());
    }
    while let Some(b) = todo.pop() {
        match b {
            Flat::Let(v, r, rest) => {
                get(&program.reps, v)?;
                if let Rhs::Prim(p, args, _) = r {
                    primitive(
                        &program.reps,
                        *p,
                        args,
                        v,
                        &program.metadata,
                    )?;
                }
                todo.push(rest);
            }
            Flat::Join(_, _, b, r) => {
                todo.push(b);
                todo.push(r);
            }
            Flat::Case(_, alts) => {
                for (_, _, b) in alts {
                    todo.push(b);
                }
            }
            _ => {}
        }
    }
    Ok(())
}
