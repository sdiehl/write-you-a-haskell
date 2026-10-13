//! Syn is the renamed program after surface sugar has become lambdas and Match.
use crate::{
    ast::Op, data::DataEnv, names::Id, renamed::BindGroup, source::Span,
};

/// Only the simple expression and pattern forms remain in these groups.
#[derive(Clone, Debug)]
pub struct Program {
    pub classes: crate::classes::Env,
    pub data: DataEnv,
    pub groups: Vec<BindGroup>,
    pub primitives: Vec<(Op, Id)>,
    pub main: Id,
    pub span: Span,
}
