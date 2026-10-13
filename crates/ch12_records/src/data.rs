//! Immutable nominal datatype declarations and constructor schemes.
use crate::{
    ast::{DataDecl, TypeSyntax},
    diagnostics::{Diagnostic, ErrorCode},
    session::{Session, Stop},
    source::Span,
    types::{Scheme, Type},
};
use std::collections::BTreeMap;

/// Nominal identity retains a spelling for types printed outside the session.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TyConId {
    pub index: u32,
    pub name: String,
}

/// Constructor identities are globally unique; runtime tags are per datatype.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConId {
    pub index: u32,
    pub name: String,
}

/// The arity of every parameter is kind `*` until Chapter 13.
#[derive(Clone, Debug)]
pub struct TyConInfo {
    pub id: TyConId,
    pub arity: usize,
    pub constructors: Vec<ConId>,
}

/// Constructor fields refer to quantified variables numbered in parameter order.
#[derive(Clone, Debug)]
pub struct ConInfo {
    pub labels: Option<Vec<String>>,
    pub id: ConId,
    pub parent: TyConId,
    pub tag: u32,
    pub fields: Vec<Type>,
    pub scheme: Scheme,
}

/// A compilation unit shares one closed table, with no references to inference cells.
#[derive(Clone, Debug, Default)]
pub struct DataEnv {
    pub types: Vec<TyConInfo>,
    pub constructors: Vec<ConInfo>,
}

impl DataEnv {
    /// Resolve source-order named fields to declaration-order slots.
    pub fn fields(
        &self,
        id: &ConId,
        names: &[String],
        complete: bool,
        span: Span,
    ) -> Result<Vec<usize>, Diagnostic> {
        let con = self.con(id);
        let labels = con.labels.as_ref().ok_or_else(|| {
            Diagnostic::error(
                ErrorCode::UnknownField,
                "named fields require a record constructor",
                span,
            )
        })?;
        let mut indices = vec![];
        for name in names {
            let index =
                labels.iter().position(|n| n == name).ok_or_else(|| {
                    Diagnostic::error(
                        ErrorCode::UnknownField,
                        format!("unknown field {name}"),
                        span,
                    )
                })?;
            if indices.contains(&index) {
                return Err(Diagnostic::error(
                    ErrorCode::RepeatedField,
                    format!("duplicate field {name}"),
                    span,
                ));
            }
            indices.push(index);
        }
        if complete && indices.len() != labels.len() {
            return Err(Diagnostic::error(
                ErrorCode::MissingField,
                "record construction requires every field",
                span,
            ));
        }
        Ok(indices)
    }
    pub fn constructor(&self, name: &str) -> Option<&ConInfo> {
        self.constructors.iter().find(|c| c.id.name == name)
    }
    pub fn con(&self, id: &ConId) -> &ConInfo {
        &self.constructors[id.index as usize]
    }
    pub fn tycon(&self, name: &str) -> Option<&TyConInfo> {
        self.types.iter().find(|t| t.id.name == name)
    }
    /// Convert an annotation, implicitly quantifying its lower-case variables.
    pub fn signature(
        &self,
        syntax: &TypeSyntax,
        span: Span,
    ) -> Result<Scheme, Diagnostic> {
        let mut variables = BTreeMap::new();
        let ty = self.convert(syntax, &mut variables, true, span)?;
        Ok(Scheme {
            variables: (0..variables.len() as u32).collect(),
            ty,
        })
    }
    fn convert(
        &self,
        syntax: &TypeSyntax,
        variables: &mut BTreeMap<String, u32>,
        implicit: bool,
        span: Span,
    ) -> Result<Type, Diagnostic> {
        if let TypeSyntax::Arrow(a, b) = syntax {
            return Ok(Type::arrow(
                self.convert(a, variables, implicit, span)?,
                self.convert(b, variables, implicit, span)?,
            ));
        }
        let mut head = syntax;
        let mut arguments = Vec::new();
        while let TypeSyntax::App(f, a) = head {
            arguments.push(&**a);
            head = f;
        }
        arguments.reverse();
        let TypeSyntax::Name(name) = head else {
            return Err(Diagnostic::error(
                ErrorCode::Kind,
                "an arrow type cannot be applied",
                span,
            ));
        };
        let args = arguments
            .into_iter()
            .map(|a| self.convert(a, variables, implicit, span))
            .collect::<Result<Vec<_>, _>>()?;
        let base = match name.as_str() {
            "Int" => Some(Type::Int),
            "Bool" => Some(Type::Bool),
            "Char" => Some(Type::Char),
            "String" => Some(Type::String),
            _ => None,
        };
        if let Some(base) = base {
            if !args.is_empty() {
                return Err(Diagnostic::error(
                    ErrorCode::Kind,
                    "a base type has kind *",
                    span,
                ));
            }
            return Ok(base);
        }
        if name.starts_with(char::is_lowercase) || name.starts_with('_') {
            if !args.is_empty() {
                return Err(Diagnostic::error(
                    ErrorCode::Kind,
                    "datatype parameters have kind * in this chapter",
                    span,
                ));
            }
            let next = variables.len() as u32;
            if !implicit && !variables.contains_key(name) {
                return Err(Diagnostic::error(
                    ErrorCode::UnboundConstructor,
                    format!("unbound type variable {name}"),
                    span,
                ));
            }
            return Ok(Type::Var(
                *variables.entry(name.clone()).or_insert(next),
            ));
        }
        let info = self.tycon(name).ok_or_else(|| {
            Diagnostic::error(
                ErrorCode::UnboundConstructor,
                format!("unknown type {name}"),
                span,
            )
        })?;
        if args.len() != info.arity {
            return Err(Diagnostic::error(
                ErrorCode::Kind,
                format!(
                    "{name} needs {} type arguments, found {}",
                    info.arity,
                    args.len()
                ),
                span,
            ));
        }
        Ok(Type::Con(info.id.clone(), args))
    }
}

/// Declare all type heads before checking fields, permitting recursive datatypes.
pub fn collect(
    sess: &mut Session,
    declarations: &[DataDecl],
) -> Result<DataEnv, Stop> {
    let mut env = DataEnv::default();
    for declaration in declarations {
        if declaration.name == "Bool" && !declaration.parameters.is_empty()
        {
            sess.emit(Diagnostic::error(
                ErrorCode::Kind,
                "Bool has kind *",
                declaration.span,
            ));
        }
        if env.tycon(&declaration.name).is_some()
            || matches!(
                declaration.name.as_str(),
                "Int" | "Char" | "String"
            )
        {
            sess.emit(Diagnostic::error(
                ErrorCode::Duplicate,
                format!("duplicate type {}", declaration.name),
                declaration.span,
            ));
            continue;
        }
        env.types.push(TyConInfo {
            id: TyConId {
                index: env.types.len() as u32,
                name: declaration.name.clone(),
            },
            arity: declaration.parameters.len(),
            constructors: vec![],
        });
    }
    sess.finish(())?;
    for declaration in declarations {
        if declaration.constructors.len() != 1
            && declaration.constructors.iter().any(|c| c.labels.is_some())
        {
            sess.emit(Diagnostic::error(
                ErrorCode::RecordConstructors,
                "a record must be the sole constructor",
                declaration.span,
            ));
        }
        let parent = env.tycon(&declaration.name).unwrap().id.clone();
        let mut variables = BTreeMap::new();
        for (index, name) in declaration.parameters.iter().enumerate() {
            if variables.insert(name.clone(), index as u32).is_some() {
                sess.emit(Diagnostic::error(
                    ErrorCode::Duplicate,
                    format!("duplicate type parameter {name}"),
                    declaration.span,
                ));
            }
        }
        for (tag, constructor) in
            declaration.constructors.iter().enumerate()
        {
            if let Some(labels) = &constructor.labels {
                let mut seen = std::collections::BTreeSet::new();
                for label in labels {
                    if !seen.insert(label) {
                        sess.emit(Diagnostic::error(
                            ErrorCode::DuplicateField,
                            format!("duplicate field {label}"),
                            constructor.span,
                        ));
                    }
                }
            }
            if env.constructor(&constructor.name).is_some() {
                sess.emit(Diagnostic::error(
                    ErrorCode::Duplicate,
                    format!("duplicate constructor {}", constructor.name),
                    constructor.span,
                ));
                continue;
            }
            let fields = constructor
                .fields
                .iter()
                .map(|t| {
                    env.convert(t, &mut variables, false, constructor.span)
                })
                .collect::<Result<Vec<_>, _>>();
            let fields = match fields {
                Ok(fields) => fields,
                Err(error) => {
                    sess.emit(error);
                    continue;
                }
            };
            let parameters = (0..declaration.parameters.len() as u32)
                .collect::<Vec<_>>();
            let mut ty = if parent.name == "Bool" {
                Type::Bool
            } else {
                Type::Con(
                    parent.clone(),
                    parameters.iter().copied().map(Type::Var).collect(),
                )
            };
            for field in fields.iter().rev() {
                ty = Type::arrow(field.clone(), ty);
            }
            let id = ConId {
                index: env.constructors.len() as u32,
                name: constructor.name.clone(),
            };
            env.types[parent.index as usize]
                .constructors
                .push(id.clone());
            env.constructors.push(ConInfo {
                labels: constructor.labels.clone(),
                id,
                parent: parent.clone(),
                tag: tag as u32,
                fields,
                scheme: Scheme {
                    variables: parameters,
                    ty,
                },
            });
        }
    }
    sess.finish(env)
}
