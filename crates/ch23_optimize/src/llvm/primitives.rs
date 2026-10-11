//! Wrapping integers and IEEE operations: never attach nsw/nuw or fast-math flags.
use super::*;
use crate::{ast::Op, primitives::Prim};
use inkwell::{FloatPredicate, IntPredicate};
impl<'ctx> Backend<'ctx, '_> {
    pub(super) fn untag(
        &self,
        v: IntValue<'ctx>,
    ) -> Result<IntValue<'ctx>> {
        Ok(self
            .builder
            .build_right_shift(v, self.n(1), true, "integer")?)
    }
    pub(super) fn retag(
        &self,
        v: IntValue<'ctx>,
    ) -> Result<IntValue<'ctx>> {
        let bits = self.builder.build_left_shift(v, self.n(1), "bits")?;
        Ok(self.builder.build_or(bits, self.n(1), "integer")?)
    }
    fn boolean(&self, bit: IntValue<'ctx>) -> Result<IntValue<'ctx>> {
        Ok(self
            .builder
            .build_select(bit, self.n(3), self.n(1), "boolean")?
            .into_int_value())
    }
    pub(super) fn primitive(
        &self,
        op: Op,
        args: &[Loc<'ctx>],
    ) -> Result<Loc<'ctx>> {
        use Prim::*;
        let p = match op {
            Op::Runtime(p) => p,
            Op::Add => IntAdd,
            Op::Sub => IntSub,
            Op::Mul => IntMul,
            Op::Div => IntDiv,
            Op::Mod => IntMod,
            Op::Eq => IntEq,
            Op::Lt => IntLt,
            Op::Le => IntLe,
            Op::Append => StrAppend,
        };
        match p {
            IntAdd | IntSub | IntMul | IntNeg | IntQuot | IntRem
            | IntDiv | IntMod | IntEq | IntLt | IntLe => {
                let a = self.loc_int(args[0])?;
                if p == IntNeg {
                    return Ok(Loc::Int(self.normalize(
                        self.builder.build_int_neg(a, "negate")?,
                    )?));
                }
                let b = self.loc_int(args[1])?;
                let result = match p {
                    IntAdd => self.builder.build_int_add(a, b, "add")?,
                    IntSub => self.builder.build_int_sub(a, b, "sub")?,
                    IntMul => self.builder.build_int_mul(a, b, "mul")?,
                    IntEq | IntLt | IntLe => {
                        let pred = match p {
                            IntEq => IntPredicate::EQ,
                            IntLt => IntPredicate::SLT,
                            _ => IntPredicate::SLE,
                        };
                        return Ok(Loc::Word(
                            self.boolean(self.builder.build_int_compare(
                                pred, a, b, "compare",
                            )?)?,
                        ));
                    }
                    _ => self.divide(p, a, b)?,
                };
                Ok(Loc::Int(self.normalize(result)?))
            }
            DblAdd | DblSub | DblMul | DblDiv | DblNeg | DblEq | DblLt
            | DblLe => {
                let a = self.loc_double(args[0])?;
                if p == DblNeg {
                    return Ok(Loc::Double(
                        self.builder.build_float_neg(a, "negate")?,
                    ));
                }
                let b = self.loc_double(args[1])?;
                let result = match p {
                    DblAdd => self.builder.build_float_add(a, b, "add")?,
                    DblSub => self.builder.build_float_sub(a, b, "sub")?,
                    DblMul => self.builder.build_float_mul(a, b, "mul")?,
                    DblDiv => self.builder.build_float_div(a, b, "div")?,
                    _ => {
                        let pred = match p {
                            DblEq => FloatPredicate::OEQ,
                            DblLt => FloatPredicate::OLT,
                            _ => FloatPredicate::OLE,
                        };
                        return Ok(Loc::Word(self.boolean(
                            self.builder.build_float_compare(
                                pred, a, b, "compare",
                            )?,
                        )?));
                    }
                };
                Ok(Loc::Double(result))
            }
            // Transcendentals, saturating conversions, text and effects reuse the runtime exactly.
            _ => {
                let words = args
                    .iter()
                    .map(|a| self.loc_word(*a))
                    .collect::<Result<Vec<_>>>()?;
                let value = self.call(
                    &format!("rt_{}", p.name()),
                    &words.iter().map(|a| (*a).into()).collect::<Vec<_>>(),
                )?;
                Ok(Loc::Word(value))
            }
        }
    }
    /// Restore signed 63-bit semantics before a comparison or division observes the result.
    // ANCHOR: normalize-int
    fn normalize(&self, value: IntValue<'ctx>) -> Result<IntValue<'ctx>> {
        let bits =
            self.builder
                .build_left_shift(value, self.n(1), "wrap.bits")?;
        Ok(self.builder.build_right_shift(
            bits,
            self.n(1),
            true,
            "wrap.int",
        )?)
    }
    // ANCHOR_END: normalize-int
    fn divide(
        &self,
        p: Prim,
        a: IntValue<'ctx>,
        b: IntValue<'ctx>,
    ) -> Result<IntValue<'ctx>> {
        let zero = self.builder.build_int_compare(
            IntPredicate::EQ,
            b,
            self.n(0),
            "zero",
        )?;
        let fail = self.block("division.zero");
        let ok = self.block("division.ok");
        self.builder.build_conditional_branch(zero, fail, ok)?;
        self.builder.position_at_end(fail);
        self.void_call("rt_div_zero", &[])?;
        self.builder.build_unreachable()?;
        self.builder.position_at_end(ok);
        // Decoded Int is 63-bit, so even MIN / -1 is representable in LLVM's i64.
        let q = self.builder.build_int_signed_div(a, b, "quotient")?;
        let r = self.builder.build_int_signed_rem(a, b, "remainder")?;
        if p == Prim::IntQuot {
            return Ok(q);
        }
        if p == Prim::IntRem {
            return Ok(r);
        }
        let nonzero = self.builder.build_int_compare(
            IntPredicate::NE,
            r,
            self.n(0),
            "nonzero",
        )?;
        let signs = self.builder.build_xor(a, b, "signs")?;
        let opposite = self.builder.build_int_compare(
            IntPredicate::SLT,
            signs,
            self.n(0),
            "opposite",
        )?;
        let adjust = self.builder.build_and(nonzero, opposite, "floor")?;
        let (plain, rounded) = if p == Prim::IntDiv {
            (
                q,
                self.builder.build_int_sub(
                    q,
                    self.n(1),
                    "floor.quotient",
                )?,
            )
        } else {
            (r, self.builder.build_int_add(r, b, "floor.remainder")?)
        };
        Ok(self
            .builder
            .build_select(adjust, rounded, plain, "division")?
            .into_int_value())
    }
}
