use crate::syntax::{ast::helpers::*, tokens::*};

/// Represents a type fn signature.
///
/// # Example
///
/// ```no_run
/// type fn AddField<Name: const ident, Val: Value>
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct STypeFnSignature {
    pub name: Attempted<TIdent>,
    pub args: Attempted<STypeArgs>,
}

impl AstItem for STypeFnSignature {
    const NAME: &'static str = "type function signature";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        reader.parse_optional_token::<TType>()?;
        reader.parse_optional_token::<TFn>()?;

        let name = reader.parse_required_token();
        let args = reader.parse_required(env);

        Ok(Self { name, args })
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        if let Ok(args) = &self.args {
            args.check(env.inside_type_only(), errors);
        }
    }
}

/// Represents a type argument list.
///
/// # Example
///
/// ```no_run
/// <Arg1, Arg2: Constraint, Arg3: const Constraint>
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct STypeArgs {
    pub args: Vec<Attempted<STypeArg>>,
}

impl AstItem for STypeArgs {
    const NAME: &'static str = "type arguments";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        reader.parse_optional_token::<TLessThan>()?;

        let mut args = Vec::new();

        loop {
            let arg = reader.parse_required(env);
            let errored = arg.is_err();
            args.push(arg);

            let had_comma = reader.parse_optional_token::<TComma>().is_ok();

            if errored || !had_comma {
                reader.parse_required_token::<TGreaterThan>().ok();
                break;
            } else {
                if reader.parse_optional_token::<TGreaterThan>().is_ok() {
                    break;
                }
            }
        }

        Ok(Self { args })
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        for arg in &self.args {
            if let Ok(arg) = &arg {
                arg.check(env.inside_type_only(), errors);
            }
        }
    }
}

/// Represents a type argument.
///
/// # Example
///
/// No constraint `ArgName`\
/// With constraint: `ArgName: TypeConstraint`
#[derive(Debug, Clone, PartialEq)]
pub struct STypeArg {
    pub name: TIdent,
    pub constraint: Option<STypeConstraint>,
}

impl AstItem for STypeArg {
    const NAME: &'static str = "type argument";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let name = reader.parse_required_token()?;

        let has_constraint = reader.parse_optional_token::<TColon>().is_ok();

        let constraint = if has_constraint {
            Some(reader.parse_required(env)?)
        } else {
            None
        };

        Ok(Self { name, constraint })
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        if let Some(constraint) = &self.constraint {
            constraint.check(env.inside_type_only(), errors);
        }
    }
}

/// Represents a type constraint. Usually used in type arguments, e.g. `TypeArg: Constraint`
///
/// # Example
///
/// Non const: `MyType`\
/// Const: `const MyType`
#[derive(Debug, Clone, PartialEq)]
pub struct STypeConstraint {
    pub is_const: bool,
    pub name: Attempted<TIdent>, // TODO: Fix
}

impl AstItem for STypeConstraint {
    const NAME: &'static str = "type constraint";

    fn parse<'a>(reader: &mut AstParser<'a>, _env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let is_const = reader.parse_optional_token::<TConst>().is_ok();

        Ok(Self {
            is_const,
            name: reader.parse_required_token(),
        })
    }

    fn check(&self, _env: CheckingPhaseEnv, _errors: &mut ErrorCollector) {
        // TODO: Check inner constraint type with env.inside_nested_expr()
    }
}
