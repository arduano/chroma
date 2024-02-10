use crate::lang::{
    ast::{helpers::*, items::*},
    tokens::*,
    ErrorCollector,
};

/// Represents a type fn.
///
/// # Example
///
/// ```no_run
/// type fn AddField<Name: const ident, Val: Value> {
///     type Result = {
///         ...Val,
///         [Name]: string,
///     };
///
///     Result
/// }
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct SyTypeFn {
    signature: SyTypeFnSignature,
    body: Attempted<SyTypeFnBody>,
}

impl AstItem for SyTypeFn {
    const NAME: &'static str = "type function";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let signature = reader.parse_optional(env)?;
        let body = reader.parse_required(env.outside_nested_expr());

        Ok(Self { signature, body })
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        self.signature.check(env, errors);

        if let Ok(body) = &self.body {
            body.check(env, errors);
        }
    }
}

impl ItemWithSpan for SyTypeFn {
    fn span(&self) -> Span {
        self.signature.span().join(&self.body.span())
    }
}

#[derive(Debug, Clone, PartialEq)]
struct SyTypeFnBody {
    braces: TkBraces,
    contents: SyBody,
}

impl AstItem for SyTypeFnBody {
    const NAME: &'static str = "type function body";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let (braces, contents) =
            reader.parse_required_group::<TkBraces, SyBody>(env.outside_nested_expr())?;

        Ok(Self { braces, contents })
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        self.contents.check(env, errors);
    }
}

impl ItemWithSpan for SyTypeFnBody {
    fn span(&self) -> Span {
        self.braces.span().join(&self.contents.span())
    }
}

/// Represents a type fn signature.
///
/// # Example
///
/// ```no_run
/// type fn AddField<Name: const ident, Val: Value>
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct SyTypeFnSignature {
    pub name: Attempted<TkIdent>,
    pub args: Attempted<SyTypeArgs>,
}

impl AstItem for SyTypeFnSignature {
    const NAME: &'static str = "type function signature";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        reader.parse_optional_token::<TkType>()?;
        reader.parse_optional_token::<TkFn>()?;

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

impl ItemWithSpan for SyTypeFnSignature {
    fn span(&self) -> Span {
        self.name.span().join(&self.args.span())
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
pub struct SyTypeArgs {
    pub args: Vec<Attempted<SyTypeArg>>,
}

impl AstItem for SyTypeArgs {
    const NAME: &'static str = "type arguments";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        reader.parse_optional_token::<TkLessThan>()?;

        let mut args = Vec::new();

        loop {
            let arg = reader.parse_required(env);
            let errored = arg.is_err();
            args.push(arg);

            let had_comma = reader.parse_optional_token::<TkComma>().is_ok();

            if errored || !had_comma {
                reader.parse_required_token::<TkGreaterThan>().ok();
                break;
            } else {
                if reader.parse_optional_token::<TkGreaterThan>().is_ok() {
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

impl ItemWithSpan for SyTypeArgs {
    fn span(&self) -> Span {
        self.args.span()
    }
}

/// Represents a type argument.
///
/// # Example
///
/// No constraint `ArgName`\
/// With constraint: `ArgName: TypeConstraint`
#[derive(Debug, Clone, PartialEq)]
pub struct SyTypeArg {
    pub name: TkIdent,
    pub constraint: Option<SyTypeConstraint>,
}

impl AstItem for SyTypeArg {
    const NAME: &'static str = "type argument";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let name = reader.parse_required_token()?;

        let has_constraint = reader.parse_optional_token::<TkColon>().is_ok();

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

impl ItemWithSpan for SyTypeArg {
    fn span(&self) -> Span {
        self.name.span().join(&self.constraint.span())
    }
}

/// Represents a type constraint. Usually used in type arguments, e.g. `TypeArg: Constraint`
///
/// # Example
///
/// Non const: `MyType`\
/// Const: `const MyType`
#[derive(Debug, Clone, PartialEq)]
pub struct SyTypeConstraint {
    pub is_const: bool,
    pub name: Attempted<TkIdent>, // TODO: Fix
}

impl AstItem for SyTypeConstraint {
    const NAME: &'static str = "type constraint";

    fn parse<'a>(reader: &mut AstParser<'a>, _env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let is_const = reader.parse_optional_token::<TkConst>().is_ok();

        Ok(Self {
            is_const,
            name: reader.parse_required_token(),
        })
    }

    fn check(&self, _env: CheckingPhaseEnv, _errors: &mut ErrorCollector) {
        // TODO: Check inner constraint type with env.inside_nested_expr()
    }
}

impl ItemWithSpan for SyTypeConstraint {
    fn span(&self) -> Span {
        self.name.span()
    }
}
