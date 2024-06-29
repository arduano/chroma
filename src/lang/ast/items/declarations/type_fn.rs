use crate::lang::{
    ast::{helpers::*, items::*},
    tokens::*,
    CompilerError, ErrorCollector,
};

#[derive(Debug, Clone)]
pub struct SyTypeFn {
    signature: SyTypeFnSignature,
    body: Attempted<Grouped<TkBraces, SyDeclarationBody>>,
}

impl AstItem for SyTypeFn {
    const NAME: &'static str = "type function";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let signature = reader.parse_optional(env)?;
        let body = reader.parse_required_group(env.outside_nested_expr());

        Ok(Self { signature, body })
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        self.signature.check(env, errors);

        if let Ok(body) = &self.body {
            body.inner.check(env, errors);
        }
    }
}

impl ItemWithSpan for SyTypeFn {
    fn span(&self) -> Span {
        self.signature.span().join(&self.body.span())
    }
}

/// Represents a type fn signature.
///
/// # Example
///
/// ```no_run
/// type fn AddField(Name: const ident, Val: Value)
/// ```
#[derive(Debug, Clone)]
pub struct SyTypeFnSignature {
    pub ty_token: TkType,
    pub fn_token: TkFn,
    pub name: Attempted<TkIdent>,
    pub args: Attempted<Grouped<TkParens, SyTypeArgs>>,
}

impl AstItem for SyTypeFnSignature {
    const NAME: &'static str = "type function signature";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let ty_token = reader.parse_required_token()?;
        let fn_token = reader.parse_required_token()?;
        let name = reader.parse_required_token();
        let args = reader.parse_required_group(env);

        Ok(Self {
            ty_token,
            fn_token,
            name,
            args,
        })
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        if let Ok(args) = &self.args {
            args.inner.check(env.inside_type_only(), errors);
        }
    }
}

impl ItemWithSpan for SyTypeFnSignature {
    fn span(&self) -> Span {
        self.name.span().join(&self.args.span())
    }
}

#[derive(Debug, Clone)]
pub struct SyTypeArgs {
    pub args: Vec<Attempted<SyTypeArg>>,
}

impl AstItem for SyTypeArgs {
    const NAME: &'static str = "type arguments";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let mut args = Vec::new();
        reader.set_error_recovery_mode(ErrorRecoveryMode::until_token::<TkComma>());

        while !reader.is_empty() {
            let arg = reader.parse_required(env.inside_nested_expr());

            let comma = reader.parse_optional_token::<TkComma>();

            if comma.is_err() && !reader.is_empty() {
                let span = reader.search_until_token::<TkDataLineEndSearch>();
                reader.add_error(CompilerError::new("Expected ,", span));
            }

            args.push(arg);
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
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
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
