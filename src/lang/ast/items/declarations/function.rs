use crate::lang::{
    ast::{helpers::*, items::*},
    tokens::*,
    CompilerError,
};

#[derive(Debug, Clone)]
pub struct SyFunction {
    pub signature: SyFunctionSignature,
    pub body: Attempted<Grouped<TkBraces, SyBody>>,
}

impl AstItem for SyFunction {
    const NAME: &'static str = "function";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let signature = reader.parse_optional(env)?;
        let body = reader.parse_required_group(env.outside_nested_expr());

        Ok(Self { signature, body })
    }
}

impl ItemWithSpan for SyFunction {
    fn span(&self) -> Span {
        self.signature.span().join(&self.body.span())
    }
}

#[derive(Debug, Clone)]
pub struct SyFunctionSignature {
    pub fn_token: TkFn,
    pub name: Attempted<TkIdent>,
    pub args: Attempted<Grouped<TkParens, SyFunctionArgs>>,
}

impl AstItem for SyFunctionSignature {
    const NAME: &'static str = "function signature";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let fn_token = reader.parse_optional_token()?;
        let name = reader.parse_required_token();
        let args = reader.parse_required_group(env);

        Ok(Self {
            fn_token,
            name,
            args,
        })
    }
}

impl ItemWithSpan for SyFunctionSignature {
    fn span(&self) -> Span {
        self.name.span().join(&self.args.span())
    }
}

#[derive(Debug, Clone)]
pub struct SyFunctionArgs {
    pub args: Vec<Attempted<SyFunctionArg>>,
}

impl AstItem for SyFunctionArgs {
    const NAME: &'static str = "function arguments";

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
}

impl ItemWithSpan for SyFunctionArgs {
    fn span(&self) -> Span {
        self.args.span()
    }
}

#[derive(Debug, Clone)]
pub struct SyFunctionArg {
    pub name: TkIdent,
    pub constraint: Option<SyFnArgTypeConstraint>,
}

impl AstItem for SyFunctionArg {
    const NAME: &'static str = "function argument";

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
}

impl ItemWithSpan for SyFunctionArg {
    fn span(&self) -> Span {
        self.name.span().join(&self.constraint.span())
    }
}

#[derive(Debug, Clone)]
pub struct SyFnArgTypeConstraint {
    pub is_const: bool,
    pub name: Attempted<TkIdent>, // TODO: Fix
}

impl AstItem for SyFnArgTypeConstraint {
    const NAME: &'static str = "function argument type constraint";

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
}

impl ItemWithSpan for SyFnArgTypeConstraint {
    fn span(&self) -> Span {
        self.name.span()
    }
}
