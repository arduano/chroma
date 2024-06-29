use std::sync::Arc;

use super::*;
/// Represents an object literal.
///
/// # Example
///
/// ```no_run
/// { field1: 1, field2, ...spreadfields, [ident]: "foo" }
/// ```
#[derive(Debug, Clone)]
pub struct SyObjectLiteral {
    pub braced: Grouped<TkBraces, SObjectLiteralFields>,
}

impl AstItem for SyObjectLiteral {
    const NAME: &'static str = "object literal";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let braced = reader.parse_optional_group::<TkBraces, SObjectLiteralFields>(env)?;
        Ok(Self { braced })
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        self.braced.inner.check(env.inside_nested_expr(), errors);
    }
}

impl ItemWithSpan for SyObjectLiteral {
    fn span(&self) -> Span {
        self.braced.span()
    }
}

/// Represents an object literal's contents.
///
/// # Example
///
/// ```no_run
/// field1: 1, field2, ...spreadfields, [ident]: "foo"
/// ```
#[derive(Debug, Clone)]
pub struct SObjectLiteralFields {
    pub fields: Vec<Attempted<SyObjectLiteralField>>,
}

impl AstItem for SObjectLiteralFields {
    const NAME: &'static str = "object literal contents";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let mut fields = Vec::new();
        reader.set_error_recovery_mode(ErrorRecoveryMode::until_token::<TkComma>());

        while !reader.is_empty() {
            let field = reader.parse_required(env.inside_nested_expr());

            let comma = reader.parse_optional_token::<TkComma>();

            if comma.is_err() && !reader.is_empty() {
                let span = reader.search_until_token::<TkDataLineEndSearch>();
                reader.add_error(CompilerError::new("Expected ,", span));
            }

            fields.push(field);
        }

        Ok(Self { fields })
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        for field in &self.fields {
            if let Ok(field) = field {
                field.check(env, errors);
            }
        }
    }
}

impl ItemWithSpan for SObjectLiteralFields {
    fn span(&self) -> Span {
        self.fields.span()
    }
}

/// Represents an object literal's fields.
///
/// # Example
///
/// Key-value: `field1: 1`
/// Key-variable: `field2`
/// Spread: `...spreadfields`
/// Computed key: `[ident]: "foo"`
#[derive(Debug, Clone)]
pub enum SyObjectLiteralField {
    KeyValue(SyObjectLiteralKeyValue),
    KeyVariable(SyObjectLiteralKeyVariable),
    Spread(SyObjectLiteralSpread),
    ComputedKey(SyObjectLiteralComputedKey),
}

impl AstItem for SyObjectLiteralField {
    const NAME: &'static str = "object literal field";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        if let Ok(expr) = reader.parse_optional(env) {
            return Ok(SyObjectLiteralField::KeyValue(expr));
        }
        if let Ok(expr) = reader.parse_optional(env) {
            return Ok(SyObjectLiteralField::KeyVariable(expr));
        }
        if let Ok(expr) = reader.parse_optional(env) {
            return Ok(SyObjectLiteralField::Spread(expr));
        }
        if let Ok(expr) = reader.parse_optional(env) {
            return Ok(SyObjectLiteralField::ComputedKey(expr));
        }

        Err(ParseError::NoMatch)
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        match self {
            Self::KeyValue(expr) => expr.check(env, errors),
            Self::KeyVariable(expr) => expr.check(env, errors),
            Self::Spread(expr) => expr.check(env, errors),
            Self::ComputedKey(expr) => expr.check(env, errors),
        }
    }
}

impl ItemWithSpan for SyObjectLiteralField {
    fn span(&self) -> Span {
        match self {
            Self::KeyValue(expr) => expr.span(),
            Self::KeyVariable(expr) => expr.span(),
            Self::Spread(expr) => expr.span(),
            Self::ComputedKey(expr) => expr.span(),
        }
    }
}

/// Represents an object literal's key-value field.
///
/// # Example
///
/// ```no_run
/// field1: 1
/// ```
#[derive(Debug, Clone)]
pub struct SyObjectLiteralKeyValue {
    pub key: TkIdent,
    pub colon: TkColon,
    pub value: Arc<Attempted<SyExpression>>,
}

impl AstItem for SyObjectLiteralKeyValue {
    const NAME: &'static str = "object literal key-value field";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let ident = reader.parse_optional_token()?;
        let colon = reader.parse_optional_token()?;
        let value = reader.parse_required(env.inside_nested_expr());

        Ok(Self {
            key: ident,
            colon,
            value: Arc::new(value),
        })
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        if let Ok(value) = &*self.value {
            value.check(env.inside_nested_expr(), errors);
        }
    }
}

impl ItemWithSpan for SyObjectLiteralKeyValue {
    fn span(&self) -> Span {
        self.key
            .span()
            .join(&self.colon.span())
            .join(&self.value.span())
    }
}

/// Represents an object literal's key-variable field.
///
/// # Example
///
/// ```no_run
/// field2
/// ```
#[derive(Debug, Clone)]
pub struct SyObjectLiteralKeyVariable {
    pub key: TkIdent,
}

impl AstItem for SyObjectLiteralKeyVariable {
    const NAME: &'static str = "object literal key-variable field";

    fn parse<'a>(reader: &mut AstParser<'a>, _env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let ident = reader.parse_optional_token()?;
        Ok(Self { key: ident })
    }

    fn check(&self, _env: CheckingPhaseEnv, _errors: &mut ErrorCollector) {
        // N/A
    }
}

impl ItemWithSpan for SyObjectLiteralKeyVariable {
    fn span(&self) -> Span {
        self.key.span()
    }
}

/// Represents an object literal's spread field.
///
/// # Example
///
/// ```no_run
/// ...spreadfields
/// ```
#[derive(Debug, Clone)]
pub struct SyObjectLiteralSpread {
    pub spread: TkEpsilon,
    pub fields: Box<Attempted<SyExpression>>,
}

impl AstItem for SyObjectLiteralSpread {
    const NAME: &'static str = "object literal spread field";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let spread = reader.parse_optional_token()?;
        let fields = reader.parse_required(env.inside_nested_expr());

        Ok(Self {
            spread,
            fields: Box::new(fields),
        })
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        if let Ok(fields) = &*self.fields {
            fields.check(env.inside_nested_expr(), errors);
        }
    }
}

impl ItemWithSpan for SyObjectLiteralSpread {
    fn span(&self) -> Span {
        self.spread.span().join(&self.fields.span())
    }
}

/// Represents an object literal's computed key expression.
///
/// # Example
///
/// ```no_run
/// [ident]: value
/// ```
#[derive(Debug, Clone)]
pub struct SyObjectLiteralComputedKey {
    pub key: Grouped<TkBrackets, Attempted<Box<SyExpression>>>,
    pub colon: Attempted<TkColon>,
    pub value_expression: Box<Attempted<SyExpression>>,
}

impl AstItem for SyObjectLiteralComputedKey {
    const NAME: &'static str = "object literal computed key expression";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let key = reader.parse_optional_group_tolerant_inner::<TkBrackets, SyExpression>(
            env.inside_nested_expr(),
        )?;
        let colon = reader.parse_required_token();
        let value_expression = reader.parse_required(env.inside_nested_expr());

        Ok(Self {
            key: key.map_inner(|key| key.map(Box::new)),
            colon,
            value_expression: Box::new(value_expression),
        })
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        if let Ok(expression) = &self.key.inner {
            expression.check(env.inside_nested_expr(), errors);
        }
        if let Ok(expression) = &*self.value_expression {
            expression.check(env.inside_nested_expr(), errors);
        }
    }
}

impl ItemWithSpan for SyObjectLiteralComputedKey {
    fn span(&self) -> Span {
        self.key
            .group_token
            .span()
            .join(&self.colon.span())
            .join(&self.value_expression.span())
    }
}
