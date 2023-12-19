use crate::lang::{ast::helpers::*, tokens::*, CompilerError};

use super::SExpression;

/// Represents an object literal.
///
/// # Example
///
/// ```no_run
/// { field1: 1, field2, ...spreadfields, [ident]: "foo" }
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct SObjectLiteral {
    braces: TBraces,
    fields: SObjectLiteralFields,
}

impl AstItem for SObjectLiteral {
    const NAME: &'static str = "object literal";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let (braces, fields) = reader.parse_optional_group::<TBraces, SObjectLiteralFields>(env)?;
        Ok(Self { braces, fields })
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        self.fields.check(env.inside_nested_expr(), errors);
    }
}

/// Represents an object literal's contents.
///
/// # Example
///
/// ```no_run
/// field1: 1, field2, ...spreadfields, [ident]: "foo"
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct SObjectLiteralFields {
    fields: Vec<Attempted<SObjectLiteralField>>,
}

impl AstItem for SObjectLiteralFields {
    const NAME: &'static str = "object literal contents";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let mut fields = Vec::new();
        reader.set_error_recovery_mode(ErrorRecoveryMode::until_token::<TComma>());

        while !reader.is_empty() {
            let field = reader.parse_required(env.inside_nested_expr());

            let comma = reader.parse_optional_token::<TComma>();

            if comma.is_err() && !reader.is_empty() {
                let span = reader.search_until_token::<TDataLineEndSearch>();
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

/// Represents an object literal's fields.
///
/// # Example
///
/// Key-value: `field1: 1`
/// Key-variable: `field2`
/// Spread: `...spreadfields`
/// Computed key: `[ident]: "foo"`
#[derive(Debug, Clone, PartialEq)]
pub enum SObjectLiteralField {
    KeyValue(SObjectLiteralKeyValue),
    KeyVariable(SObjectLiteralKeyVariable),
    Spread(SObjectLiteralSpread),
    ComputedKey(SObjectLiteralComputedKey),
}

impl AstItem for SObjectLiteralField {
    const NAME: &'static str = "object literal field";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        if let Ok(expr) = reader.parse_optional(env) {
            return Ok(SObjectLiteralField::KeyValue(expr));
        }
        if let Ok(expr) = reader.parse_optional(env) {
            return Ok(SObjectLiteralField::KeyVariable(expr));
        }
        if let Ok(expr) = reader.parse_optional(env) {
            return Ok(SObjectLiteralField::Spread(expr));
        }
        if let Ok(expr) = reader.parse_optional(env) {
            return Ok(SObjectLiteralField::ComputedKey(expr));
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

/// Represents an object literal's key-value field.
///
/// # Example
///
/// ```no_run
/// field1: 1
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct SObjectLiteralKeyValue {
    key: TIdent,
    colon: TColon,
    value: Box<Attempted<SExpression>>,
}

impl AstItem for SObjectLiteralKeyValue {
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
            value: Box::new(value),
        })
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        if let Ok(value) = &*self.value {
            value.check(env.inside_nested_expr(), errors);
        }
    }
}

/// Represents an object literal's key-variable field.
///
/// # Example
///
/// ```no_run
/// field2
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct SObjectLiteralKeyVariable {
    key: TIdent,
}

impl AstItem for SObjectLiteralKeyVariable {
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

/// Represents an object literal's spread field.
///
/// # Example
///
/// ```no_run
/// ...spreadfields
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct SObjectLiteralSpread {
    spread: TEpsilon,
    fields: Box<Attempted<SExpression>>,
}

impl AstItem for SObjectLiteralSpread {
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

/// Represents an object literal's computed key expression.
///
/// # Example
///
/// ```no_run
/// [ident]: value
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct SObjectLiteralComputedKey {
    key_brackets: TBrackets,
    key_expression: Box<SExpression>,
    colon: Attempted<TColon>,
    value_expression: Box<Attempted<SExpression>>,
}

impl AstItem for SObjectLiteralComputedKey {
    const NAME: &'static str = "object literal computed key expression";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let (key_brackets, key_expression) =
            reader.parse_optional_group::<TBrackets, SExpression>(env.inside_nested_expr())?;
        let colon = reader.parse_required_token();
        let value_expression = reader.parse_required(env.inside_nested_expr());

        Ok(Self {
            key_brackets,
            key_expression: Box::new(key_expression),
            colon,
            value_expression: Box::new(value_expression),
        })
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        self.key_expression
            .check(env.inside_nested_expr().inside_type_only(), errors);
        if let Ok(expression) = &*self.value_expression {
            expression.check(env.inside_nested_expr(), errors);
        }
    }
}
