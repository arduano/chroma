use super::*;
use crate::syntax::tokens::*;

#[derive(Debug, Clone, PartialEq)]
pub enum SExpression {
    TypeDefine(STypeDefine),
    VarRead(SVarRead),
}

impl AstItem for SExpression {
    const NAME: &'static str = "expression";

    fn parse<'a>(reader: &mut AstParser<'a>) -> ParseResult<Self>
    where
        Self: Sized,
    {
        if let Ok(expr) = reader.parse_optional() {
            return Ok(Self::TypeDefine(expr));
        }
        if let Ok(expr) = reader.parse_optional() {
            return Ok(Self::VarRead(expr));
        }

        Err(ParseError::NoMatch)
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        match self {
            Self::TypeDefine(expr) => expr.check(env, errors),
            Self::VarRead(expr) => expr.check(env, errors),
        }
    }
}

/// Represents a type declaration.
///
/// # Example
///
/// ```no_run
/// type Foo = { a: string }
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct STypeDefine {
    pub type_token: TType,
    pub name: TIdent,
    pub eq_token: TEquals,
    pub value: Box<Attempted<SExpression>>,
}

impl AstItem for STypeDefine {
    const NAME: &'static str = "type define";

    fn parse<'a>(reader: &mut AstParser<'a>) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let type_token = reader.parse_optional_token()?;
        let name = reader.parse_optional_token()?;
        let eq_token = reader.parse_optional_token()?;
        let value = reader.parse_required();

        Ok(Self {
            type_token,
            name,
            eq_token,
            value: Box::new(value),
        })
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        if let Ok(value) = &*self.value {
            value.check(env.inside_type_only().inside_nested_expr(), errors);
        }
    }
}

/// Represents a simple variable read.
///
/// # Example
///
/// ```no_run
/// varname
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct SVarRead {
    pub name: TIdent,
}

impl AstItem for SVarRead {
    const NAME: &'static str = "variable read";

    fn parse<'a>(reader: &mut AstParser<'a>) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let name = reader.parse_optional_token()?;

        Ok(Self { name })
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        // N/A
    }
}
