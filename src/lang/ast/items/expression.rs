use crate::lang::{ast::helpers::*, tokens::*, CompilerError};

mod obj_literal;
pub use obj_literal::*;

trait ExpressionBottomUpParse {
    /// Parse an expression from the bottom up. Returns Ok if the
    /// expression was parsed, Err if it was not.
    fn parse_bottom_up<'a>(
        expression: SExpression,
        reader: &mut AstParser<'a>,
        env: ParsingPhaseEnv,
    ) -> Result<SExpression, SExpression>
    where
        Self: Sized;
}

#[derive(Debug, Clone, PartialEq)]
pub enum SExpression {
    TerminatedExpr(STerminatedExpr),
    TypeDefine(STypeDefine),
    VarRead(SVarRead),
    ObjectLiteral(SObjectLiteral),
}

impl AstItem for SExpression {
    const NAME: &'static str = "expression";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let inner = parse_expression_inner(reader, env)?;
        let expression = process_bottom_up_until_settled(inner, reader, env);
        Ok(expression)
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        match self {
            Self::TypeDefine(expr) => expr.check(env, errors),
            Self::VarRead(expr) => expr.check(env, errors),
            Self::ObjectLiteral(expr) => expr.check(env, errors),
            Self::TerminatedExpr(expr) => {
                if env.inside_nested_expr {
                    errors.push(CompilerError::new(
                        "Unexpected semicolon",
                        expr.semicolon.span(),
                    ));
                }
                expr.check(env, errors)
            }
        }
    }
}

fn parse_expression_inner<'a>(
    reader: &mut AstParser<'a>,
    env: ParsingPhaseEnv,
) -> ParseResult<SExpression> {
    if let Ok(expr) = reader.parse_optional(env) {
        return Ok(SExpression::TypeDefine(expr));
    }
    if let Ok(expr) = reader.parse_optional(env) {
        return Ok(SExpression::VarRead(expr));
    }
    if let Ok(expr) = reader.parse_optional(env) {
        return Ok(SExpression::ObjectLiteral(expr));
    }

    Err(ParseError::NoMatch)
}

fn process_bottom_up_until_settled<'a>(
    mut expression: SExpression,
    reader: &mut AstParser<'a>,
    env: ParsingPhaseEnv,
) -> SExpression {
    loop {
        match process_bottom_up(expression, reader, env) {
            Ok(unchanged_expression) => {
                return unchanged_expression;
            }
            Err(new_expression) => {
                expression = new_expression;
            }
        }
    }
}

fn process_bottom_up<'a>(
    expression: SExpression,
    reader: &mut AstParser<'a>,
    env: ParsingPhaseEnv,
) -> Result<SExpression, SExpression> {
    let expression = STerminatedExpr::parse_bottom_up(expression, reader, env)?;
    Ok(expression)
}

/// Represents an expression with a semicolon at the end.
///
/// # Example
///
/// ```no_run
/// let a = 1;
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct STerminatedExpr {
    pub expr: Box<SExpression>,
    pub semicolon: TSemicolon,
}

impl AstItem for STerminatedExpr {
    const NAME: &'static str = "terminated expression";

    fn parse<'a>(_reader: &mut AstParser<'a>, _env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        unreachable!("This should never be called directly");
    }

    fn check(&self, _env: CheckingPhaseEnv, _errors: &mut ErrorCollector) {
        // N/A
    }
}

impl ExpressionBottomUpParse for STerminatedExpr {
    fn parse_bottom_up<'a>(
        expression: SExpression,
        reader: &mut AstParser<'a>,
        env: ParsingPhaseEnv,
    ) -> Result<SExpression, SExpression>
    where
        Self: Sized,
    {
        // If the expression is already terminated, don't do anything.
        if let SExpression::TerminatedExpr(_) = &expression {
            return Ok(expression);
        }
        if env.inside_nested_expr {
            return Ok(expression);
        }

        let semicolon = reader.parse_optional_token::<TSemicolon>();
        if let Ok(semicolon) = semicolon {
            Err(SExpression::TerminatedExpr(STerminatedExpr {
                expr: Box::new(expression),
                semicolon,
            }))
        } else {
            Ok(expression)
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
    pub eq_token: TAssign,
    pub value: Box<Attempted<SExpression>>,
}

impl AstItem for STypeDefine {
    const NAME: &'static str = "type define";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let type_token = reader.parse_optional_token()?;
        let name = reader.parse_optional_token()?;
        let eq_token = reader.parse_optional_token()?;
        let value = reader.parse_required(env.inside_nested_expr());

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

    fn parse<'a>(reader: &mut AstParser<'a>, _env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let name = reader.parse_optional_token()?;

        Ok(Self { name })
    }

    fn check(&self, _env: CheckingPhaseEnv, _errors: &mut ErrorCollector) {
        // N/A
    }
}
