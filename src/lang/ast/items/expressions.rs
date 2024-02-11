use crate::lang::{ast::helpers::*, tokens::*, CompilerError, ErrorCollector};

mod obj_literal;
pub use obj_literal::*;
mod unary;
pub use unary::*;
mod binary;
pub use binary::*;

trait ExpressionBottomUpParse {
    /// Parse an expression from the bottom up. Returns Err if the
    /// expression was parsed, Ok if it was not.
    /// TODO: Make this more clear with the Try trait in the future
    fn parse_bottom_up<'a>(
        expression: SyExpression,
        reader: &mut AstParser<'a>,
        env: ParsingPhaseEnv,
    ) -> Result<SyExpression, SyExpression>
    where
        Self: Sized;
}

#[derive(Debug, Clone)]
pub enum SyExpression {
    VarRead(SyVarRead),
    StringLiteral(SyStringLiteral),
    ObjectLiteral(SyObjectLiteral),
    Binary(SyBinary),
}

impl SyExpression {
    pub fn needs_semicolon(&self) -> bool {
        match self {
            Self::VarRead(_) => true,
            Self::StringLiteral(_) => true,
            Self::ObjectLiteral(_) => true,
            Self::Binary(_) => true,
        }
    }
}

impl AstItem for SyExpression {
    const NAME: &'static str = "expression";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let is_inside_binary = env.inside_binary_expr;
        let env = env.outside_binary_expr();

        fn parse_expression_beginning<'a>(
            reader: &mut AstParser<'a>,
            env: ParsingPhaseEnv,
        ) -> ParseResult<SyExpression> {
            if let Ok(expr) = reader.parse_optional(env) {
                return Ok(SyExpression::VarRead(expr));
            }
            if let Ok(expr) = reader.parse_optional(env) {
                return Ok(SyExpression::StringLiteral(expr));
            }
            if let Ok(expr) = reader.parse_optional(env) {
                return Ok(SyExpression::ObjectLiteral(expr));
            }

            Err(ParseError::NoMatch)
        }

        fn process_bottom_up<'a>(
            expression: SyExpression,
            reader: &mut AstParser<'a>,
            env: ParsingPhaseEnv,
            inside_binary: bool,
        ) -> Result<SyExpression, SyExpression> {
            let expression = if !inside_binary {
                SyBinary::parse_bottom_up(expression, reader, env)?
            } else {
                expression
            };

            Ok(expression)
        }

        // First, parse the beginning top-down
        let beginning = parse_expression_beginning(reader, env)?;

        // Then, repeatedly attempt parsing bottom up until no more can be parsed
        let mut expression = beginning;
        loop {
            match process_bottom_up(expression, reader, env, is_inside_binary) {
                Ok(unchanged_expression) => {
                    expression = unchanged_expression;
                    break;
                }
                Err(new_expression) => {
                    expression = new_expression;
                }
            }
        }

        Ok(expression)
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        match self {
            Self::VarRead(expr) => expr.check(env, errors),
            Self::StringLiteral(expr) => expr.check(env, errors),
            Self::ObjectLiteral(expr) => expr.check(env, errors),
            Self::Binary(expr) => expr.check(env, errors),
        }
    }
}

impl ItemWithSpan for SyExpression {
    fn span(&self) -> Span {
        match self {
            Self::VarRead(expr) => expr.span(),
            Self::StringLiteral(expr) => expr.span(),
            Self::ObjectLiteral(expr) => expr.span(),
            Self::Binary(expr) => expr.span(),
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
pub struct SyVarRead {
    pub name: TkIdent,
}

impl AstItem for SyVarRead {
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

impl ItemWithSpan for SyVarRead {
    fn span(&self) -> Span {
        self.name.span()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SyStringLiteral {
    pub literal: TkString,
}

impl AstItem for SyStringLiteral {
    const NAME: &'static str = "string literal";

    fn parse<'a>(reader: &mut AstParser<'a>, _env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let literal = reader.parse_optional_token()?;

        Ok(Self { literal })
    }

    fn check(&self, _env: CheckingPhaseEnv, _errors: &mut ErrorCollector) {
        // N/A
    }
}

impl ItemWithSpan for SyStringLiteral {
    fn span(&self) -> Span {
        self.literal.span()
    }
}
