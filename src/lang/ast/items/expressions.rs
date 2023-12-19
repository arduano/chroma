use crate::lang::{ast::helpers::*, tokens::*};

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
    VarRead(SVarRead),
    ObjectLiteral(SObjectLiteral),
}

impl SExpression {
    pub fn needs_semicolon(&self) -> bool {
        match self {
            Self::VarRead(_) => true,
            Self::ObjectLiteral(_) => false,
        }
    }
}

impl AstItem for SExpression {
    const NAME: &'static str = "expression";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        fn parse_expression_beginning<'a>(
            reader: &mut AstParser<'a>,
            env: ParsingPhaseEnv,
        ) -> ParseResult<SExpression> {
            if let Ok(expr) = reader.parse_optional(env) {
                return Ok(SExpression::VarRead(expr));
            }
            if let Ok(expr) = reader.parse_optional(env) {
                return Ok(SExpression::ObjectLiteral(expr));
            }

            Err(ParseError::NoMatch)
        }

        fn process_bottom_up<'a>(
            expression: SExpression,
            _reader: &mut AstParser<'a>,
            _env: ParsingPhaseEnv,
        ) -> Result<SExpression, SExpression> {
            // let expression = STerminatedExpr::parse_bottom_up(expression, reader, env)?;
            Ok(expression)
        }

        // First, parse the beginning top-down
        let beginning = parse_expression_beginning(reader, env)?;

        // Then, repeatedly attempt parsing bottom up until no more can be parsed
        let mut expression = beginning;
        loop {
            match process_bottom_up(expression, reader, env) {
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
            Self::ObjectLiteral(expr) => expr.check(env, errors),
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
