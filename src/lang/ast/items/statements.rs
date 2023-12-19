use super::*;
use crate::lang::ast::helpers::*;

pub enum SemiRequirement {
    Never,
    Expression,
    Always,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SStatement {
    Declaration(SDeclaration),
    Expression(SExpression),
}

impl SStatement {
    pub fn needs_semicolon(&self) -> SemiRequirement {
        match self {
            SStatement::Declaration(decl) => {
                if decl.needs_semicolon() {
                    SemiRequirement::Always
                } else {
                    SemiRequirement::Never
                }
            }
            SStatement::Expression(expr) => {
                if expr.needs_semicolon() {
                    SemiRequirement::Expression
                } else {
                    SemiRequirement::Never
                }
            }
        }
    }
}

impl AstItem for SStatement {
    const NAME: &'static str = "expression";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        fn parse_expression_beginning<'a>(
            reader: &mut AstParser<'a>,
            env: ParsingPhaseEnv,
        ) -> ParseResult<SStatement> {
            if let Ok(expr) = reader.parse_optional(env) {
                return Ok(SStatement::Declaration(expr));
            }
            if let Ok(expr) = reader.parse_optional(env) {
                return Ok(SStatement::Expression(expr));
            }

            Err(ParseError::NoMatch)
        }

        fn process_bottom_up<'a>(
            expression: SStatement,
            _reader: &mut AstParser<'a>,
            _env: ParsingPhaseEnv,
        ) -> Result<SStatement, SStatement> {
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
            Self::Declaration(expr) => expr.check(env, errors),
            Self::Expression(expr) => expr.check(env.outside_nested_expr(), errors),
        }
    }
}
