use crate::lang::{ast::helpers::*, tokens::*};

use super::*;

/// Represents the contents of a body (inside {} braces), basically a list of statements.
///
/// # Example
///
/// ```no_run
///     type Result = {
///         ...Val,
///         [Name]: string,
///     };
///
///     let a = 1;
///
///     Result
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct SBody {
    pub statements: Vec<Attempted<SBodyExpression>>,
}

impl AstItem for SBody {
    const NAME: &'static str = "body";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let mut statements = Vec::new();
        reader.set_error_recovery_mode(ErrorRecoveryMode::until_token::<TSemicolon>());

        while !reader.is_empty() {
            let statement = reader.parse_required(env.outside_nested_expr());

            // TODO: Probably not needed anymore?
            // if let Ok(statement) = &statement {
            //     if !matches!(&statement, SExpression::TerminatedExpr(_)) && !reader.is_empty() {
            //         let span = reader.search_until_token::<TBlockLineEndSearch>();
            //         reader.add_error(CompilerError::new("Expected ;", span));
            //     }
            // }

            statements.push(statement);
        }

        Ok(Self { statements })
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        for statement in &self.statements {
            if let Ok(statement) = statement {
                statement.check(env, errors);
            }
        }
    }
}

/// Represents an expression with a semicolon at the end.
///
/// # Example
///
/// ```no_run
/// let a = 1;
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct SBodyExpression {
    pub expr: Box<SExpression>,

    // Result of the semicolon or the span where the semicolon should be
    pub semicolon: Result<TSemicolon, Span>,
}

impl AstItem for SBodyExpression {
    const NAME: &'static str = "body expression";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let expr = reader.parse_required(env.outside_nested_expr())?;
        let semicolon = reader.parse_optional_token::<TSemicolon>();

        let semicolon = match semicolon {
            Ok(semicolon) => Ok(semicolon),
            Err(_) => {
                let span = reader.input().span().clone();
                Err(span)
            }
        };

        Ok(Self {
            expr: Box::new(expr),
            semicolon,
        })
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        self.expr.check(env.outside_nested_expr(), errors);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SDeclarationBody {
    pub statements: Vec<Attempted<SDeclarationBodyItem>>,
}

impl AstItem for SDeclarationBody {
    const NAME: &'static str = "declaration body";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let mut statements = Vec::new();
        reader.set_error_recovery_mode(ErrorRecoveryMode::until_end());

        while !reader.is_empty() {
            let statement = reader.parse_required(env.outside_nested_expr());
            statements.push(statement);
        }

        Ok(Self { statements })
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        for statement in &self.statements {
            if let Ok(statement) = statement {
                statement.check(env, errors);
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SDeclarationPubVisibility {
    pub public: TPub,
}

impl AstItem for SDeclarationPubVisibility {
    const NAME: &'static str = "declaration visibility";

    fn parse<'a>(reader: &mut AstParser<'a>, _env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let public = reader.parse_optional_token::<TPub>()?;
        Ok(Self { public })
    }

    fn check(&self, _env: CheckingPhaseEnv, _errors: &mut ErrorCollector) {
        // N/A
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SDeclarationBodyItem {
    pub visibility: Option<SDeclarationPubVisibility>,
    pub item: Attempted<SDeclaration>,
}

impl AstItem for SDeclarationBodyItem {
    const NAME: &'static str = "declaration";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let visibility = reader.parse_optional::<SDeclarationPubVisibility>(env);
        let visibility = match visibility {
            Ok(visibility) => Some(visibility),
            Err(ParseError::NoMatch) => None,
            Err(ParseError::Error) => return Err(ParseError::Error),
        };

        let item = reader.parse_required(env);

        Ok(Self { visibility, item })
    }

    fn check(&self, env: CheckingPhaseEnv, errors: &mut ErrorCollector) {
        if let Ok(item) = &self.item {
            item.check(env, errors);
        }
    }
}
