use std::sync::Arc;

use crate::lang::{
    ast::{
        helpers::*,
        linked_items::StatementId,
        linking::{
            ident_finder::LinkingIdentFinder, FunctionBuilder, FunctionExpression,
            FunctionLinkingCompilation, FunctionStatement,
        },
    },
    tokens::*,
    CompilerError,
};

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
#[derive(Debug, Clone)]
pub struct SyBody {
    pub statements: Vec<Attempted<SyBodyStatement>>,
}

impl AstItem for SyBody {
    const NAME: &'static str = "body";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let env = env.outside_nested_expr();

        let mut statements = Vec::new();
        reader.set_error_recovery_mode(ErrorRecoveryMode::until_token::<TkSemicolon>());

        while !reader.is_empty() {
            let statement = reader.parse_required::<SyBodyStatement>(env.outside_nested_expr());

            if let Ok(statement) = &statement {
                let semi_requirement = statement.statement.needs_semicolon();
                match semi_requirement {
                    SemiRequirement::Never => {
                        if let SySemicolon::Exists(semicolon) = &statement.semicolon {
                            reader.add_error(CompilerError::new(
                                "Unexpected ;",
                                semicolon.span().clone(),
                            ));
                        }
                    }
                    SemiRequirement::Expression => {
                        if !reader.is_empty() {
                            if let SySemicolon::Missing(span) = &statement.semicolon {
                                reader.add_error(CompilerError::new("Expected ;", span.clone()));
                            }
                        }
                    }
                    SemiRequirement::Always => {
                        if let SySemicolon::Missing(span) = &statement.semicolon {
                            reader.add_error(CompilerError::new("Expected ;", span.clone()));
                        }
                    }
                }
            }

            statements.push(statement);
        }

        Ok(Self { statements })
    }
}

impl ItemWithSpan for SyBody {
    fn span(&self) -> Span {
        self.statements.span()
    }
}

impl FunctionStatement for SyBody {
    fn link_statement(&self, builder: &mut FunctionBuilder, ctx: &mut FunctionLinkingCompilation) {
        builder.with_child_ident_scope(|builder| {
            for statement in self.statements.iter() {
                if let Ok(statement) = &statement {
                    statement.statement.link_statement(builder, ctx);
                }
            }
        });
    }
}

/// Represents a statement with a semicolon at the end.
///
/// # Example
///
/// ```no_run
/// let a = 1;
/// ```
#[derive(Debug, Clone)]
pub struct SyBodyStatement {
    pub statement: Box<SyStatement>,
    pub semicolon: SySemicolon,
}

impl AstItem for SyBodyStatement {
    const NAME: &'static str = "body statement";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let statement = reader.parse_required(env.outside_nested_expr())?;
        let semicolon = reader.parse_optional_token::<TkSemicolon>();

        let semicolon = match semicolon {
            Ok(semicolon) => SySemicolon::Exists(semicolon),
            Err(_) => {
                let span = reader.input().span().clone();
                SySemicolon::Missing(span)
            }
        };

        Ok(Self {
            statement: Box::new(statement),
            semicolon,
        })
    }
}

impl ItemWithSpan for SyBodyStatement {
    fn span(&self) -> Span {
        self.statement.span().join(&self.semicolon.span())
    }
}

#[derive(Debug, Clone)]
pub struct SyDeclarationBody {
    pub statements: Vec<Attempted<Arc<SyDeclarationBodyItem>>>,
}

impl AstItem for SyDeclarationBody {
    const NAME: &'static str = "declaration body";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let mut statements = Vec::new();
        reader.set_error_recovery_mode(ErrorRecoveryMode::until_token::<TkBlockLineEndSearch>());

        while !reader.is_empty() {
            let statement =
                reader.parse_required::<SyDeclarationBodyItem>(env.outside_nested_expr());

            if let Ok(statement) = &statement {
                let needs_semi = statement.item.needs_semicolon();
                match needs_semi {
                    false => {
                        if let SySemicolon::Exists(semicolon) = &statement.semicolon {
                            reader.add_error(CompilerError::new(
                                "Unexpected ;",
                                semicolon.span().clone(),
                            ));
                        }
                    }
                    true => {
                        if let SySemicolon::Missing(span) = &statement.semicolon {
                            reader.add_error(CompilerError::new("Expected ;", span.clone()));
                        }
                    }
                }
            }

            statements.push(statement.map(Arc::new));
        }

        Ok(Self { statements })
    }
}

impl ItemWithSpan for SyDeclarationBody {
    fn span(&self) -> Span {
        self.statements.span()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SyDeclarationPubVisibility {
    pub public: TkPub,
}

impl AstItem for SyDeclarationPubVisibility {
    const NAME: &'static str = "declaration visibility";

    fn parse<'a>(reader: &mut AstParser<'a>, _env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let public = reader.parse_optional_token::<TkPub>()?;
        Ok(Self { public })
    }
}

impl ItemWithSpan for SyDeclarationPubVisibility {
    fn span(&self) -> Span {
        self.public.span()
    }
}

#[derive(Debug, Clone)]
pub struct SyDeclarationBodyItem {
    pub visibility: Option<SyDeclarationPubVisibility>,
    pub item: SyDeclaration,
    pub semicolon: SySemicolon,
}

impl AstItem for SyDeclarationBodyItem {
    const NAME: &'static str = "body declaration item";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let visibility = reader.parse_optional::<SyDeclarationPubVisibility>(env);
        let visibility = match visibility {
            Ok(visibility) => Some(visibility),
            Err(ParseError::NoMatch) => None,
            Err(ParseError::Error) => return Err(ParseError::Error),
        };

        let item = reader.parse_optional(env)?;

        let semicolon = reader.parse_optional_token::<TkSemicolon>();

        let semicolon = match semicolon {
            Ok(semicolon) => SySemicolon::Exists(semicolon),
            Err(_) => {
                let span = reader.input().span().clone();
                SySemicolon::Missing(span)
            }
        };

        Ok(Self {
            visibility,
            item,
            semicolon,
        })
    }
}

impl ItemWithSpan for SyDeclarationBodyItem {
    fn span(&self) -> Span {
        self.visibility
            .span()
            .join(&self.item.span())
            .join(&self.semicolon.span())
    }
}
