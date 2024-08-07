use crate::lang::{
    ast::{
        helpers::*,
        items::SyBody,
        linked_items::{
            Li2ExpressionStatement, Li2ExpressionStatementKind, Li2PrimitiveLiteral,
            Li2PrimitiveLiteralKind, StatementId,
        },
        linking::{
            ident_finder::{LinkingIdentFinder, LinkingIdentKind},
            FunctionBuilder, FunctionExpression, FunctionLinkingCompilation,
        },
    },
    tokens::*,
    CompilerError,
};

mod obj_literal;
pub use obj_literal::*;

mod unary;
pub use unary::*;

mod binary;
pub use binary::*;

mod if_statement;
pub use if_statement::*;

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
    IntLiteral(SyIntLiteral),
    FloatLiteral(SyFloatLiteral),
    ObjectLiteral(SyObjectLiteral),
    Parentheses(SyParenthesizedExpr),
    Binary(SyBinary),
    Invalid,
}

impl SyExpression {
    pub fn needs_semicolon(&self) -> bool {
        match self {
            Self::VarRead(_) => true,
            Self::StringLiteral(_) => true,
            Self::IntLiteral(_) => true,
            Self::FloatLiteral(_) => true,
            Self::ObjectLiteral(_) => true,
            Self::Parentheses(_) => true,
            Self::Binary(_) => true,
            Self::Invalid => false,
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
            macro_rules! try_parse {
                ($expr:ident) => {
                    if let Ok(expr) = reader.parse_optional(env) {
                        return Ok(SyExpression::$expr(expr));
                    }
                };
            }

            try_parse!(VarRead);
            try_parse!(StringLiteral);
            try_parse!(IntLiteral);
            try_parse!(FloatLiteral);
            try_parse!(ObjectLiteral);
            try_parse!(Parentheses);

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
}

impl ItemWithSpan for SyExpression {
    fn span(&self) -> Span {
        match self {
            Self::VarRead(expr) => expr.span(),
            Self::StringLiteral(expr) => expr.span(),
            Self::IntLiteral(expr) => expr.span(),
            Self::FloatLiteral(expr) => expr.span(),
            Self::ObjectLiteral(expr) => expr.span(),
            Self::Parentheses(expr) => expr.span(),
            Self::Binary(expr) => expr.span(),
            Self::Invalid => Span::new_empty(),
        }
    }
}

impl FunctionExpression for SyExpression {
    fn link_expression(
        &self,
        builder: &mut FunctionBuilder,
        ctx: &mut FunctionLinkingCompilation,
    ) -> StatementId {
        match self {
            Self::VarRead(var) => var.link_expression(builder, ctx),
            Self::StringLiteral(string) => todo!(),
            Self::IntLiteral(int) => int.link_expression(builder, ctx),
            Self::FloatLiteral(float) => todo!(),
            Self::ObjectLiteral(obj) => todo!(),
            Self::Parentheses(expr) => todo!(),
            Self::Binary(binary) => binary.link_expression(builder, ctx),
            Self::Invalid => todo!(),
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
}

impl ItemWithSpan for SyVarRead {
    fn span(&self) -> Span {
        self.name.span()
    }
}

impl FunctionExpression for SyVarRead {
    fn link_expression(
        &self,
        builder: &mut FunctionBuilder,
        ctx: &mut FunctionLinkingCompilation,
    ) -> StatementId {
        let found_value = builder.idents.find_ident(&self.name.ident);
        let LinkingIdentKind::Variable(variable) = found_value else {
            ctx.errors
                .push(CompilerError::new("Variable not found", self.name.span()));
            return builder.add_statement(Li2ExpressionStatement {
                kind: Li2ExpressionStatementKind::Unknown,
                span: Some(self.span()),
            });
        };

        builder.add_statement(Li2ExpressionStatement {
            kind: Li2ExpressionStatementKind::ReadVar { source: variable },
            span: Some(self.span()),
        })
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
}

impl ItemWithSpan for SyStringLiteral {
    fn span(&self) -> Span {
        self.literal.span()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SyIntLiteral {
    pub literal: TkInteger,
}

impl AstItem for SyIntLiteral {
    const NAME: &'static str = "integer literal";

    fn parse<'a>(reader: &mut AstParser<'a>, _env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let literal = reader.parse_optional_token()?;

        Ok(Self { literal })
    }
}

impl ItemWithSpan for SyIntLiteral {
    fn span(&self) -> Span {
        self.literal.span()
    }
}

impl FunctionExpression for SyIntLiteral {
    fn link_expression(
        &self,
        builder: &mut FunctionBuilder,
        _ctx: &mut FunctionLinkingCompilation,
    ) -> StatementId {
        let literal = Li2PrimitiveLiteral {
            span: Some(self.span()),
            kind: Li2PrimitiveLiteralKind::Integer {
                value: self.literal.clone(),
            },
        };

        builder.add_statement(Li2ExpressionStatement {
            kind: Li2ExpressionStatementKind::PrimitiveLiteral { literal },
            span: Some(self.span()),
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SyFloatLiteral {
    pub literal: TkFloat,
}

impl AstItem for SyFloatLiteral {
    const NAME: &'static str = "float literal";

    fn parse<'a>(reader: &mut AstParser<'a>, _env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let literal = reader.parse_optional_token()?;

        Ok(Self { literal })
    }
}

impl ItemWithSpan for SyFloatLiteral {
    fn span(&self) -> Span {
        self.literal.span()
    }
}

#[derive(Debug, Clone)]
pub struct SyParenthesizedExpr {
    pub parens: Grouped<TkParens, Attempted<Box<SyExpression>>>,
}

impl AstItem for SyParenthesizedExpr {
    const NAME: &'static str = "parenthesized expression";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let parens = reader.parse_optional_group_tolerant_inner(env)?;

        Ok(Self {
            parens: parens.map_inner(|expression| expression.map(Box::new)),
        })
    }
}

impl ItemWithSpan for SyParenthesizedExpr {
    fn span(&self) -> Span {
        self.parens.span()
    }
}
