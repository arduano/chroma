use crate::lang::ast::{
    linked_items::{Li2BlockEnd, Li2BlockEndKind, Li2Jump},
    linking::FunctionStatement,
};

use super::*;

#[derive(Debug, Clone)]
pub struct SyIfCondition {
    pub if_token: TkIf,
    pub condition: Grouped<TkParens, Attempted<Box<SyExpression>>>,
    pub code_block: Attempted<Grouped<TkBraces, SyBody>>,
    pub else_if_conditions: Vec<SyElseIfCondition>,
    pub else_condition: Option<SyElseCondition>,
}

impl SyIfCondition {
    fn iter_conditionals(&self) -> impl '_ + Iterator<Item = Conditional> {
        let first = std::iter::once(Conditional {
            condition: Some(&self.condition.inner),
            block: self
                .code_block
                .value_as_ref()
                .map(|code_block| &code_block.inner),
            jump_span: self.if_token.span(),
        });

        let middle = self
            .else_if_conditions
            .iter()
            .map(|else_if_condition| Conditional {
                condition: Some(&else_if_condition.condition.inner),
                block: else_if_condition
                    .code_block
                    .value_as_ref()
                    .map(|code_block| &code_block.inner),
                jump_span: else_if_condition
                    .if_token
                    .span()
                    .join(&else_if_condition.condition.span()),
            });

        let last = self
            .else_condition
            .iter()
            .map(|else_condition| Conditional {
                condition: None,
                block: else_condition
                    .code_block
                    .value_as_ref()
                    .map(|code_block| &code_block.inner),
                jump_span: else_condition.else_token.span(),
            });

        first.chain(middle).chain(last)
    }
}

impl AstItem for SyIfCondition {
    const NAME: &'static str = "if condition";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let if_token = reader.parse_optional_token()?;
        let condition = reader.parse_optional_group_tolerant_inner(env)?;
        let code_block = reader.parse_required_group(env.outside_nested_expr());

        let mut else_if_conditions = Vec::new();

        loop {
            let else_if_condition = reader.parse_optional(env);
            if let Ok(else_if_condition) = else_if_condition {
                else_if_conditions.push(else_if_condition);
            } else {
                break;
            }
        }

        let else_condition = reader.parse_optional(env).ok();

        Ok(Self {
            if_token,
            condition: condition.map_inner(|expression| expression.map(Box::new)),
            code_block,
            else_if_conditions,
            else_condition,
        })
    }
}

impl ItemWithSpan for SyIfCondition {
    fn span(&self) -> Span {
        let mut span = self.if_token.span();

        for else_if_condition in &self.else_if_conditions {
            span = span.join(&else_if_condition.span());
        }

        if let Some(else_condition) = &self.else_condition {
            span = span.join(&else_condition.span());
        }

        span.join(&self.code_block.span())
    }
}

struct Conditional<'a> {
    jump_span: Span,
    condition: Option<&'a Attempted<Box<SyExpression>>>,
    block: Attempted<&'a SyBody>,
}

impl FunctionStatement for SyIfCondition {
    fn link_statement(&self, builder: &mut FunctionBuilder, ctx: &mut FunctionLinkingCompilation) {
        let conditionals = self.iter_conditionals();

        let mut blocks_to_link_to_end = Vec::new();

        for conditional in conditionals {
            if let Some(condition) = conditional.condition {
                let condition = condition.link_expression(builder, ctx);
                let condition_block = builder.current_block();

                let then_block = builder.add_block();
                builder.switch_block(then_block);
                conditional.block.link_statement(builder, ctx);

                let then_block_end = builder.current_block();
                if let Some(then_block_end) = then_block_end {
                    blocks_to_link_to_end.push(then_block_end);
                }

                let else_block = builder.add_block();

                if let Some(condition_block) = condition_block {
                    builder.switch_block(condition_block);
                    builder.finish_current_block_with(Li2BlockEnd {
                        kind: Li2BlockEndKind::JumpIf {
                            cond: condition,
                            if_true: Li2Jump::new_forward(then_block),
                            if_false: Li2Jump::new_forward(else_block),
                        },
                        span: Some(conditional.jump_span),
                    });
                }

                builder.switch_block(else_block);
            } else {
                conditional.block.link_statement(builder, ctx);

                let then_block_end = builder.current_block();
                if let Some(then_block_end) = then_block_end {
                    blocks_to_link_to_end.push(then_block_end);
                }
            }
        }

        if blocks_to_link_to_end.len() > 0 {
            let final_block = builder.current_block_or_create();

            for block in blocks_to_link_to_end {
                builder.switch_block(block);
                builder.finish_current_block_with(Li2BlockEnd {
                    kind: Li2BlockEndKind::Jump {
                        to: Li2Jump::new_forward(final_block),
                    },
                    span: None,
                });
            }

            builder.switch_block(final_block);
        }
    }
}

#[derive(Debug, Clone)]
pub struct SyElseIfCondition {
    pub else_token: TkIf,
    pub if_token: TkIf,
    pub condition: Grouped<TkParens, Attempted<Box<SyExpression>>>,
    pub code_block: Attempted<Grouped<TkBraces, SyBody>>,
}

impl AstItem for SyElseIfCondition {
    const NAME: &'static str = "else if condition";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let else_token = reader.parse_optional_token()?;
        let if_token = reader.parse_optional_token()?;
        let condition = reader.parse_optional_group_tolerant_inner(env)?;
        let code_block = reader.parse_required_group(env.outside_nested_expr());

        Ok(Self {
            else_token,
            if_token,
            condition: condition.map_inner(|expression| expression.map(Box::new)),
            code_block: code_block,
        })
    }
}

impl ItemWithSpan for SyElseIfCondition {
    fn span(&self) -> Span {
        self.else_token
            .span()
            .join(&self.if_token.span())
            .join(&self.condition.span())
            .join(&self.code_block.span())
    }
}

#[derive(Debug, Clone)]
pub struct SyElseCondition {
    pub else_token: TkElse,
    pub code_block: Attempted<Grouped<TkBraces, SyBody>>,
}

impl AstItem for SyElseCondition {
    const NAME: &'static str = "else condition";

    fn parse<'a>(reader: &mut AstParser<'a>, env: ParsingPhaseEnv) -> ParseResult<Self>
    where
        Self: Sized,
    {
        let else_token = reader.parse_optional_token()?;
        let code_block = reader.parse_required_group(env.outside_nested_expr());

        Ok(Self {
            else_token,
            code_block,
        })
    }
}

impl ItemWithSpan for SyElseCondition {
    fn span(&self) -> Span {
        self.else_token.span().join(&self.code_block.span())
    }
}
