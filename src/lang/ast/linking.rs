use crate::lang::{
    solver::{ItemSet, ModItemSet},
    tokens::{ItemWithSpan, Span, TkIdent},
    CompilerError, ErrorCollector,
};

use super::{helpers::Attempted, items::SyFunction, linked_items::*};

pub mod ident_finder;

pub struct FunctionLinkingCompilation<'a> {
    functions: &'a mut ModItemSet<Li2ExpressionFunction>,
    errors: &'a mut ErrorCollector,
}

pub fn parse_function_ast(ast: &SyFunction) {
    let name = ast.signature.name.clone().ok();
    let span = ast.span();

    let mut arguments = ItemSet::new();
    let mut type_arguments = ItemSet::new();

    for arg in ast.signature.args.iter() {
        let arg = &arg.inner;
        let id = arguments.add_value(Li2Argument {
            span: Some(arg.span()),
        });
        // type_arguments.add_value(id);
    }

    let mut builder = FunctionBuilder::new();

    let function = Li2ExpressionFunction {
        name,
        span: Some(span),
        arguments,
        type_arguments,
        variables: builder.variables,
        blocks: builder.blocks,
    };
}

pub trait FunctionExpression {
    fn link_expression(
        &self,
        builder: &mut FunctionBuilder,
        ctx: &mut FunctionLinkingCompilation,
    ) -> StatementId;
}

impl<T: FunctionExpression> FunctionExpression for Attempted<T> {
    fn link_expression(
        &self,
        builder: &mut FunctionBuilder,
        ctx: &mut FunctionLinkingCompilation,
    ) -> StatementId {
        match self {
            Ok(expr) => expr.link_expression(builder, ctx),
            Err(err) => builder.add_statement(Li2ExpressionStatement {
                kind: Li2ExpressionStatementKind::Unknown,
                span: None,
            }),
        }
    }
}

pub struct FunctionBuilder {
    current_block: BlockId,
    variables: ItemSet<Li2Variable>,
    blocks: ItemSet<Li2ExpressionBlock>,
}

impl FunctionBuilder {
    pub fn new() -> Self {
        let mut blocks = ItemSet::new();
        let starting_id = blocks.add_value(Li2ExpressionBlock {
            statements: ItemSet::new(),
            end: Li2BlockEnd {
                kind: Li2BlockEndKind::Unknown,
                span: None,
            },
        });

        Self {
            current_block: starting_id,
            variables: ItemSet::new(),
            blocks: ItemSet::new(),
        }
    }

    pub fn add_statement(&mut self, statement: Li2ExpressionStatement) -> StatementId {
        let current_block = self.blocks.get_mut(self.current_block).unwrap();
        let id = current_block.statements.add_value(statement);
        StatementId::new(self.current_block, id)
    }

    pub fn add_variable(&mut self, name: TkIdent, span: Option<Span>) -> VariableId {
        let id = self.variables.add_value(Li2Variable { span, name });
        id
    }

    pub fn add_block(&mut self, end: Li2BlockEnd) -> BlockId {
        let id = self.blocks.add_value(Li2ExpressionBlock {
            statements: ItemSet::new(),
            end,
        });
        self.current_block = id;
        id
    }

    pub fn set_block_end(&mut self, end: Li2BlockEnd) {
        let current_block = self.blocks.get_mut(self.current_block).unwrap();
        current_block.end = end;
    }

    pub fn switch_block(&mut self, block: BlockId) {
        self.current_block = block;
    }
}
