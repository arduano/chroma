use ident_finder::{LinkingIdentFinder, LinkingIdentKind};

use crate::lang::{
    solver::{ItemSet, ModItemSet},
    tokens::{ItemWithSpan, Span, TkIdent},
    CompilerError, ErrorCollector,
};

use super::{helpers::Attempted, items::SyFunction, linked_items::*};

pub mod ident_finder;

pub struct FunctionLinkingCompilation<'a> {
    // pub functions: &'a mut ModItemSet<Li2ExpressionFunction>,
    pub errors: &'a mut ErrorCollector,
}

pub fn parse_function_ast(
    ast: &SyFunction,
    inherited_idents: Vec<&LinkingIdentFinder>,
    function_linking_compilation: &mut FunctionLinkingCompilation,
) -> Li2ExpressionFunction {
    let name = ast.signature.name.clone().ok();
    let span = ast.span();

    let mut arguments = ItemSet::new();
    let type_arguments = ItemSet::new();

    let ident_finder = LinkingIdentFinder::new_with_inherited(inherited_idents);
    let mut builder = FunctionBuilder::new(ident_finder);

    if let Ok(args) = &ast.signature.args {
        let args = &args.inner.args;
        for arg in args.iter() {
            if let Ok(arg) = &arg {
                // Insert the argument
                let arg_id = arguments.add_value(Li2Argument {
                    span: Some(arg.span()),
                });

                // Create a variable for the argument
                let var_id = builder.register_variable(arg.name.clone(), Some(arg.name.span()));
                builder.add_statement(Li2ExpressionStatement {
                    kind: Li2ExpressionStatementKind::WriteVar {
                        destination: var_id,
                        value: Li2ValueSource::Argument(arg_id),
                    },
                    span: Some(arg.span()),
                });
            }
        }
    }

    let body = &ast.body.as_ref().unwrap().inner;
    body.link_statement(&mut builder, function_linking_compilation);

    Li2ExpressionFunction {
        name,
        span: Some(span),
        arguments,
        type_arguments,
        variables: builder.variables,
        blocks: builder.blocks,
    }
}

pub trait FunctionExpression {
    fn link_expression(
        &self,
        builder: &mut FunctionBuilder,
        ctx: &mut FunctionLinkingCompilation,
    ) -> StatementId;
}

pub trait FunctionStatement {
    fn link_statement(&self, builder: &mut FunctionBuilder, ctx: &mut FunctionLinkingCompilation);
}

impl<T: FunctionExpression> FunctionExpression for Attempted<T> {
    fn link_expression(
        &self,
        builder: &mut FunctionBuilder,
        ctx: &mut FunctionLinkingCompilation,
    ) -> StatementId {
        match self {
            Ok(expr) => expr.link_expression(builder, ctx),
            Err(_) => builder.add_statement(Li2ExpressionStatement {
                kind: Li2ExpressionStatementKind::Unknown,
                span: None,
            }),
        }
    }
}

pub struct FunctionBuilder<'a> {
    current_block: Option<BlockId>,
    variables: ItemSet<Li2Variable>,
    blocks: ItemSet<Li2ExpressionBlock>,
    pub idents: LinkingIdentFinder<'a>,
}

impl<'a> FunctionBuilder<'a> {
    pub fn new(ident_finder: LinkingIdentFinder<'a>) -> Self {
        let mut blocks = ItemSet::new();

        Self {
            current_block: None,
            variables: ItemSet::new(),
            blocks,
            idents: ident_finder,
        }
    }

    fn get_or_create_current_block(&mut self) -> (BlockId, &mut Li2ExpressionBlock) {
        if let Some(block_id) = self.current_block {
            let block = self.blocks.get_mut(block_id).unwrap();

            (block_id, block)
        } else {
            let block_id = self.blocks.add_value(Li2ExpressionBlock {
                statements: ItemSet::new(),
                end: Li2BlockEnd {
                    kind: Li2BlockEndKind::Unknown,
                    span: None,
                },
            });

            self.current_block = Some(block_id);

            let block = self.blocks.get_mut(block_id).unwrap();

            (block_id, block)
        }
    }

    pub fn add_statement(&mut self, statement: Li2ExpressionStatement) -> StatementId {
        let (block_id, current_block) = self.get_or_create_current_block();
        let id = current_block.statements.add_value(statement);
        StatementId::new(block_id, id)
    }

    pub fn add_unknown_statement(&mut self) -> StatementId {
        self.add_statement(Li2ExpressionStatement {
            kind: Li2ExpressionStatementKind::Unknown,
            span: None,
        })
    }

    pub fn add_unknown_statement_with_span(&mut self, span: Span) -> StatementId {
        self.add_statement(Li2ExpressionStatement {
            kind: Li2ExpressionStatementKind::Unknown,
            span: Some(span),
        })
    }

    pub fn register_variable(&mut self, name: TkIdent, span: Option<Span>) -> VariableId {
        let id = self.variables.add_value(Li2Variable {
            span,
            name: name.clone(),
        });

        self.idents
            .add_local_ident(name.ident, LinkingIdentKind::Variable(id));

        id
    }

    pub fn add_block(&mut self, end: Li2BlockEnd) -> BlockId {
        let id = self.blocks.add_value(Li2ExpressionBlock {
            statements: ItemSet::new(),
            end,
        });
        self.current_block = Some(id);
        id
    }

    pub fn finish_current_block_with(&mut self, end: Li2BlockEnd) {
        let (_, current_block) = self.get_or_create_current_block();
        current_block.end = end;
        self.current_block = None;
    }

    pub fn switch_block(&mut self, block: BlockId) {
        self.current_block = Some(block);
    }

    pub fn with_child_ident_scope<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.idents.push_child_scope();
        let result = f(self);
        self.idents.pop_child_scope();
        result
    }
}
