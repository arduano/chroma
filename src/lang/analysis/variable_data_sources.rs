use std::collections::{HashMap, HashSet};

use crate::lang::ast::linked_items::{
    BlockId, Li2ExpressionFunction, Li2ExpressionStatementKind, StatementId, VariableId,
};

use super::AnBlockLinks;

#[derive(Debug, Clone)]
pub struct AnVariableDataSources {
    variable_data_sources: HashMap<StatementId, Vec<StatementId>>,
}

fn get_data_sources_for_variable_read(
    function: &Li2ExpressionFunction,
    block_links: &AnBlockLinks<BlockId>,
    statement_id: StatementId,
    variable_id: VariableId,
) -> Vec<StatementId> {
    let mut data_sources = Vec::new();

    let start_block_id = statement_id.group();

    let mut visited_blocks = Vec::new();
    let mut current = vec![start_block_id];
    let mut next = Vec::new();

    // BFS through all blocks
    while !current.is_empty() {
        for block_id in current.drain(..) {
            if visited_blocks.contains(&block_id) {
                continue;
            }
            visited_blocks.push(block_id);

            let block = function.blocks.get(block_id).unwrap();

            let mut reached_write = false;

            // If there's no statements in this block, don't bother
            if block.statements.len() != 0 {
                // Get start and end IDs
                let start_id = block.statements.first_id().unwrap();
                let end_id = if block_id == start_block_id {
                    // If this is the starting block, use the statement ID
                    statement_id.item()
                } else {
                    // Otherwise, use the last statement ID
                    block.statements.last_id().unwrap()
                };

                let end_index = block.statements.get_index_for_id(end_id);
                let start_index = block.statements.get_index_for_id(start_id);

                // Loop through all statements in the block in reverse, until we find a write to the variable
                for id_index in (start_index..=end_index).rev() {
                    let id = block.statements.id_for_index(id_index);

                    let statement = block.statements.get(id).unwrap();
                    if let Li2ExpressionStatementKind::WriteVar { destination, .. } =
                        &statement.kind
                    {
                        if *destination == variable_id {
                            reached_write = true;
                            data_sources.push(StatementId::new(block_id, id));
                            break;
                        }
                    }
                }
            }

            // End the BFS here if we reached a write
            if reached_write {
                continue;
            }

            // Otherwise, add all the blocks that are connected backwards to this block
            let block_links = block_links.for_block(block_id);
            let backwards_jumps = &block_links.forwards.jumps_in;
            for &jump in backwards_jumps {
                next.push(jump);
            }
        }

        // Swap the current and next vectors
        std::mem::swap(&mut current, &mut next);
    }

    data_sources
}

impl AnVariableDataSources {
    pub fn analyze_variable_data_sources_for(
        function: &Li2ExpressionFunction,
        block_links: &AnBlockLinks<BlockId>,
    ) -> Self {
        let mut variable_data_sources = HashMap::new();

        for (sid, statement) in function.iter_all_statements() {
            if let Li2ExpressionStatementKind::ReadVar { source } = &statement.kind {
                let data_sources =
                    get_data_sources_for_variable_read(function, block_links, sid, *source);

                variable_data_sources.insert(sid, data_sources);
            }
        }

        AnVariableDataSources {
            variable_data_sources,
        }
    }
}
