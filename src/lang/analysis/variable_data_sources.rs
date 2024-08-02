use std::collections::{HashMap, HashSet};

use crate::lang::ast::linked_items::{Li2ExpressionFunction, StatementId};

pub struct AnVariableDataSources {
    variable_data_sources: HashMap<StatementId, HashSet<StatementId>>,
}

// impl AnVariableDataSources {
//     pub fn analyze_variable_data_sources_for(&mut self, function: &Li2ExpressionFunction) -> Self {
//         let mut variable_data_sources = HashMap::new();
//     }
// }
