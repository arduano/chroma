use core::panic;
use std::collections::{HashMap, VecDeque};
use std::hash::Hash;

use super::*;

// 1. Calculate forward jumps HashMap<BlockId, Vec<BlockId>>
// 2. Calculate forward DFS HashMap<BlockId, JumpsWithDfs>]
// 2. Calculate forward Backwards jumps HashMap<BlockId, JumpsWithDfs>

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnBlockJumps<Id> {
    /// The block IDs that jump to this block.
    jumps_in: Vec<Id>,
    /// The block IDs that this block jumps to.
    jumps_out: Vec<Id>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnLinksOnBlock<Id> {
    /// Jumps that go forwards. They do not induce cycles in the block graph.
    forwards: AnBlockJumps<Id>,
    /// Jumps that go backwards, caused by things like loops. They cause cycles.
    cyclic: AnBlockJumps<Id>,

    forwards_dfs: Vec<Id>,
    backward_dfs: Vec<Id>,
}

#[derive(Debug, Clone)]
pub struct AnBlockLinks<Id> {
    block_relations: HashMap<Id, AnLinksOnBlock<Id>>,
}

impl AnBlockLinks<()> {
    pub fn analyze_block_links_for<Container: AnLogicBlockContainer>(
        container: &Container,
    ) -> AnBlockLinks<Container::BlockId> {
        let mut out_forwards_jumps = HashMap::new();
        let mut out_cyclic_jumps = HashMap::new();
        let mut in_forwards_jumps = HashMap::new();
        let mut in_cyclic_jumps = HashMap::new();

        for (block_id, block) in container.iter_blocks() {
            let block_id = block_id;
            let forward_out_jumps = block.forward_out_jumps_as_vec();
            let cyclic_out_jumps = block.cyclic_out_jumps_as_vec();

            in_forwards_jumps.entry(block_id).or_insert_with(Vec::new);
            in_cyclic_jumps.entry(block_id).or_insert_with(Vec::new);

            for jump in &forward_out_jumps {
                in_forwards_jumps
                    .entry(*jump)
                    .or_insert_with(Vec::new)
                    .push(block_id);
            }
            for jump in &cyclic_out_jumps {
                in_cyclic_jumps
                    .entry(*jump)
                    .or_insert_with(Vec::new)
                    .push(block_id);
            }

            out_forwards_jumps.insert(block_id, forward_out_jumps);
            out_cyclic_jumps.insert(block_id, cyclic_out_jumps);
        }

        dbg!(&out_forwards_jumps);
        dbg!(&in_forwards_jumps);

        let mut forward_dfs = calculate_dfs_on(&out_forwards_jumps);
        let mut backward_dfs = calculate_dfs_on(&in_forwards_jumps);

        // Build the final map

        let mut block_relations = HashMap::new();
        for (block_id, _) in container.iter_blocks() {
            block_relations.insert(
                block_id,
                AnLinksOnBlock {
                    forwards: AnBlockJumps {
                        jumps_in: in_forwards_jumps.remove(&block_id).unwrap(),
                        jumps_out: out_forwards_jumps.remove(&block_id).unwrap(),
                    },
                    cyclic: AnBlockJumps {
                        jumps_in: in_cyclic_jumps.remove(&block_id).unwrap(),
                        jumps_out: out_cyclic_jumps.remove(&block_id).unwrap(),
                    },
                    forwards_dfs: forward_dfs.remove(&block_id).unwrap().to_vec(),
                    backward_dfs: backward_dfs.remove(&block_id).unwrap().to_vec(),
                },
            );
        }

        return AnBlockLinks { block_relations };
    }
}

fn topological_sort<Id: Eq + Copy + Hash + std::fmt::Debug>(
    graph: &HashMap<Id, Vec<Id>>,
) -> Vec<Id> {
    let mut in_degree = HashMap::new();
    let mut zero_in_degree = VecDeque::new();
    let mut sorted = Vec::new();

    // Initialize in-degree of all nodes
    for &node in graph.keys() {
        in_degree.insert(node, 0);
    }
    for edges in graph.values() {
        for &neighbor in edges {
            *in_degree.entry(neighbor).or_insert(0) += 1;
        }
    }

    // Collect nodes with zero in-degree
    for (&node, &degree) in &in_degree {
        if degree == 0 {
            zero_in_degree.push_back(node);
        }
    }

    // Process nodes with zero in-degree
    while let Some(node) = zero_in_degree.pop_front() {
        sorted.push(node);

        if let Some(neighbors) = graph.get(&node) {
            for &neighbor in neighbors {
                let degree = in_degree
                    .get_mut(&neighbor)
                    .expect("Linked node not found in topological sort");
                *degree -= 1;
                if *degree == 0 {
                    zero_in_degree.push_back(neighbor);
                }
            }
        }
    }

    // If sorted contains all the nodes, return it, otherwise return an error
    if sorted.len() == graph.len() {
        return sorted;
    } else {
        panic!("Graph has at least one cycle during topological sort");
    }
}

fn calculate_dfs_on<Id: Eq + Copy + Hash + std::fmt::Debug>(
    out_jumps: &HashMap<Id, Vec<Id>>,
) -> HashMap<Id, Vec<Id>> {
    let stack = topological_sort(out_jumps);

    let mut new_dfs = HashMap::new();

    dbg!(&stack);

    // Iterate through the stack in reverse order, for each node combining all the DFS arrays of the child nodes.
    for id in stack.into_iter().rev() {
        let mut new_dfs_vec = Vec::new();
        let jumps = out_jumps.get(&id).unwrap();

        println!("ID: {:?} jumps {:?}", id, jumps);

        for jump in jumps {
            if new_dfs_vec.contains(jump) {
                continue;
            }

            new_dfs_vec.push(*jump);

            let child_dfs = new_dfs.get(jump).unwrap();

            for child in child_dfs {
                if !new_dfs_vec.contains(child) {
                    new_dfs_vec.push(*child);
                } else {
                    dbg!("Broke at", child);
                    dbg!(&child_dfs);
                    dbg!(&new_dfs_vec);
                    break;
                }
            }
        }

        new_dfs.insert(id, new_dfs_vec);
    }

    new_dfs
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dfs() {
        let out_jumps = HashMap::from([
            (1, vec![2]),
            (2, vec![3, 5]),
            (3, vec![4]),
            (4, vec![7]),
            (5, vec![6]),
            (6, vec![7]),
            (7, vec![]),
        ]);

        // Calculate DFS
        let dfs = calculate_dfs_on(&out_jumps);

        let expected_dfs = HashMap::from([
            (1, vec![2, 3, 4, 7, 5, 6]),
            (2, vec![3, 4, 7, 5, 6]),
            (3, vec![4, 7]),
            (4, vec![7]),
            (5, vec![6, 7]),
            (6, vec![7]),
            (7, vec![]),
        ]);

        assert_eq!(dfs, expected_dfs);
    }

    #[test]
    fn test_dfs_2() {
        let out_jumps = HashMap::from([(1, vec![2, 3]), (2, vec![3]), (3, vec![])]);

        // Calculate DFS
        let dfs = calculate_dfs_on(&out_jumps);

        let expected_dfs = HashMap::from([(1, vec![2, 3]), (2, vec![3]), (3, vec![])]);

        assert_eq!(dfs, expected_dfs);
    }
}
