use crate::tree::{NodeId, Tree};

/// A primitive tree representation designed for ease of rough assembly and conversion into `tree:Tree<T>` once assembled
#[derive(Debug, Clone)]
pub struct PrimNode<T> {
    pub val: T,
    pub children: Vec<Box<PrimNode<T>>>,
}

impl<T> PrimNode<T> {
    pub fn new(val: T) -> Self {
        Self {
            val,
            children: Vec::new(),
        }
    }
    pub fn new_with_children(val: T, children: Vec<PrimNode<T>>) -> Self {
        Self {
            val,
            children: children.into_iter().map(|c| Box::new(c)).collect(),
        }
    }
    fn add_to_tree(self, tree: &mut Tree<T>) -> NodeId {
        //Add `self` to the tree
        let self_id = tree.new_node(self.val);

        //Add every child of `self` to the tree and append them to `self`
        for c in self.children {
            let child_id = c.add_to_tree(tree);
            tree.append_to(self_id, child_id)
                .expect("Should never happen");
        }

        self_id
    }
}

impl<T> From<PrimNode<T>> for Tree<T> {
    fn from(head: PrimNode<T>) -> Self {
        let mut tree = Tree::new();
        head.add_to_tree(&mut tree);

        tree
    }
}
