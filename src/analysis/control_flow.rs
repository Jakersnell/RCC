use crate::analysis::mlir::{
    MidLevelIR, MlirBlock, MlirExpr, MlirFunction, MlirStmt, MlirType, MlirTypeDecl, MlirTypeKind,
    VOID_TYPE,
};
use crate::util::str_intern::InternedStr;
use derive_new::new;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{write, Display, Formatter};
use std::hash::Hasher;
use std::ops::DerefMut;
use std::rc::Rc;

/*
Some resources on control flow analysis:

https://www.youtube.com/watch?v=yfGuxHCyZvs - Compiler Design: Basic Blocks and Flow Graphs
https://people.iith.ac.in/ramakrishna/fc5264/control-flow-analysis.pdf - Control Flow Analysis
 */

macro_rules! block {
    ($kind:expr) => {
        Rc::new(RefCell::new(BasicBlock::new($kind)))
    };
}

static mut BLOCK_COUNT: usize = 0;

#[derive(PartialEq, Hash, PartialOrd)]
pub struct BasicBlock<'a> {
    kind: BasicBlockKind,
    statements: Vec<&'a MlirStmt>,
    incoming: Vec<Rc<BasicBlockEdge<'a>>>,
    outgoing: Vec<Rc<BasicBlockEdge<'a>>>,
    id: usize,
}

impl<'a> BasicBlock<'a> {
    fn new(kind: BasicBlockKind) -> Self {
        let id = unsafe {
            BLOCK_COUNT += 1;
            BLOCK_COUNT
        };
        Self {
            kind,
            statements: Vec::new(),
            incoming: Vec::new(),
            outgoing: Vec::new(),
            id,
        }
    }
}

impl<'a> Eq for BasicBlock<'a> {}
impl<'a> Display for BasicBlock<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            BasicBlockKind::Start => {
                write!(f, "<start>")
            }
            BasicBlockKind::End => {
                write!(f, "<end>")
            }
            BasicBlockKind::Base => {
                for stmt in &self.statements {
                    write!(f, "{}", stmt.type_to_string())?;
                }
                Ok(())
            }
        }
    }
}

#[derive(PartialEq, Hash, PartialOrd, Eq)]
pub enum BasicBlockKind {
    Base,
    Start,
    End,
}

#[derive(PartialEq, PartialOrd)]
pub struct BasicBlockEdge<'a> {
    from: Rc<RefCell<BasicBlock<'a>>>,
    to: Rc<RefCell<BasicBlock<'a>>>,
    condition: Option<(&'a MlirExpr, bool)>,
}

impl<'a> std::hash::Hash for BasicBlockEdge<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.from.borrow().hash(state);
        self.to.borrow().hash(state);
        if let Some(condition) = self.condition {
            condition.hash(state);
        }
    }
}

impl<'a> BasicBlockEdge<'a> {
    pub fn new(
        from: Rc<RefCell<BasicBlock<'a>>>,
        to: Rc<RefCell<BasicBlock<'a>>>,
        condition: Option<(&'a MlirExpr, bool)>,
    ) -> Self {
        Self {
            from,
            to,
            condition,
        }
    }
}

impl<'a> Display for BasicBlockEdge<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.condition.is_some() {
            write!(f, "<has condition>")
        } else {
            write!(f, "<no condition>")
        }
    }
}

pub struct BasicBlockFactory<'a> {
    mlir: &'a MlirBlock,
    statements: Vec<&'a MlirStmt>,
    blocks: Vec<Rc<RefCell<BasicBlock<'a>>>>,
}

impl<'a> BasicBlockFactory<'a> {
    pub fn new(mlir: &'a MlirBlock) -> Self {
        Self {
            mlir,
            statements: Vec::new(),
            blocks: Vec::new(),
        }
    }
    pub fn build(mut self) -> Vec<Rc<RefCell<BasicBlock<'a>>>> {
        for stmt in self.mlir.0.iter() {
            match &stmt {
                MlirStmt::Label(_ident) => {
                    self.transition_block();
                    self.statements.push(stmt);
                }
                MlirStmt::Expression(_) | MlirStmt::VariableDeclaration(_) => {
                    self.statements.push(stmt);
                }
                MlirStmt::Return(_)
                | MlirStmt::Goto(_)
                | MlirStmt::GotoFalse(_, _)
                | MlirStmt::GotoTrue(_, _) => {
                    self.statements.push(stmt);
                    self.transition_block();
                }
                _ => panic!("Unexpected statement: {}", stmt.type_to_string()),
            }
        }
        self.transition_block();
        self.blocks
    }

    fn transition_block(&mut self) {
        if !self.statements.is_empty() {
            let block = block!(BasicBlockKind::Base);
            block
                .borrow_mut()
                .statements
                .extend(std::mem::take(&mut self.statements));
            debug_assert!(self.statements.is_empty());
            self.blocks.push(block);
        }
    }
}

pub struct GraphFactory<'a> {
    block_from_statement: HashMap<&'a MlirStmt, Rc<RefCell<BasicBlock<'a>>>>,
    block_from_label: HashMap<InternedStr, Rc<RefCell<BasicBlock<'a>>>>,
    edges: Vec<Rc<BasicBlockEdge<'a>>>,
    start: Rc<RefCell<BasicBlock<'a>>>,
    end: Rc<RefCell<BasicBlock<'a>>>,
}

impl<'a> GraphFactory<'a> {
    pub fn new() -> Self {
        Self {
            block_from_statement: HashMap::new(),
            block_from_label: HashMap::new(),
            edges: Vec::new(),
            start: block!(BasicBlockKind::Start),
            end: block!(BasicBlockKind::End),
        }
    }

    pub fn build(mut self, blocks: Vec<Rc<RefCell<BasicBlock<'a>>>>) -> ControlFlowGraph<'a> {
        let mut blocks = blocks;
        if let Some(block) = blocks.first() {
            self.connect(self.start.clone(), block.clone(), None);
        } else {
            self.connect(self.start.clone(), self.end.clone(), None);
        }

        for block in blocks.iter() {
            for stmt in &block.borrow().statements {
                self.block_from_statement.insert(*stmt, block.clone());
                if let MlirStmt::Label(marker) = stmt {
                    self.block_from_label.insert(marker.clone(), block.clone());
                }
            }
        }

        for i in 0..blocks.len() {
            let current = blocks[i].clone();
            let next = if (i == blocks.len() - 1) {
                self.end.clone()
            } else {
                blocks[i + 1].clone()
            };
            for stmt in &current.borrow().statements {
                let is_last_in_block = current
                    .borrow()
                    .statements
                    .last()
                    .is_some_and(|last| **last == **stmt);
                match stmt {
                    MlirStmt::Goto(label) => {
                        let to_block = self
                            .block_from_label
                            .get(label)
                            .expect("Corresponding label not found.");
                        self.connect(current.clone(), to_block.clone(), None);
                    }
                    MlirStmt::GotoFalse(condition, label)
                    | MlirStmt::GotoTrue(condition, label) => {
                        let jump_if_true = matches!(stmt, MlirStmt::GotoTrue(_, _));
                        let to_block = self
                            .block_from_label
                            .get(label)
                            .expect("Corresponding label not found.");
                        let else_block = &next;
                        self.connect(
                            current.clone(),
                            to_block.clone(),
                            Some((condition, jump_if_true)),
                        );
                        self.connect(
                            current.clone(),
                            else_block.clone(),
                            Some((condition, !jump_if_true)),
                        );
                    }
                    MlirStmt::Return(_) => {
                        self.connect(current.clone(), self.end.clone(), None);
                    }
                    MlirStmt::Expression(_)
                    | MlirStmt::VariableDeclaration(_)
                    | MlirStmt::Label(_)
                        if is_last_in_block =>
                    {
                        self.connect(current.clone(), next.clone(), None);
                    }
                    _ => panic!("Unexpected statement: {}", stmt.type_to_string()),
                }
            }
        }

        let mut index = 0;
        while let Some(block) = blocks.get(index) {
            if block.borrow().incoming.is_empty() {
                let block = block.clone();
                self.remove_block(&mut blocks, block);
                index = 0;
            } else {
                index += 1;
            }
        }

        blocks.insert(0, self.start.clone());
        blocks.push(self.end.clone());

        ControlFlowGraph {
            start: std::mem::replace(&mut self.start, block!(BasicBlockKind::Start)),
            end: std::mem::replace(&mut self.end, block!(BasicBlockKind::End)),
            edges: std::mem::take(&mut self.edges),
            blocks,
        }
    }

    fn connect(
        &mut self,
        from: Rc<RefCell<BasicBlock<'a>>>,
        to: Rc<RefCell<BasicBlock<'a>>>,
        condition: Option<(&'a MlirExpr, bool)>,
    ) {
        // in the future id like to have a folded literal check here.
        let mut edge = Rc::new(BasicBlockEdge::new(from.clone(), to.clone(), condition));
        from.borrow_mut().outgoing.push(edge.clone());
        to.borrow_mut().incoming.push(edge.clone());
        self.edges.push(edge);
    }

    fn remove_block(
        &mut self,
        blocks: &mut Vec<Rc<RefCell<BasicBlock<'a>>>>,
        block: Rc<RefCell<BasicBlock<'a>>>,
    ) {
        macro_rules! remove_edge {
            ($remove_from:ident, $location:ident, $sub_location:ident) => {
                for edge in &block.borrow().$remove_from {
                    if let Some(index) = edge
                        .$location
                        .borrow()
                        .$sub_location
                        .iter()
                        .position(|iter_edge| **iter_edge == **edge)
                    {
                        edge.$location.borrow_mut().$sub_location.remove(index);
                    }
                }
            };
        }
        remove_edge!(incoming, from, outgoing);
        remove_edge!(outgoing, to, incoming);
        blocks.remove(
            blocks
                .iter()
                .position(|_block| **_block == *block)
                .expect("Block not properly inserted into main vec."),
        );
    }
}

pub struct ControlFlowGraph<'a> {
    start: Rc<RefCell<BasicBlock<'a>>>,
    end: Rc<RefCell<BasicBlock<'a>>>,
    blocks: Vec<Rc<RefCell<BasicBlock<'a>>>>,
    edges: Vec<Rc<BasicBlockEdge<'a>>>,
}

impl<'a> ControlFlowGraph<'a> {
    pub fn new(mlir: &'a MlirBlock) -> Self {
        let block_factory = BasicBlockFactory::new(mlir);
        let blocks = block_factory.build();
        let graph_factory = GraphFactory::new();
        graph_factory.build(blocks)
    }

    pub fn all_paths_return(&self) -> bool {
        for edge in &self.end.borrow().incoming {
            let borrow = edge.from.borrow();
            let last = borrow.incoming.last().and_then(|edge| {
                edge.from
                    .borrow()
                    .statements
                    .last()
                    .map(|stmt| !matches!(stmt, MlirStmt::Return(_)))
            });
            if last.is_none() || last.unwrap() {
                return false;
            }
        }
        true
    }
}

impl<'a> Display for ControlFlowGraph<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        macro_rules! quote {
            ($text:expr) => {
                format!(
                    "\"{}\"",
                    $text
                        .trim_end()
                        .replace("\\", "\\\\")
                        .replace("\"", "\\\"")
                        .replace("\n", "\\l")
                )
            };
        }
        write!(f, "digraph G {{")?;
        macro_rules! id {
            ($block:expr) => {
                format!("N{}", $block.borrow().id)
            };
        }

        for block in &self.blocks {
            let id = id!(block);
            let label = quote!(block.borrow().to_string());
            write!(f, "    {id}[label = {label}, shape = box]")?;
        }

        for edge in &self.edges {
            let from_id = edge.from.borrow().id;
            let to_id = edge.to.borrow().id;
            let label = quote!(edge.to_string());
            write!(f, "    {from_id} -> {to_id} [label = {label}]")?;
        }

        writeln!(f, "}}")
    }
}
