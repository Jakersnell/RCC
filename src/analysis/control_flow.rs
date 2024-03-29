use crate::analysis::mlir::{
    MidLevelIR, MlirBlock, MlirExpr, MlirFunction, MlirStmt, MlirType, MlirTypeDecl, MlirTypeKind,
    VOID_TYPE,
};
use crate::util::str_intern::InternedStr;
use crate::OUTPUT_GRAPH;
use derive_new::new;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{write, Debug, Display, Formatter};
use std::fs::File;
use std::hash::Hasher;
use std::io::{BufWriter, Write};
use std::ops::{DerefMut, Index};
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
    id: usize,
    kind: BasicBlockKind,
    statements: Vec<&'a MlirStmt>,
    incoming: Vec<Rc<BasicBlockEdge<'a>>>,
    outgoing: Vec<Rc<BasicBlockEdge<'a>>>,
}

impl<'a> Debug for BasicBlock<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "BasicBlock {{")?;
        writeln!(f, "    id: {},", self.id)?;
        writeln!(f, "    kind: {:#?}", self.kind)?;
        writeln!(f, "    statements: [")?;
        for stmt in &self.statements {
            writeln!(f, "        {},", stmt.type_to_string())?;
        }
        writeln!(f, "    ],")?;
        macro_rules! write_edges {
            ($location:ident) => {
                writeln!(f, "    {}: [", stringify!($location));
                for edge in &self.$location {
                    writeln!(
                        f,
                        "        edge {{ from: {}, to: {}, condition: {} }},",
                        edge.from.borrow().id,
                        edge.to.borrow().id,
                        if edge.condition.is_some() {
                            "Some"
                        } else {
                            "None"
                        }
                    )?;
                }
                writeln!(f, "    ],")?;
            };
        }
        write_edges!(incoming);
        write_edges!(outgoing);
        Ok(())
    }
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
                    writeln!(f, "{:#?}", stmt.type_to_string())?;
                }
                Ok(())
            }
        }
    }
}

#[derive(PartialEq, Hash, PartialOrd, Eq, Debug)]
pub enum BasicBlockKind {
    Base,
    Start,
    End,
}

#[derive(PartialEq, PartialOrd, Debug)]
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
        let start = Rc::new(RefCell::new(BasicBlock::new(BasicBlockKind::Start)));
        let end = Rc::new(RefCell::new(BasicBlock::new(BasicBlockKind::End)));
        Self {
            block_from_statement: HashMap::new(),
            block_from_label: HashMap::new(),
            edges: Vec::new(),
            start,
            end,
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
            for stmt in unsafe { &current.try_borrow_unguarded().unwrap().statements } {
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
                    | MlirStmt::Label(_) => {
                        if is_last_in_block {
                            self.connect(current.clone(), next.clone(), None);
                        }
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
            start: self.start,
            end: self.end,
            edges: self.edges,
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
        for edge in unsafe { &block.try_borrow_unguarded().unwrap().incoming } {
            edge.from
                .borrow_mut()
                .outgoing
                .retain(|_edge| **_edge != **edge);
            self.edges.retain(|_edge| **_edge != **edge);
        }
        for edge in unsafe { &block.try_borrow_unguarded().unwrap().outgoing } {
            edge.to
                .borrow_mut()
                .incoming
                .retain(|_edge| **_edge != **edge);
            self.edges.retain(|_edge| **_edge != **edge);
        }
        blocks.retain(|_block| **_block != *block);
    }
}

#[derive(Debug)]
pub struct ControlFlowGraph<'a> {
    start: Rc<RefCell<BasicBlock<'a>>>,
    end: Rc<RefCell<BasicBlock<'a>>>,
    blocks: Vec<Rc<RefCell<BasicBlock<'a>>>>,
    edges: Vec<Rc<BasicBlockEdge<'a>>>,
}
static mut GRAPH_COUNT: usize = 0;
impl<'a> ControlFlowGraph<'a> {
    pub fn new(mlir: &'a MlirBlock) -> Self {
        let block_factory = BasicBlockFactory::new(mlir);
        let blocks = block_factory.build();
        let graph_factory = GraphFactory::new();
        let cfg = graph_factory.build(blocks);
        if OUTPUT_GRAPH {
            let cfg_to_string = cfg.to_string();
            let mut file = File::create(format!("graph_{}.dot", unsafe {
                GRAPH_COUNT += 1;
                GRAPH_COUNT
            }))
            .unwrap();
            let mut writer = BufWriter::new(file);
            writer.write_all(cfg_to_string.as_bytes());
        }
        cfg
    }

    pub fn all_paths_return(&self) -> bool {
        for edge in &self.end.borrow().incoming {
            let borrow = edge.from.borrow();
            let last = borrow.statements.last();
            if !last.is_some_and(|last| matches!(last, MlirStmt::Return(_))) {
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
        writeln!(f, "digraph G {{")?;
        macro_rules! id {
            ($block:expr) => {
                format!("N{}", $block.borrow().id)
            };
        }

        for block in &self.blocks {
            let id = id!(block);
            let label = quote!(block.borrow().to_string());
            writeln!(f, "    {id}[label = {label}, shape = box]")?;
        }

        for edge in &self.edges {
            let from_id = edge.from.borrow().id;
            let to_id = edge.to.borrow().id;
            let label = quote!(edge.to_string());
            writeln!(f, "    N{from_id} -> N{to_id} [label = {label}]")?;
        }

        writeln!(f, "}}")
    }
}
