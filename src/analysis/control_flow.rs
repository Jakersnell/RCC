use crate::analysis::mlir::{
    MidLevelIR, MlirBlock, MlirExpr, MlirFunction, MlirStmt, MlirType, MlirTypeDecl, MlirTypeKind,
    VOID_TYPE,
};
use crate::util::str_intern::InternedStr;
use derive_new::new;
use std::cell::RefCell;
use std::collections::HashMap;
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
struct BasicBlock<'a> {
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

#[derive(PartialEq, Hash, PartialOrd)]
enum BasicBlockKind {
    Base,
    Start,
    End,
}

#[derive(PartialEq, PartialOrd)]
struct BasicBlockEdge<'a> {
    from: Rc<RefCell<BasicBlock<'a>>>,
    to: Rc<RefCell<BasicBlock<'a>>>,
    condition: Option<&'a MlirExpr>,
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
    fn new(
        from: Rc<RefCell<BasicBlock<'a>>>,
        to: Rc<RefCell<BasicBlock<'a>>>,
        condition: Option<&'a MlirExpr>,
    ) -> Self {
        Self {
            from,
            to,
            condition,
        }
    }
}

struct BasicBlockFactory<'a> {
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
                MlirStmt::Return(_) | MlirStmt::Goto(_) | MlirStmt::ConditionalGoto(_, _) => {
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
    fn new(start: Rc<RefCell<BasicBlock<'a>>>, end: Rc<RefCell<BasicBlock<'a>>>) -> Self {
        Self {
            block_from_statement: HashMap::new(),
            block_from_label: HashMap::new(),
            edges: Vec::new(),
            start,
            end,
        }
    }

    fn build(&mut self, blocks: &[Rc<RefCell<BasicBlock<'a>>>]) -> ControlFlowGraph<'a> {
        if let Some(block) = blocks.iter().next() {
            self.connect(self.start.clone(), block.clone(), None);
        } else {
            self.connect(self.start.clone(), self.end.clone(), None);
        }

        for block in blocks {
            for stmt in &block.borrow().statements {
                self.block_from_statement.insert(*stmt, block.clone());
            }
        }

        todo!()
    }

    fn connect(
        &mut self,
        from: Rc<RefCell<BasicBlock<'a>>>,
        to: Rc<RefCell<BasicBlock<'a>>>,
        condition: Option<&'a MlirExpr>,
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
    function: &'a MlirFunction,
    start: Rc<RefCell<BasicBlock<'a>>>,
    end: Rc<RefCell<BasicBlock<'a>>>,
    blocks: Vec<Rc<RefCell<BasicBlock<'a>>>>,
    edges: Vec<Rc<BasicBlockEdge<'a>>>,
    ty_is_void: bool,
}

impl<'a> ControlFlowGraph<'a> {
    fn new(function: &'a MlirFunction) -> Self {
        let ty_is_void = (function.ty.value == VOID_TYPE);
        Self {
            function,
            start: block!(BasicBlockKind::Start),
            end: block!(BasicBlockKind::End),
            blocks: Vec::new(),
            edges: Vec::new(),
            ty_is_void,
        }
    }
}
