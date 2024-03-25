use crate::analysis::hlir::{HighLevelIR, HlirBlock, HlirExpr, HlirFunction, HlirStmt, HlirType};
use std::collections::HashMap;
use std::rc::Rc;

static mut BLOCK_COUNT: usize = 0;

struct BasicBlock<'a> {
    kind: BasicBlockKind,
    statements: Vec<&'a HlirStmt>,
    incoming: Vec<Rc<BasicBlock<'a>>>,
    outgoing: Vec<Rc<BasicBlock<'a>>>,
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

enum BasicBlockKind {
    Base,
    Start,
    End,
}

struct BasicBlockEdge<'a> {
    from: Rc<BasicBlock<'a>>,
    to: Rc<BasicBlock<'a>>,
    condition: Option<&'a HlirExpr>,
}

struct BasicBlockFactory<'a> {
    statements: Vec<&'a HlirStmt>,
    blocks: Vec<Rc<BasicBlock<'a>>>,
}

impl<'a> BasicBlockFactory<'a> {
    pub fn build(block: &HlirBlock) {
        for stmt in block.iter() {}
    }
}

pub struct ControlFlowGraph<'a> {
    function: &'a HlirFunction,
    start: Rc<BasicBlock<'a>>,
    end: Rc<BasicBlock<'a>>,
    blocks: Vec<Rc<BasicBlock<'a>>>,
    edges: Vec<Rc<BasicBlockEdge<'a>>>,
}

impl<'a> ControlFlowGraph<'a> {
    fn new(function: &'a HlirFunction) -> Self {
        Self {
            function,
            start: Rc::new(BasicBlock::new(BasicBlockKind::Start)),
            end: Rc::new(BasicBlock::new(BasicBlockKind::End)),
            blocks: Vec::new(),
            edges: Vec::new(),
        }
    }

    pub fn build(mut self) -> Self {
        for stmt in &self.function.body.0 {
            todo!()
        }
        todo!()
    }
}
