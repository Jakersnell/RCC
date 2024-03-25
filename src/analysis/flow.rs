use crate::analysis::hlir::{HighLevelIR, HlirExpr, HlirStmt, HlirType};
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

pub struct ControlFlowGraph<'a> {
    hlir: &'a HighLevelIR,
    start: Rc<BasicBlock<'a>>,
    end: Rc<BasicBlock<'a>>,
    blocks: Vec<Rc<BasicBlock<'a>>>,
    edges: Vec<Rc<BasicBlockEdge<'a>>>,
}
