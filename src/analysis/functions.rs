use crate::analysis::hlir::{HlirFunction, HlirStmt};
use crate::analysis::SharedReporter;
use crate::util::error::CompilerError;
use crate::util::Span;
use std::rc::Rc;

static mut NODE_COUNT: usize = 0;

struct GraphNode {
    id: usize,
    span: Span,
    incoming: Vec<Rc<GraphNode>>,
    outgoing: Vec<Rc<GraphNode>>,
    contents: Vec<String>,
}
impl GraphNode {
    fn new(span: Span) -> Self {
        let id = unsafe {
            NODE_COUNT += 1;
            NODE_COUNT
        };
        GraphNode {
            id,
            span,
            incoming: Vec::new(),
            outgoing: Vec::new(),
            contents: Vec::new(),
        }
    }
}

struct GraphEdge {
    from: Rc<GraphNode>,
    to: Rc<GraphNode>,
}

pub struct FunctionFlowAnalyzer<'a> {
    function: &'a HlirFunction,
    start: Rc<GraphNode>,
    end: Rc<GraphNode>,
}
impl<'a> FunctionFlowAnalyzer<'a> {
    pub fn new(reporter: SharedReporter, function: &'a HlirFunction) -> Self {
        Self {
            function,
            start: Rc::new(GraphNode::new(Span::default())),
            end: Rc::new(GraphNode::new(Span::default())),
        }
    }

    pub fn analyze(mut self) -> Result<(), ()> {
        todo!()
    }
}
