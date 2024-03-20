use crate::analysis::hlir::HlirBlock;
use crate::analysis::GlobalValidator;
use crate::parser::ast::Block;
use crate::util::Locatable;

impl GlobalValidator {
    fn validate_block(&mut self, block: &Locatable<Block>) -> Result<HlirBlock, ()> {
        let mut statements = Vec::new();
        for raw_stmt in &block.0 {
            statements.push(self.validate_statement(raw_stmt)?);
        }
        Ok(HlirBlock(statements))
    }
}
