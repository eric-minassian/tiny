use crate::{
    ast::{visit::AstVisitor, Assignment, Block, ReturnStatement},
    lexer::Identifier,
};

pub struct PhiDetector {
    identifiers: Vec<Identifier>,
    temp_identifiers: Vec<Identifier>,
}

impl PhiDetector {
    pub fn detect(block: &Block) -> Vec<Identifier> {
        let mut detector = Self::new();
        detector.visit_block(block);

        detector.identifiers.sort();
        detector.identifiers.dedup();

        detector.identifiers
    }

    fn new() -> Self {
        Self {
            identifiers: Vec::new(),
            temp_identifiers: Vec::new(),
        }
    }
}

impl AstVisitor for PhiDetector {
    fn visit_block(&mut self, block: &Block) {
        for statement in &block.statements {
            self.visit_statement(statement);
        }

        self.identifiers.extend(self.temp_identifiers.drain(..));
    }

    fn visit_return_statement(&mut self, _return_statement: &ReturnStatement) {
        self.temp_identifiers.clear();
    }

    fn visit_assignment(&mut self, assignment: &Assignment) {
        self.temp_identifiers.push(assignment.ident.clone());
    }
}
