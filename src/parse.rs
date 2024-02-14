use std::mem;

use crate::{
    ast::{
        BinaryNode, EnclosedNode, FunctionKind, FunctionNode, Node, NumberNode, SyntaxTree,
        UnaryNode,
    },
    lexer::Lexer,
    token::{Token, TokenKind},
};

pub struct Parser {
    errors: Vec<String>,
    tokens: Vec<Token>,
    position: usize,
}

/// made multiple helper functions #[inline] for slight performance increase
impl Parser {
    pub fn parse(tokens: Vec<Token>) -> Result<SyntaxTree, Vec<String>> {
        let mut parser = Self::new(tokens);
        let root = parser.parse_top_level();
        let eof = parser.get_match(TokenKind::EndOfFile);
        if parser.errors.is_empty() {
            Ok(SyntaxTree::new(root, eof))
        } else {
            Err(mem::take(&mut parser.errors))
        }
    }

    #[inline]
    fn new(tokens: Vec<Token>) -> Self {
        let mut diagnostics = Vec::new();
        let tokens = tokens.into_iter().filter(Token::is_usable_token).collect();

        Parser {
            errors: diagnostics,
            tokens,
            position: 0,
        }
    }

    #[inline]
    fn peek(&self, offset: i32) -> Token {
        self.tokens
            .get(self.position + offset as usize)
            .unwrap_or(self.tokens.last().unwrap())
            .clone()
    }

    #[inline]
    fn current(&self) -> Token {
        self.peek(0)
    }

    #[inline]
    fn next_token(&mut self) -> Token {
        let current = self.current();
        self.position += 1;
        current
    }

    #[inline]
    fn get_match(&mut self, kind: TokenKind) -> Token {
        let current = self.current();
        if current.kind == kind {
            self.next_token()
        } else {
            let error_str = format!(
                "Error at input index {}. Expected {:#?} but found {:#?} '{:#?}'",
                current.index, kind, current.kind, current.value
            );
            self.errors.push(error_str);
            Token::new(kind, self.position, "".to_owned())
        }
    }

    #[inline]
    fn parse_top_level(&mut self) -> Node {
        self.parse_binary_expression(0)
    }

    fn parse_binary_expression(&mut self, lp: u8) -> Node {
        let mut left = self.parse_unary_expression();

        loop {
            let precedence = self.current().get_binary_precedence();
            if precedence == 0 || precedence <= lp {
                break;
            }
            let operator = self.next_token();
            let right = self.parse_binary_expression(precedence);

            left = Node::Binary(BinaryNode::new(operator, left, right));
        }

        left
    }

    fn parse_unary_expression(&mut self) -> Node {
        let mut node;
        if 0 < self.current().get_unary_precedence() {
            let token = self.next_token();
            let expression = self.parse_unary_expression();
            node = Node::Unary(UnaryNode::new(token, expression));
        } else {
            node = self.parse_function_call();
        }
        node
    }

    fn parse_function_call(&mut self) -> Node {
        let mut node;

        if self.current().kind == TokenKind::FunctionCall {
            let token = self.next_token();
            let args = self.get_function_args();
            let kind = FunctionKind::from(&token.value);
            node = Node::Function(FunctionNode::new(kind, token, args));
        } else {
            node = self.parse_primary_expression();
        }

        node
    }

    fn get_function_args(&mut self) -> Vec<Node> {
        self.get_match(TokenKind::OpenParenthesis);
        let mut nodes = vec![];
        while (self.current().kind != TokenKind::CloseParenthesis) {
            nodes.push(self.parse_top_level());
        }
        self.get_match(TokenKind::CloseParenthesis);
        nodes
    }

    fn parse_primary_expression(&mut self) -> Node {
        static ENCLOSING_TOKENS: [(TokenKind, TokenKind); 2] = [
            (TokenKind::OpenParenthesis, TokenKind::CloseParenthesis),
            (TokenKind::Pipe, TokenKind::Pipe),
        ];

        for (opening_type, closing_type) in ENCLOSING_TOKENS {
            if self.current().kind == opening_type {
                let left = self.next_token();
                let expression = self.parse_binary_expression(0);
                let right = self.get_match(closing_type);

                return Node::Enclosed(EnclosedNode::new(left, expression, right));
            }
        }

        self.parse_number_node()
    }

    fn parse_number_node(&mut self) -> Node {
        let token = self.get_match(TokenKind::NumberToken);

        let mut node_number = token.value.parse::<f32>().unwrap_or_else(|_| {
            self.errors.push(format!(
                "Invalid number found {} at input index {}.",
                token.value, token.index
            ));
            0.0
        });

        Node::Number(NumberNode::new(token, node_number))
    }
}
