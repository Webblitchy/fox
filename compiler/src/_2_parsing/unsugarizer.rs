/*
use crate::{token::{Token, TokenType}, untypedAST::Stmt};

pub fn unsugarize(stmt: Stmt) -> Stmt {
    let mut unsugarized = Vec::new();

    let mut curr = -1;
    while tokens.get(curr).is_some() {
        curr += 1;

        let token = tokens[curr].tokenType;

        if token == TokenType::TwoPlus {
            match tokens.get(curr + 1) {
                Some(token) => {
                    if token.type == TokenType::Identifier {
                        unsugarized.push(
                    }
                }
            }
        } else {
            unsugarized.push(token);
        }

    }

    return 1;
}
*/
