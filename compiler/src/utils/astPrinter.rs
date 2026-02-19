#![allow(non_snake_case)]
#![allow(dead_code)]

use crate::untypedAST::Expr;

fn explore(exp: Expr, currStr: &mut String) {
    match exp {
        Expr::BinaryExpr {
            left,
            operator,
            right,
            metadata,
        } => {
            explore(*left, currStr);
            *currStr = format!("{} {}", currStr, operator.lexeme);
            explore(*right, currStr);
        }
        Expr::LogicalExpr {
            left,
            operator,
            right,
            metadata,
        } => {
            explore(*left, currStr);
            *currStr = format!("{} {}", currStr, operator.lexeme);
            explore(*right, currStr);
        }
        Expr::UnaryExpr { operator, right, metadata } => {
            *currStr = format!("{} {}", currStr, operator.lexeme);
            explore(*right, currStr);
        }
        Expr::Literal { value, metadata } => {
            *currStr = format!("{} {:?}", currStr, value);
        }
        Expr::Grouping { expr: internalExp, metadata } => {
            *currStr = format!("{} (", currStr);
            explore(*internalExp, currStr);
            *currStr = format!("{})", currStr);
        }
        Expr::FnCall { name, arguments, metadata } => {
            unimplemented!();
        }
        Expr::CompileErr => {
            *currStr = String::from("ERROR-NODE");
        }
    };
}

// To test
pub fn main(expr: Expr) {
    let mut str = String::new();
    explore(expr, &mut str);

    println!("{}", str);
}
