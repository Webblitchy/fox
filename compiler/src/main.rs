// TODO remove at the end
#![allow(non_snake_case)]
#![allow(unused_variables)]
#![allow(dead_code)]
#![allow(unused_imports)]

use std::env;

use crate::fox::RunMode;

mod _1_lexing;
use _1_lexing::*;
mod _2_parsing;
use _2_parsing::*;
mod _3_checking;
use _3_checking::*;
mod _4_codegen;
use _4_codegen::*;
mod symbols;
use symbols::*;

mod compileErr;
mod metadata;
mod utils;
mod vartype;

mod fox;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() == 3 {
        match args[1].as_str() {
            "build" => {
                fox::compileFile(&args[2], RunMode::Build);
                return;
            }
            "run" => {
                fox::compileFile(&args[2], RunMode::Run);
                return;
            }
            _ => {}
        }
    } else if args.len() == 2 {
        fox::compileFile(&args[1], RunMode::Run);
        return;
    }

    println!("{:?}", args);
    println!("Usage: fox [ run | build ] <srcCode>.fox");
}
