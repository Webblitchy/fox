#![allow(dead_code)]
#![allow(non_snake_case)]
use std::fs;
use std::process::{Command, Stdio};

use crate::{
    compileErr::CompileErr,
    irGen::IrGen,
    literal::Literal,
    metadata::{self, Metadata},
    parser::Parser,
    scanner::Scanner,
    token::TokenType,
    typeChecker::TypeChecker,
    typedAST::{TypedExpr, TypedStmt},
    untypedAST::{Expr, Stmt},
    utils::astPrinter,
};
use inkwell::context::Context;
use inkwell::targets::TargetMachine;

pub enum RunMode {
    Build,
    Run,
}

const IR_FILE: &str = "output.ll"; // LLVM IR

pub fn compileFile(filePath: &str, mode: RunMode) {
    let mut errors: Vec<CompileErr> = Vec::new();
    let filename = filePath.split("/").last().unwrap();
    let shortFilename = filename.replace(".fox", "").to_string();

    // VERIFY DEPENDENCIES
    if Command::new("clang")
        .arg("--version")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .is_err()
    {
        panic!("Clang not found. Please install clang first."); // TODO: url vers ma doc
    }

    //-------------------------------------------------------
    // READ CODE
    let fileContent = match fs::read_to_string(filePath) {
        Ok(fileContent) => fileContent,
        Err(err) => {
            match err.kind() {
                std::io::ErrorKind::InvalidData => println!("Bad file format: {}", filePath),
                std::io::ErrorKind::NotFound => println!("File not found: {}", filePath),
                _ => println!("File error: {}", err),
            }
            return;
        }
    };

    //-------------------------------------------------------
    // SCANNER
    let mut scanner = Scanner::new(fileContent.as_str());
    scanner.scanTokens();
    errors.extend(scanner.errors);
    println!("✓ Scanning complete");

    // Debug
    for t in scanner.tokens.clone() {
        // print!("{} ", t.lexeme);
        print!("{:?} ", t.tokenType);
    }
    println!("");

    //-------------------------------------------------------
    // PARSER
    let mut parser = Parser::new(scanner.tokens);
    let statements = parser.parse();
    errors.extend(parser.errors);
    println!("✓ Parsing complete");

    // For debug
    // println!("\nSTATEMENTS:");
    // for stmt in statements.clone() {
    //     println!("{:?}", stmt);
    // }

    //-------------------------------------------------------
    // TYPE CHECKER
    // Take statements, return typed statements (unsugarised)
    let mut typedStmts = Vec::new();
    let mut typeChecker = TypeChecker::new();
    typeChecker.insertBuiltinFn();
    for untypedStmt in &statements {
        typeChecker.declarationPass(untypedStmt);
    }
    for untypedStmt in &statements {
        typedStmts.push(typeChecker.checkType(untypedStmt));
    }
    typeChecker.afterChecks();
    errors.extend(typeChecker.errors);

    println!("\nTYPED STATEMENTS:");
    for typedStmt in typedStmts.clone() {
        println!("{:?}", typedStmt);
    }

    //-------------------------------------------------------
    // Error handling
    if errors.len() != 0 {
        println!("\n\x1b[31m##### COMPILE ERROR #####\x1b[0m\n");
        for error in errors {
            printCompileErr(&fileContent, &filename, error.clone());
            print!("{}\n\n", "_".repeat(50));
        }

        return;
    }

    //-------------------------------------------------------
    // Constant Folding
    // TODO

    //-------------------------------------------------------
    // IR Generator
    let irContext = Context::create();
    let mut irGen = IrGen::new(&irContext, &shortFilename, &typeChecker.fnTable);

    // Declare functions signatures before statements
    match irGen.declareFnSignatures() {
        Ok(()) => (),
        Err(error) => {
            println!("\n\x1b[31m##### COMPILE ERROR #####\x1b[0m\n");
            printCompileErr(&fileContent, &filename, error);
            return;
        }
    }

    // Generate IR LLVM
    for stmt in &typedStmts {
        match irGen.genStatement(stmt) {
            Ok(_) => (),
            Err(error) => {
                println!("\n\x1b[31m##### COMPILE ERROR #####\x1b[0m\n");
                printCompileErr(&fileContent, &filename, error);
                return;
            }
        }
    }

    //-------------------------------------------------------
    //  EXPORT TO .ll
    irGen.module.set_triple(&TargetMachine::get_default_triple());
    irGen.module.print_to_file(IR_FILE).expect("Failed to write LLVM IR file");

    println!("File successfully compiled !");

    match mode {
        RunMode::Build => genBinary(filename),
        RunMode::Run => runLlvm(),
    }
}

pub fn genBinary(name: &str) {
    // Adapt for OS
    let mut libFileName = "libcorelib".to_string();
    let mut binFileName = name.replace(".fox", "").to_string();
    if cfg!(target_os = "windows") {
        libFileName += ".lib";
        binFileName += ".exe";
    } else if cfg!(target_os = "macos") {
        libFileName += ".a";
    } else if cfg!(target_os = "linux") {
        libFileName += ".a";
    } else {
        panic!("Unsupported OS");
    };

    let mut args = vec![IR_FILE, &libFileName, "-o", &binFileName];

    // Optionnel mais propre ?
    if cfg!(target_os = "linux") {
        args.push("-fuse-ld=lld");
    }

    let status = Command::new("clang").args(&args).status().expect("Failed to run clang");
    assert!(status.success());

    println!("Binary successfully generated !");
}

pub fn runLlvm() {
    println!("\n### EXECUTING CODE###\n");

    let status = Command::new("lli").args(&[IR_FILE]).status().expect("Failed to run lli");
    assert!(status.success());
}

// Print only if exists
fn printCodeLine(fileContent: &str, lineNb: usize, lineWidth: usize) {
    match fileContent.lines().nth(lineNb - 1) {
        Some(line) => println!("{:<width$} | {}", lineNb, line, width = lineWidth),
        None => {}
    }
}

pub fn printCompileErr(fileContent: &str, filename: &str, error: CompileErr) {
    let metadata = error.metadata;

    if metadata.end == 0 {
        // No metadata

        println!("{} => \x1b[31m{}\x1b[0m", filename, error.errType);
        if error.msg != "" {
            println!("{}", error.msg);
        }
        return;
    } else {
        println!(
            "{}: [{}:{}] => \x1b[31m{}\x1b[0m",
            filename, metadata.line, metadata.column, error.errType
        );

        let lineNbWidth = format!("{}", metadata.line + 1).len(); // +1, because width is based on max width (line-1, line, line+1)

        // print previous line
        if metadata.line != 1 {
            printCodeLine(fileContent, metadata.line - 1, lineNbWidth);
            println!("{} |", " ".repeat(lineNbWidth)); // print empty line
        }

        printCodeLine(fileContent, metadata.line, lineNbWidth);
        print!("{} |", " ".repeat(lineNbWidth)); // print empty line
        print!("{}", " ".repeat(metadata.column));
        let underlineLen = metadata.end - metadata.start;
        if underlineLen > 1 {
            print!("\x1b[31m{}\x1b[0m", "~".repeat(underlineLen));
        } else {
            print!("\x1b[31m^\x1b[0m");
        }
        println!("  \x1b[31m{}\x1b[0m", error.msg);

        //print next line
        if metadata.line + 1 < fileContent.lines().count() {
            printCodeLine(fileContent, metadata.line + 1, lineNbWidth);
        }
    }

    println!();
}
