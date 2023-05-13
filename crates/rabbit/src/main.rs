#![allow(dead_code)]

use clap::{Parser, Subcommand};
use std::io::Read;
use std::path::PathBuf;
use std::{fmt, fs};
use rabbit::input::Input;
use rabbit::llvm::LlvmCodegen;

use rabbit::tokenizer::{Token, TokenKind, Tokenizer};

/// Rabbit — the Rust Wabbit compiler
#[derive(clap::Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Tokenize the input and display the tokens
    #[clap(aliases = &["tok"])]
    Tokenize {
        /// Path to wabbit source file (or stdin if not present)
        path: Option<PathBuf>,

        /// Wabbit source code
        #[arg(short)]
        code: Option<String>,
    },

    /// Parse the input and display the AST
    Parse {
        /// Path to wabbit source file (or stdin if not present)
        path: Option<PathBuf>,

        /// Wabbit source code
        #[arg(short)]
        code: Option<String>,
    },

    /// Run the interpreter on the code
    #[clap(aliases = &["int", "run"])]
    Interp {
        /// Path to wabbit source file (or stdin if not present)
        path: Option<PathBuf>,

        /// Wabbit source code
        #[arg(short)]
        code: Option<String>,
    },

    /// Run the formatter on the code
    #[clap(aliases = &["fmt"])]
    Format {
        /// Path to wabbit source file
        path: Option<PathBuf>,

        /// Wabbit source code
        #[arg(short)]
        code: Option<String>,
    },

    /// Run the code generator on the code
    Llvm {
        /// Path to wabbit source file
        path: Option<PathBuf>,

        /// Wabbit source code
        #[arg(short)]
        code: Option<String>,
    },
}

fn get_source(path: Option<PathBuf>, code: Option<String>) -> anyhow::Result<String> {
    if let Some(code) = code {
        Ok(code)
    } else if let Some(path) = path {
        Ok(fs::read_to_string(path)?)
    } else {
        let mut buffer = String::new();
        std::io::stdin().read_to_string(&mut buffer)?;
        Ok(buffer)
    }
}

/// Newtype wrapper over a [`Token`] for the purpose of pretty printing the token stream.
struct DisplayToken(Token);

impl fmt::Display for DisplayToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let span = format!("{}", self.0.span);
        let kind = match self.0.kind {
            TokenKind::Name(ref s) => format!("{:10} {:?}", "Name", s),
            TokenKind::Int(i) => format!("{:10} {}", "Int", i),
            TokenKind::Float(fl) => format!("{:10} {:?}", "Float", fl),
            TokenKind::Bool(b) => format!("{:10} {}", "Bool", b),
            _ => format!("{:?}", self.0.kind),
        };

        write!(f, "{:15} {}", span, kind)
    }
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    match cli.command {
        Commands::Tokenize { path, code } => {
            let source = get_source(path, code)?;
            let tokens = Tokenizer::tokenize(&Input::new(&source))?;
            for token in tokens {
                println!("{}", DisplayToken(token));
            }
        }

        Commands::Parse { path, code } => {
            let source = get_source(path, code)?;
            let input = Input::new(&source);
            let block = rabbit::parser::Parser::parse(&input)?;
            println!("{:#?}", block);
        }

        Commands::Interp { path, code } => {
            let source = get_source(path, code)?;
            let input = Input::new(&source);
            rabbit::interp::Interp::interp(&input)?;
        }

        Commands::Format { path, code } => {
            let source = get_source(path, code)?;
            let input = Input::new(&source);
            let block = rabbit::parser::Parser::parse(&input)?;
            println!("{block}");
        }

        Commands::Llvm { path, code } => {
            let source = get_source(path, code)?;
            let input = Input::new(&source);
            let block = rabbit::parser::Parser::parse(&input)?;
            let mut llvm = LlvmCodegen::new(&input);
            llvm.run(&block)?;
            let out = llvm.generate()?;
            println!("{}", out);
        }
    }

    Ok(())
}
