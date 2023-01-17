use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use crate::counter::Counted;
use crate::either::Either;
use crate::nimbus_lexer::Location;


#[derive(Debug)]
pub struct Ast {
    pub tree: Vec<Counted<AstDecl>>,
    pub locations: HashMap<usize, Location>
}

impl Ast {
    pub fn new() -> Self {
        Self { tree: Vec::new(), locations: HashMap::new() }
    }
}

#[derive(Debug)]
pub enum AstDecl {
    FnDecl {
        name: String,
        generic_type_args: Vec<String>,
        args: Vec<(String, TypeLiteral)>,
        block: AstExpr
    }
}

#[derive(Debug)]
pub enum AstStatement {
    Block(Block),
    ExprStatement(AstExpr),
    For(String, AstExpr, Block),
    While(AstExpr, Block),
}

#[derive(Debug)]
pub struct Block {
    statements: Vec<Either<AstStatement, AstDecl>>,
    final_expr: Option<AstExpr>
}

#[derive(Debug)]
pub struct If {
    condition: AstExpr,
    block: Block
}

#[derive(Debug)]
pub enum BopType {
    Plus,
    Minus,
    Leq
}

#[derive(Debug)]
pub enum AstExpr {
    If,
    VarGet(String),
    Bop(Box<AstExpr>, BopType, Box<AstExpr>),
    FnCall{callee: Box<AstExpr>, args: Vec<AstExpr>},
    Atom(i32)
}

#[derive(Debug)]
pub enum TypeLiteral {
    SimpleType(String),
    ArrayType(Box<TypeLiteral>),
    GenericType(String, Vec<TypeLiteral>),
    OptionType(Box<TypeLiteral>)
}