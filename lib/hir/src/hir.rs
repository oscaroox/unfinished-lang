use span_util::Span;
use type_core::Type;

pub type Program = Vec<Expression>;

#[derive(Debug, Clone)]
pub struct Ident {
    pub name: String,
    pub ty: Option<Type>,
    pub span: Span,
}


#[derive(Debug, Clone)]
pub struct Let {
    pub ident: Ident,
    pub init: Option<Box<Expression>>,
    pub span: Span,
    pub let_span: Span,
}

#[derive(Debug, Clone)]
pub struct LetRef {
    pub ident: Ident,
    pub scope_distance: usize,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Literal {
    pub kind: LiteralKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum LiteralKind {
    Int(u64),
    Float(f64),
    Bool(bool),
    String(String),
    Array(Vec<Expression>)
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub lhs: Ident,
    pub scope_distance: usize,
    pub rhs: Box<Expression>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum BinOpKind {
    Add(Span),
    // used for string interpolation
    Concat,
    Mul(Span),
    Sub(Span),
    Div(Span),
}

#[derive(Debug, Clone)]
pub struct BinOp {
    pub lhs: Box<Expression>,
    pub op: BinOpKind,
    pub rhs: Box<Expression>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum UnaryOpKind {
    Negate(Span),
    Not(Span),
}

#[derive(Debug, Clone)]
pub struct UnaryOp {
    pub op: UnaryOpKind,
    pub rhs: Box<Expression>,
    pub span: Span
}

#[derive(Debug, Clone)]
pub struct Grouping {
    pub expr: Box<Expression>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum LogicOpKind {
    Or(Span),
    And(Span),
    Eq(Span),
    Ne(Span),
    Lt(Span),
    Lte(Span),
    Gt(Span),
    Gte(Span),
}

#[derive(Debug, Clone)]
pub struct LogicOp {
    pub lhs: Box<Expression>,
    pub op: LogicOpKind,
    pub rhs: Box<Expression>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub ident: String,
    pub ty: Type,
    pub span: Span, 
}


#[derive(Debug, Clone)]
pub enum ImpSelf {
    Implicit,
    None,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Option<String>,
    pub params: Vec<Param>,
    pub return_type: Type,
    pub body: Box<Expression>,
    pub imp_self: ImpSelf, 
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Arg(Expression);

#[derive(Debug, Clone)]
pub struct Call {
    pub callee: Box<Expression>,
    pub arguments: Vec<Arg>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub exprs: Vec<Expression>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct If {
    pub condition: Box<Expression>,
    pub then: Box<Expression>,
    pub not_then: Option<Box<Expression>>,
    pub span: Span,
    pub kw_span: Span,
}

#[derive(Debug, Clone)]
pub struct Return {
    pub value: Option<Box<Expression>>,
    pub kw_span: Span,
    pub span: Span,
    pub imp_return: bool,
}

#[derive(Debug, Clone)]
pub struct SelfExpr {
    pub target: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Continue {
    pub span: Span
}

#[derive(Debug, Clone)]
pub struct Break {
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct GetIndex {
    pub lhs: Box<Expression>,
    pub idx: Box<Expression>,
    pub span: Span,
}


#[derive(Debug, Clone)]
pub struct SetIndex {
    pub lhs: Box<Expression>,
    pub idx: Box<Expression>,
    pub value: Box<Expression>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct GetProperty {
    pub object: Box<Expression>,
    pub ident: Ident,
    pub is_callable: bool,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct SetProperty {
    pub lhs: Box<Expression>,
    pub ident: Ident,
    pub value: Box<Expression>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct DataStructField {
    pub name: String,
    pub ty: Type,
    pub span: Span,
}


#[derive(Debug, Clone)]
pub struct DataStruct {
    pub ident: Ident,
    pub fields: Vec<DataStructField>,
    pub methods: Vec<Function>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct While {
    pub condition: Box<Expression>,
    pub body: Program,
    pub kw_span: Span,
}


#[derive(Debug, Clone)]
pub enum Expression {
    Let(Let),
    LetRef(LetRef),
    GetIndex(GetIndex),
    SetIndex(SetIndex),
    GetProperty(GetProperty),
    SetProperty(SetProperty),
    Literal(Literal),
    Assign(Assign),
    BinOp(BinOp),
    UnaryOp(UnaryOp),
    Grouping(Grouping),
    LogicOp(LogicOp),
    Function(Function),
    Call(Call),
    Block(Block),
    If(If),
    Return(Return),
    SelfExpr(SelfExpr),
    Continue(Continue),
    Break(Break),
    DataStruct(DataStruct),
    While(While),
}
