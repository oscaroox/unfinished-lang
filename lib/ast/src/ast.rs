use span_util::Span;
use type_core::Type;

pub type Program = Vec<Expression>;

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier {
    pub value: String,
    // pub token_type: Option<TokenType>,
    pub value_type: Option<Type>,
    pub span: Span,
}

impl Identifier {
    pub fn new(value: String, span: Span) -> Identifier {
        Identifier {
            value,
            // token_type: None,
            value_type: None,
            span,
        }
    }

    pub fn with_all(
        value: String,
        // token_type: TokenType,
        value_type: Type,
        span: Span,
    ) -> Identifier {
        Identifier {
            value,
            value_type: Some(value_type),
            // token_type: Some(token_type),
            span,
        }
    }

    pub fn with_token_type(
        value: String,
        // token_type: TokenType,
        span: Span,
    ) -> Identifier {
        Identifier {
            value,
            value_type: None,
            // token_type: Some(token_type),
            span,
        }
    }

    pub fn with_value_type(value: String, value_type: Option<Type>, span: Span) -> Identifier {
        Identifier {
            value,
            value_type,
            // token_type: None,
            span,
        }
    }

    pub fn is_self(&self) -> bool {
        self.value == "self"
        // match &self.token_type {
        //     Some(tt) => TokenType::SELF == *tt,
        //     None => false,
        // }
    }
}

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Assign {
    pub name: Identifier,
    pub scope_distance: Option<usize>,
    pub rhs: Box<Expression>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOperation {
    Add(Span),
    // TODO while this is convenient, this should be its own expression
    ConcatInterpolation,
    Subtract(Span),
    Multiply(Span),
    Divide(Span),
}

impl std::fmt::Display for BinaryOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOperation::Add(_) => write!(f, "+"),
            BinaryOperation::ConcatInterpolation => write!(f, ""),
            BinaryOperation::Subtract(_) => write!(f, "-"),
            BinaryOperation::Multiply(_) => write!(f, "*"),
            BinaryOperation::Divide(_) => write!(f, "/"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BinOp {
    pub left: Box<Expression>,
    pub op: BinaryOperation,
    pub right: Box<Expression>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOperation {
    Minus(Span),
    Plus(Span),
    Not(Span),
}

impl std::fmt::Display for UnaryOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOperation::Minus(_) => write!(f, "-"),
            UnaryOperation::Plus(_) => write!(f, "+"),
            UnaryOperation::Not(_) => write!(f, "!"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnaryOp {
    pub op: UnaryOperation,
    pub rhs: Box<Expression>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    pub exprs: Vec<Expression>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BreakExpr {
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Call {
    pub callee: Box<Expression>,
    pub arguments: Vec<CallArgs>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallArgs(pub Option<Identifier>, pub Expression);

impl CallArgs {
    pub fn is_named(&self) -> bool {
        match self.0 {
            Some(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ContinueExpr {
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct DataStructInstanceField {
    pub name: Identifier,
    pub value: Expression,
}

impl DataStructInstanceField {
    pub fn new(name: Identifier, value: Expression) -> DataStructInstanceField {
        DataStructInstanceField { name, value }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct DataStructInstance {
    pub name: Identifier,
    pub fields: Vec<DataStructInstanceField>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct DataStruct {
    pub name: Identifier,
    // TODO add spans to fields
    pub fields: Vec<Identifier>,
    pub methods: Vec<Expression>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub name: Option<String>,
    pub params: Vec<Identifier>,
    pub return_type: Type,
    pub body: Box<Expression>,
    pub is_static: bool,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GetIndex {
    pub lhs: Box<Expression>,
    pub index: Box<Expression>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GetProperty {
    pub object: Box<Expression>,
    pub name: Identifier,
    pub is_callable: bool,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Grouping {
    pub expr: Box<Expression>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfConditional {
    pub condition: Box<Expression>,
    pub then: Box<Expression>,
    pub not_then: Option<Box<Expression>>,
    pub span: Span,
    pub if_token: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ImplicitReturn {
    pub value: Box<Expression>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnExpr {
    pub value: Box<Option<Expression>>,
    pub return_token: Span,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LetExpr {
    pub name: Identifier,
    pub value: Option<Box<Expression>>,
    pub span: Span,
    pub let_token: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LetRef {
    pub name: Identifier,
    pub scope_distance: Option<usize>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum LiteralValue {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Array(Vec<Expression>),
    Null,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Literal {
    pub value: LiteralValue,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum LogicOperation {
    Or(Span),
    And(Span),
    Equal(Span),
    NotEqual(Span),
    LessThan(Span),
    LessThanEqual(Span),
    GreaterThan(Span),
    GreaterThanEqual(Span),
}

impl std::fmt::Display for LogicOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LogicOperation::Or(_) => write!(f, "||"),
            LogicOperation::And(_) => write!(f, "&&"),
            LogicOperation::Equal(_) => write!(f, "=="),
            LogicOperation::NotEqual(_) => write!(f, "!="),
            LogicOperation::LessThan(_) => write!(f, "<"),
            LogicOperation::LessThanEqual(_) => write!(f, "<="),
            LogicOperation::GreaterThan(_) => write!(f, ">"),
            LogicOperation::GreaterThanEqual(_) => write!(f, ">="),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Logic {
    pub lhs: Box<Expression>,
    pub op: LogicOperation,
    pub rhs: Box<Expression>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LoopExpr {
    pub body: Box<Expression>,
    pub loop_token: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct WhileExpr {
    pub condition: Box<Expression>,
    pub body: Box<Expression>,
    pub span: Span,
    pub while_token: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ForExpr {
    pub identifier: Identifier,
    pub iterator: Box<Expression>,
    pub body: Box<Expression>,
    pub span: Span,
    pub for_token: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SelfExpr {
    pub name: String,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SetIndex {
    pub lhs: Box<Expression>,
    pub index: Box<Expression>,
    pub value: Box<Expression>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SetProperty {
    pub object: Box<Expression>,
    pub name: Identifier,
    pub value: Box<Expression>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    BinOp(BinOp),
    Literal(Literal),
    Assign(Assign),
    GetIndex(GetIndex),
    SetIndex(SetIndex),
    GetProperty(GetProperty),
    SetProperty(SetProperty),
    Let(LetExpr),
    LetRef(LetRef),
    UnaryOp(UnaryOp),
    Grouping(Grouping),
    Logic(Logic),
    Call(Call),
    Function(Function),
    DataStruct(DataStruct),
    DataStructInstance(DataStructInstance),
    Block(Block),
    If(IfConditional),
    ImplicitReturn(ImplicitReturn),
    Return(ReturnExpr),
    SelfExpr(SelfExpr),
    LoopExpr(LoopExpr),
    WhileExpr(WhileExpr),
    ForExpr(ForExpr),
    BreakExpr(BreakExpr),
    ContinueExpr(ContinueExpr),
}

impl Expression {
    pub fn to_assign(&self) -> Assign {
        match &self {
            Expression::Assign(e) => e.clone(),
            _ => panic!("Cannot cast to assign"),
        }
    }

    pub fn to_let(&self) -> LetExpr {
        match &self {
            Expression::Let(e) => e.clone(),
            _ => panic!("Cannot cast to assign"),
        }
    }

    pub fn to_let_ref(&self) -> LetRef {
        match &self {
            Expression::LetRef(e) => e.clone(),
            _ => panic!("Cannot cast to assign"),
        }
    }

    pub fn is_string_lit(&self) -> bool {
        match self {
            Expression::Literal(lit) => match lit.value {
                LiteralValue::String(_) => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub fn is_array_lit(&self) -> bool {
        match self {
            Expression::Literal(lit) => match lit.value {
                LiteralValue::Array(_) => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub fn is_function(&self) -> bool {
        match self {
            Expression::Function(_) => true,
            _ => false,
        }
    }

    pub fn create_let(
        name: Identifier,
        value: Option<Expression>,
        span: Span,
        let_token_span: Span,
    ) -> Expression {
        let value = if let Some(e) = value {
            Some(Box::new(e))
        } else {
            None
        };
        Expression::Let(LetExpr {
            name,
            value,
            span,
            let_token: let_token_span,
        })
    }

    pub fn create_binop(
        left: Expression,
        op: BinaryOperation,
        right: Expression,
        span: Span,
    ) -> Expression {
        Expression::BinOp(BinOp {
            left: Box::new(left),
            op,
            right: Box::new(right),
            span,
        })
    }

    pub fn create_literal(value: LiteralValue, span: Span) -> Expression {
        Expression::Literal(Literal { value, span })
    }

    pub fn create_assign(
        name: Identifier,
        rhs: Expression,
        span: Span,
        scope_index: Option<usize>,
    ) -> Expression {
        Expression::Assign(Assign {
            name,
            scope_distance: scope_index,
            rhs: Box::new(rhs),
            span,
        })
    }

    pub fn create_let_ref(ident: Identifier, span: Span, scope_index: Option<usize>) -> Expression {
        Expression::LetRef(LetRef {
            name: ident,
            span,
            scope_distance: scope_index,
        })
    }

    pub fn create_unaryop(op: UnaryOperation, rhs: Expression, span: Span) -> Expression {
        Expression::UnaryOp(UnaryOp {
            op,
            rhs: Box::new(rhs),
            span,
        })
    }

    pub fn create_grouping(expr: Expression, span: Span) -> Expression {
        Expression::Grouping(Grouping {
            expr: Box::new(expr),
            span,
        })
    }

    pub fn create_call(callee: Expression, args: Vec<CallArgs>, span: Span) -> Expression {
        Expression::Call(Call {
            callee: Box::new(callee),
            arguments: args,
            span,
        })
    }

    pub fn create_index(lhs: Expression, index: Expression, span: Span) -> Expression {
        Expression::GetIndex(GetIndex {
            lhs: Box::new(lhs),
            index: Box::new(index),
            span,
        })
    }

    pub fn create_set_index(
        lhs: Expression,
        index: Expression,
        value: Expression,
        span: Span,
    ) -> Expression {
        Expression::SetIndex(SetIndex {
            lhs: Box::new(lhs),
            index: Box::new(index),
            value: Box::new(value),
            span,
        })
    }

    pub fn create_get_property(
        object: Expression,
        name: Identifier,
        is_callable: bool,
        span: Span,
    ) -> Expression {
        Expression::GetProperty(GetProperty {
            object: Box::new(object),
            is_callable,
            name,
            span,
        })
    }

    pub fn create_set_property(
        object: Expression,
        name: Identifier,
        value: Expression,
        span: Span,
    ) -> Expression {
        Expression::SetProperty(SetProperty {
            object: Box::new(object),
            name,
            value: Box::new(value),
            span,
        })
    }

    pub fn create_function(
        name: Option<String>,
        params: Vec<Identifier>,
        return_type: Type,
        is_static: bool,
        body: Expression,
        span: Span,
    ) -> Expression {
        Expression::Function(Function {
            name,
            params,
            return_type,
            body: Box::new(body),
            is_static,
            span,
        })
    }

    pub fn create_if(
        condition: Expression,
        then: Expression,
        not_then: Option<Expression>,
        span: Span,
        if_token_span: Span,
    ) -> Expression {
        let not_then = if let Some(e) = not_then {
            Some(Box::new(e))
        } else {
            None
        };
        Expression::If(IfConditional {
            condition: Box::new(condition),
            then: Box::new(then),
            not_then,
            if_token: if_token_span,
            span,
        })
    }

    pub fn create_logic(
        lhs: Expression,
        op: LogicOperation,
        rhs: Expression,
        span: Span,
    ) -> Expression {
        Expression::Logic(Logic {
            lhs: Box::new(lhs),
            op,
            rhs: Box::new(rhs),
            span,
        })
    }

    pub fn create_block(exprs: Vec<Expression>, span: Span) -> Expression {
        Expression::Block(Block { exprs, span })
    }

    pub fn create_return(
        value: Option<Expression>,
        span: Span,
        return_token_span: Span,
    ) -> Expression {
        Expression::Return(ReturnExpr {
            value: Box::new(value),
            span,
            return_token: return_token_span,
        })
    }

    pub fn create_implicit_return(value: Expression, span: Span) -> Expression {
        Expression::ImplicitReturn(ImplicitReturn {
            value: Box::new(value),
            span,
        })
    }

    pub fn create_data_struct(
        name: Identifier,
        fields: Vec<Identifier>,
        methods: Vec<Expression>,
        span: Span,
    ) -> Expression {
        Expression::DataStruct(DataStruct {
            fields,
            name,
            methods,
            span,
        })
    }

    pub fn create_data_struct_instance(
        name: Identifier,
        fields: Vec<DataStructInstanceField>,
        span: Span,
    ) -> Expression {
        Expression::DataStructInstance(DataStructInstance { name, fields, span })
    }

    pub fn create_self(name: String, span: Span) -> Expression {
        Expression::SelfExpr(SelfExpr { name, span })
    }

    pub fn create_loop(body: Expression, loop_token_span: Span) -> Expression {
        Expression::LoopExpr(LoopExpr {
            body: Box::new(body),
            loop_token: loop_token_span,
        })
    }

    pub fn create_while(
        condition: Expression,
        body: Expression,
        while_token_span: Span,
    ) -> Expression {
        Expression::WhileExpr(WhileExpr {
            condition: Box::new(condition),
            body: Box::new(body),
            span: while_token_span.clone(),
            while_token: while_token_span,
        })
    }

    pub fn create_for(
        identifier: Identifier,
        iterator: Expression,
        body: Expression,
        span: Span,
        for_token_span: Span,
    ) -> Expression {
        Expression::ForExpr(ForExpr {
            identifier,
            body: Box::new(body),
            iterator: Box::new(iterator),
            span: span,
            for_token: for_token_span,
        })
    }

    pub fn create_break(span: Span) -> Expression {
        Expression::BreakExpr(BreakExpr { span })
    }

    pub fn create_continue(span: Span) -> Expression {
        Expression::ContinueExpr(ContinueExpr { span })
    }

    pub fn get_span(&self) -> Span {
        match &self {
            Expression::BinOp(s) => s.span.clone(),
            Expression::Literal(s) => s.span.clone(),
            Expression::Assign(s) => s.span.clone(),
            Expression::GetIndex(s) => s.span.clone(),
            Expression::SetIndex(s) => s.span.clone(),
            Expression::GetProperty(s) => s.span.clone(),
            Expression::SetProperty(s) => s.span.clone(),
            Expression::Let(s) => s.span.clone(),
            Expression::LetRef(s) => s.span.clone(),
            Expression::UnaryOp(s) => s.span.clone(),
            Expression::Grouping(s) => s.span.clone(),
            Expression::Logic(s) => s.span.clone(),
            Expression::Call(s) => s.span.clone(),
            Expression::Function(s) => s.span.clone(),
            Expression::DataStruct(s) => s.span.clone(),
            Expression::DataStructInstance(s) => s.span.clone(),
            Expression::Block(s) => s.span.clone(),
            Expression::If(s) => s.span.clone(),
            Expression::ImplicitReturn(s) => s.span.clone(),
            Expression::Return(s) => s.span.clone(),
            Expression::SelfExpr(s) => s.span.clone(),
            Expression::LoopExpr(s) => s.loop_token.clone(),
            Expression::WhileExpr(s) => s.span.clone(),
            Expression::ForExpr(s) => s.span.clone(),
            Expression::BreakExpr(s) => s.span.clone(),
            Expression::ContinueExpr(s) => s.span.clone(),
        }
    }
}
