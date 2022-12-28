use span_util::Span;

use super::Expression;

#[derive(Debug, PartialEq, Clone)]
pub struct LoopExpr {
    pub condition: Box<Expression>,
    pub body: Box<Expression>,
    pub span: Span,
    pub loop_token: Span,

    // this field is used for the 3 part for loop
    // e.g loop i = 0; i < 10; i += 1; {}
    // the 'i += 1' is important when using the continue expr in this kind of loop
    pub iterator: Option<Box<Expression>>,
}
