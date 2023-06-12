#[derive(Debug)]
pub struct Program {
    pub items: Vec<TopLevelItem>,
}

#[derive(Debug)]
pub enum TopLevelItem {
    Statement(Statement),
    FnDecl(FnDecl),
    NewFile(NewFile),
    Export(Export),
    Import(Import),
    ClassDecl(ClassDecl),
}

#[derive(Debug)]
pub enum ClassItem {
    Statement(Statement),
    FnDecl(FnDecl),
}

#[derive(Debug)]
pub enum Statement {
    Expr(ExpressionStatement),
    If(IfStatement),
    Assignment(AssignmentStatement),
    When(WhenStatement),
    Return(ReturnStatement),
    Delete(DeleteStatement),
    PlusEq(PlusEqStatement),
    MinusEq(MinusEqStatement),
    TimesEq(TimesEqStatement),
    DivideEq(DivideEqStatement),
    Noop(NoopStatement),
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub expr: Expression,
    pub debug: bool,
}

#[derive(Debug)]
pub enum Expression {
    /// Lowest precedence first

    /// Infix
    FourEq(FourEqExpr),
    ThreeEq(ThreeEqExpr),
    TwoEq(TwoEqExpr),
    OneEq(OneEqExpr),
    LooseAdd(LooseAddExpr),
    LooseSub(LooseSubExpr),
    LooseMul(LooseMulExpr),
    LooseDiv(LooseDivExpr),
    TightAdd(TightAddExpr),
    TightSub(TightSubExpr),
    TightMul(TightMulExpr),
    TightDiv(TightDivExpr),
    MemberFnCall(MemberFnCallExpr),

    /// Prefix
    Not(Box<Expression>),
    Previous(PreviousExpr),
    AwaitNext(AwaitNextExpr),
    New(NewExpr),
    PrefixPlusPlus(Box<Expression>),
    PrefixMinusMinus(Box<Expression>),

    /// Postfix
    PostfixPlusPlus(Box<Expression>),
    PostfixMinusMinus(Box<Expression>),

    /// Literals
    Array(ArrayExpr),
    FreeFnCall(FreeFnCallExpr),
    BoolLit(BoolLit),
    StringLit(StringLit),
    NumericLit(NumericLit),
    Null,
    Undefined,
}

#[derive(Debug)]
pub enum Literal {
    Bool(BoolLit),
    String(StringLit),
    Numeric(NumericLit),
    Ident(Ident),
}

#[derive(Debug)]
pub struct Ident {
    pub name: String,
}

#[derive(Debug)]
pub enum BoolLit {
    True,
    False,
    Maybe,
}

#[derive(Debug)]
pub struct StringLit {
    pub contents: String,
}

#[derive(Debug)]
pub struct NumericLit {
    pub digits: String,
}

#[derive(Debug)]
pub struct FreeFnCallExpr {
    pub fn_name: Ident,
    pub params: Vec<Expression>,
}

#[derive(Debug)]
pub struct MemberFnCallExpr {
    pub on: Box<Expression>,
    pub fn_name: Ident,
    pub params: Vec<Expression>,
}

#[derive(Debug)]
pub struct ArrayExpr {
    pub elems: Vec<Expression>,
}

#[derive(Debug)]
pub struct TightMulExpr {
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Debug)]
pub struct TightDivExpr {
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Debug)]
pub struct TightAddExpr {
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Debug)]
pub struct TightSubExpr {
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Debug)]
pub struct LooseMulExpr {
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Debug)]
pub struct LooseDivExpr {
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Debug)]
pub struct LooseAddExpr {
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Debug)]
pub struct LooseSubExpr {
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Debug)]
pub struct OneEqExpr {
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Debug)]
pub struct TwoEqExpr {
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Debug)]
pub struct ThreeEqExpr {
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Debug)]
pub struct FourEqExpr {
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Debug)]
pub struct PreviousExpr {
    pub variable: Ident,
}

#[derive(Debug)]
pub struct AwaitNextExpr {
    pub variable: Ident,
}

#[derive(Debug)]
pub struct NewExpr {
    pub class_name: Ident,
    pub params: Vec<Expression>,
}

#[derive(Debug)]
pub struct IfStatement {
    pub cond: Box<Expression>,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub struct AssignmentStatement {
    pub decl_type: DeclType,
    pub lhs: Literal,
    pub rhs: Expression,
    pub termination: StatementTermination,
}
#[derive(Debug)]
pub enum StatementTermination {
    Debug,
    Importance(usize),
}

impl StatementTermination {
    pub fn is_debug(&self) -> bool {
        matches!(self, Self::Debug)
    }
}

#[derive(Debug)]
pub enum DeclType {
    /// Reassignable, mutable
    VarVar,
    /// Reassignable, immutable
    VarConst,
    /// No reassignment, mutable
    ConstVar,
    /// No reassignment, immutable
    ConstConst,
    /// Global ConstConst
    ConstConstConst,
}

#[derive(Debug)]
pub struct WhenStatement {
    pub item: Literal,
    pub equals: Expression,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub value: Expression,
    pub debug: bool,
}

#[derive(Debug)]
pub struct DeleteStatement {
    pub target: DeleteTarget,
    pub debug: bool,
}

#[derive(Debug)]
pub struct PlusEqStatement {
    pub ident: Ident,
    pub expr: Expression,
    pub debug: bool,
}

#[derive(Debug)]
pub struct MinusEqStatement {
    pub ident: Ident,
    pub expr: Expression,
    pub debug: bool,
}

#[derive(Debug)]
pub struct TimesEqStatement {
    pub ident: Ident,
    pub expr: Expression,
    pub debug: bool,
}

#[derive(Debug)]
pub struct DivideEqStatement {
    pub ident: Ident,
    pub expr: Expression,
    pub debug: bool,
}

#[derive(Debug)]
pub enum DeleteTarget {
    Number(NumericLit),
    String(StringLit),
    Bool(BoolLit),
    Keyword(String),
}

#[derive(Debug)]
pub struct NoopStatement;

#[derive(Debug)]
pub struct FnDecl {
    pub is_async: bool,
    pub keyword: String,
    pub name: Ident,
    pub args: Vec<Ident>,
    pub body: FnBody,
}

#[derive(Debug)]
pub enum FnBody {
    ExprBody(Expression),
    BlockBody(Vec<Statement>),
}

#[derive(Debug)]
pub struct NewFile {
    pub name: Option<Ident>,
}

#[derive(Debug)]
pub struct Export {
    pub what: Ident,
    pub to: StringLit,
}

#[derive(Debug)]
pub struct Import {
    pub what: Ident,
}

#[derive(Debug)]
pub struct ClassDecl {
    pub keyword: String,
    pub name: Ident,
    pub body: Vec<ClassItem>,
}
