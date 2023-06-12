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
    Expr(Expr),
    If(IfStatement),
    Assignment(AssignmentStatement),
    When(WhenStatement),
    Return(ReturnStatement),
    Delete(DeleteStatement),
    Noop(NoopStatement),
}

#[derive(Debug)]
pub enum Expr {
    /// Lowest precedence first

    /// Infix
    FourEq(FourEqExpr),
    ThreeEq(ThreeEqExpr),
    TwoEq(TwoEqExpr),
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
    Not(Box<Expr>),
    Previous(PreviousExpr),
    AwaitNext(AwaitNextExpr),
    New(NewExpr),

    /// Literals
    Array(ArrayExpr),
    FreeFnCall(FreeFnCallExpr),
    BoolLit(BoolLit),
    StringLit(StringLit),
    NumericLit(NumericLit),
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
    pub params: Vec<Expr>,
}

#[derive(Debug)]
pub struct MemberFnCallExpr {
    pub on: Box<Expr>,
    pub fn_name: Ident,
    pub params: Vec<Expr>,
}

#[derive(Debug)]
pub struct ArrayExpr {
    pub elems: Vec<Expr>,
}

#[derive(Debug)]
pub struct TightMulExpr {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct TightDivExpr {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct TightAddExpr {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct TightSubExpr {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct LooseMulExpr {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct LooseDivExpr {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct LooseAddExpr {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct LooseSubExpr {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct TwoEqExpr {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct ThreeEqExpr {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct FourEqExpr {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
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
}

#[derive(Debug)]
pub struct IfStatement {
    pub cond: Box<Expr>,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub struct AssignmentStatement {
    pub decl_type: DeclType,
    pub lhs: Literal,
    pub rhs: Expr,
    pub importance: usize,
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
    pub equals: Expr,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub value: Expr,
}

#[derive(Debug)]
pub struct DeleteStatement {
    pub target: DeleteTarget,
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
    ExprBody(Expr),
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
