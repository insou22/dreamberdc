use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{anychar, char, digit1, multispace1, none_of, space0, space1},
    combinator::{eof, map, opt, peek, verify},
    error::ParseError,
    multi::{many0, many1, many1_count, separated_list0},
    sequence::tuple,
    IResult, Parser,
};
use nom_locate::LocatedSpan;

#[cfg(feature = "trace")]
use nom_tracable::{tracable_parser, TracableInfo};

use crate::types::*;

// vscode highlighting breaks on braces
// for some stupid reason...
// so hoist the broken highlighting out of the code
const LEFT_BRACE: char = '{';
const RIGHT_BRACE: char = '}';

type Error<'a> = nom::error::Error<Span<'a>>;
type Result<'a, T> = IResult<Span<'a>, T, Error<'a>>;

#[cfg(feature = "trace")]
type Span<'a> = LocatedSpan<&'a str, TracableInfo>;
#[cfg(not(feature = "trace"))]
type Span<'a> = LocatedSpan<&'a str>;

#[cfg_attr(feature = "trace", tracable_parser)]
pub fn parse_program(i: Span<'_>) -> Result<'_, Program> {
    map(
        tuple((
            many1(map(
                tuple((
                    comment_multispace0,
                    parse_top_level_item,
                    comment_multispace0,
                )),
                |(_, item, _)| item,
            )),
            eof,
        )),
        |(items, _)| Program { items },
    )(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_top_level_item(i: Span<'_>) -> Result<'_, TopLevelItem> {
    let (leftover, item) = alt((
        map(parse_statement, TopLevelItem::Statement),
        map(parse_fn_decl, TopLevelItem::FnDecl),
        map(parse_new_file, TopLevelItem::NewFile),
        map(parse_export, TopLevelItem::Export),
        map(parse_import, TopLevelItem::Import),
        map(parse_class_decl, TopLevelItem::ClassDecl),
    ))(i)?;

    Ok((leftover, item))
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_class_item(i: Span<'_>) -> Result<'_, ClassItem> {
    let (leftover, item) = alt((
        map(parse_statement, ClassItem::Statement),
        map(parse_fn_decl, ClassItem::FnDecl),
    ))(i)?;

    Ok((leftover, item))
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_statement(i: Span<'_>) -> Result<'_, Statement> {
    let (leftover, item) = alt((
        map(parse_expr_stmt, Statement::Expr),
        map(parse_if, Statement::If),
        map(parse_assignment, Statement::Assignment),
        map(parse_when, Statement::When),
        map(parse_return, Statement::Return),
        map(parse_delete, Statement::Delete),
        map(parse_plus_eq, Statement::PlusEq),
        map(parse_minus_eq, Statement::MinusEq),
        map(parse_times_eq, Statement::TimesEq),
        map(parse_divide_eq, Statement::DivideEq),
        map(parse_noop, Statement::Noop),
    ))(i)?;

    Ok((leftover, item))
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_fn_decl(i: Span<'_>) -> Result<'_, FnDecl> {
    let (leftover, (is_async, keyword, _, name, _, _, args, _, _, _, _, _, body)) = tuple((
        map(opt(tuple((tag("async"), comment_multispace1))), |opt| {
            opt.is_some()
        }),
        parse_fn_keyword,
        comment_multispace1,
        parse_ident,
        comment_multispace0,
        tag("("),
        separated_list0(
            tag(","),
            map(
                tuple((comment_multispace0, parse_ident, comment_multispace0)),
                |(_, ident, _)| ident,
            ),
        ),
        comment_multispace0,
        tag(")"),
        comment_multispace0,
        opt(tag("=>")),
        comment_multispace0,
        parse_fn_body,
    ))(i)?;

    Ok((
        leftover,
        FnDecl {
            is_async,
            keyword,
            name,
            args,
            body,
        },
    ))
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_fn_keyword(i: Span<'_>) -> Result<'_, String> {
    fn flatten(
        tuple: (
            Option<char>,
            Option<char>,
            Option<char>,
            Option<char>,
            Option<char>,
            Option<char>,
            Option<char>,
            Option<char>,
        ),
    ) -> String {
        let mut output = String::with_capacity(8);
        let mut push_if_some = |c| {
            if let Some(c) = c {
                output.push(c)
            }
        };

        push_if_some(tuple.0);
        push_if_some(tuple.1);
        push_if_some(tuple.2);
        push_if_some(tuple.3);
        push_if_some(tuple.4);
        push_if_some(tuple.5);
        push_if_some(tuple.6);
        push_if_some(tuple.7);

        output
    }

    verify(
        map(
            tuple((
                opt(char('f')),
                opt(char('u')),
                opt(char('n')),
                opt(char('c')),
                opt(char('t')),
                opt(char('i')),
                opt(char('o')),
                opt(char('n')),
            )),
            |keyword| flatten(keyword),
        ),
        |keyword: &String| !keyword.is_empty(),
    )(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_fn_body(i: Span<'_>) -> Result<'_, FnBody> {
    alt((
        map(parse_expr, FnBody::ExprBody),
        map(parse_block_body, FnBody::BlockBody),
    ))(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_block_body(i: Span<'_>) -> Result<'_, Vec<Statement>> {
    let (leftover, (_, _, statements, _, _)) = tuple((
        char(LEFT_BRACE),
        comment_multispace0,
        separated_list0(comment_multispace0, parse_statement),
        comment_multispace0,
        char(RIGHT_BRACE),
    ))(i)?;

    Ok((leftover, statements))
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_new_file(i: Span<'_>) -> Result<'_, NewFile> {
    alt((parse_new_named_file, parse_new_anonymous_file))(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_new_named_file(i: Span<'_>) -> Result<'_, NewFile> {
    let (leftover, (_, _, _, name, _, _)) = tuple((
        tag("====="),
        many0(tag("=")),
        space0,
        parse_ident,
        space0,
        many0(tag("=")),
    ))(i)?;

    Ok((leftover, NewFile { name: Some(name) }))
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_new_anonymous_file(i: Span<'_>) -> Result<'_, NewFile> {
    map(tuple((tag("====="), many0(tag("=")))), |_| NewFile {
        name: None,
    })(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_export(i: Span<'_>) -> Result<'_, Export> {
    map(
        tuple((
            tag("export"),
            space1,
            parse_ident,
            space1,
            tag("to"),
            space1,
            parse_string_lit,
            space1,
            parse_end_statement,
        )),
        |(_, _, what, _, _, _, to, _, _)| Export { what, to },
    )(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_import(i: Span<'_>) -> Result<'_, Import> {
    map(
        tuple((
            tag("import"),
            space1,
            parse_ident,
            space1,
            parse_end_statement,
        )),
        |(_, _, what, _, _)| Import { what },
    )(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_class_decl(i: Span<'_>) -> Result<'_, ClassDecl> {
    map(
        tuple((
            alt((tag("className"), tag("class"))),
            comment_multispace0,
            parse_ident,
            comment_multispace0,
            char(LEFT_BRACE),
            comment_multispace0,
            separated_list0(comment_multispace0, parse_class_item),
            comment_multispace0,
            char(RIGHT_BRACE),
        )),
        |(keyword, _, name, _, _, _, body, _, _)| ClassDecl {
            keyword: keyword.to_string(),
            name,
            body,
        },
    )(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_expr_stmt(i: Span<'_>) -> Result<'_, ExpressionStatement> {
    map(tuple((parse_expr, parse_end_statement)), |(expr, term)| {
        ExpressionStatement {
            expr,
            debug: term.is_debug(),
        }
    })(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_expr(i: Span<'_>) -> Result<'_, Expression> {
    parse_eq(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_if(i: Span<'_>) -> Result<'_, IfStatement> {
    map(
        tuple((
            tag("if"),
            comment_multispace0,
            tag("("),
            comment_multispace0,
            boxed(parse_expr),
            comment_multispace0,
            tag(")"),
            comment_multispace0,
            parse_block_body,
        )),
        |(_, _, _, _, cond, _, _, _, body)| IfStatement { cond, body },
    )(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_assignment(i: Span<'_>) -> Result<'_, AssignmentStatement> {
    map(
        tuple((
            parse_decl_type,
            comment_multispace1,
            parse_literal,
            comment_multispace0,
            tag("="),
            comment_multispace0,
            parse_expr,
            comment_multispace0,
            parse_end_statement,
        )),
        |(decl_type, _, lhs, _, _, _, rhs, _, termination)| AssignmentStatement {
            decl_type,
            lhs,
            rhs,
            termination,
        },
    )(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_decl_type(i: Span<'_>) -> Result<'_, DeclType> {
    alt((
        map(
            tuple((
                tag("const"),
                comment_multispace0,
                tag("const"),
                comment_multispace0,
                tag("const"),
            )),
            |_| DeclType::ConstConstConst,
        ),
        map(
            tuple((tag("const"), comment_multispace0, tag("const"))),
            |_| DeclType::ConstConst,
        ),
        map(
            tuple((tag("const"), comment_multispace0, tag("var"))),
            |_| DeclType::ConstVar,
        ),
        map(
            tuple((tag("var"), comment_multispace0, tag("const"))),
            |_| DeclType::VarConst,
        ),
        map(tuple((tag("var"), comment_multispace0, tag("var"))), |_| {
            DeclType::VarVar
        }),
    ))(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_when(i: Span<'_>) -> Result<'_, WhenStatement> {
    map(
        tuple((
            tag("when"),
            comment_multispace0,
            tag("("),
            comment_multispace0,
            parse_literal,
            comment_multispace0,
            tag("="),
            comment_multispace0,
            parse_expr,
            comment_multispace0,
            parse_block_body,
        )),
        |(_, _, _, _, item, _, _, _, equals, _, body)| WhenStatement { item, equals, body },
    )(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_return(i: Span<'_>) -> Result<'_, ReturnStatement> {
    map(
        tuple((
            tag("return"),
            comment_multispace0,
            parse_expr,
            comment_multispace0,
            parse_end_statement,
        )),
        |(_, _, value, _, term)| ReturnStatement {
            value,
            debug: term.is_debug(),
        },
    )(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_delete(i: Span<'_>) -> Result<'_, DeleteStatement> {
    map(
        tuple((
            tag("delete"),
            comment_multispace0,
            parse_delete_target,
            comment_multispace0,
            parse_end_statement,
        )),
        |(_, _, target, _, term)| DeleteStatement {
            target,
            debug: term.is_debug(),
        },
    )(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_plus_eq(i: Span<'_>) -> Result<'_, PlusEqStatement> {
    map(parse_op_eq('+'), |(ident, expr, debug)| PlusEqStatement {
        ident,
        expr,
        debug,
    })(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_minus_eq(i: Span<'_>) -> Result<'_, MinusEqStatement> {
    map(parse_op_eq('-'), |(ident, expr, debug)| MinusEqStatement {
        ident,
        expr,
        debug,
    })(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_times_eq(i: Span<'_>) -> Result<'_, TimesEqStatement> {
    map(parse_op_eq('*'), |(ident, expr, debug)| TimesEqStatement {
        ident,
        expr,
        debug,
    })(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_divide_eq(i: Span<'_>) -> Result<'_, DivideEqStatement> {
    map(parse_op_eq('/'), |(ident, expr, debug)| DivideEqStatement {
        ident,
        expr,
        debug,
    })(i)
}

fn parse_op_eq<'a>(op: char) -> impl Parser<Span<'a>, (Ident, Expression, bool), Error<'a>> {
    move |input| {
        map(
            tuple((
                parse_ident,
                comment_multispace0,
                char(op),
                char('='),
                comment_multispace0,
                parse_expr,
                parse_end_statement,
            )),
            |(ident, _, _, _, _, expr, term)| (ident, expr, term.is_debug()),
        )(input)
    }
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_delete_target(i: Span<'_>) -> Result<'_, DeleteTarget> {
    alt((
        map(parse_numeric_lit, DeleteTarget::Number),
        map(parse_string_lit, DeleteTarget::String),
        map(parse_bool_lit, DeleteTarget::Bool),
        map(parse_keyword, DeleteTarget::Keyword),
    ))(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_keyword(i: Span<'_>) -> Result<'_, String> {
    alt((
        parse_fn_keyword,
        boxed_string(alt((
            tag("async"),
            tag("export"),
            tag("import"),
            tag("class"),
            tag("className"),
            tag("if"),
            tag("const const const"),
            tag("const const"),
            tag("const var"),
            tag("var const"),
            tag("var var"),
            tag("const"),
            tag("var"),
            tag("when"),
            tag("return"),
            tag("delete"),
            tag("noop"),
            tag("previous"),
            tag("await"),
            tag("next"), // limit of 21 to a tuple
            alt((tag("new"), tag("null"), tag("undefined"))),
        ))),
    ))(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_noop(i: Span<'_>) -> Result<'_, NoopStatement> {
    map(
        tuple((tag("noop"), comment_multispace0, parse_end_statement)),
        |(..)| NoopStatement,
    )(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_eq(i: Span<'_>) -> Result<'_, Expression> {
    map(
        tuple((
            parse_loose_add_sub,
            many0(map(
                tuple((
                    comment_multispace0,
                    map(
                        alt((tag("===="), tag("==="), tag("=="), tag("="))),
                        |eq: Span| eq.len(),
                    ),
                    comment_multispace0,
                    boxed(parse_loose_add_sub),
                )),
                |(_, eq, _, value)| (eq, value),
            )),
        )),
        |(lhs, rhs)| {
            rhs.into_iter().fold(lhs, |acc, (eq, value)| match eq {
                1 => Expression::OneEq(OneEqExpr {
                    lhs: Box::new(acc),
                    rhs: value,
                }),
                2 => Expression::TwoEq(TwoEqExpr {
                    lhs: Box::new(acc),
                    rhs: value,
                }),
                3 => Expression::ThreeEq(ThreeEqExpr {
                    lhs: Box::new(acc),
                    rhs: value,
                }),
                4 => Expression::FourEq(FourEqExpr {
                    lhs: Box::new(acc),
                    rhs: value,
                }),
                _ => unreachable!(),
            })
        },
    )(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_loose_add_sub(i: Span<'_>) -> Result<'_, Expression> {
    map(
        tuple((
            parse_loose_mul_div,
            many0(map(
                tuple((
                    comment_multispace1,
                    alt((char('+'), char('-'))),
                    comment_multispace1,
                    boxed(parse_loose_mul_div),
                )),
                |(_, tag, _, value)| (tag, value),
            )),
        )),
        |(lhs, rhs)| {
            rhs.into_iter().fold(lhs, |acc, (tag, value)| match tag {
                '+' => Expression::LooseAdd(LooseAddExpr {
                    lhs: Box::new(acc),
                    rhs: value,
                }),
                '-' => Expression::LooseSub(LooseSubExpr {
                    lhs: Box::new(acc),
                    rhs: value,
                }),
                _ => unreachable!(),
            })
        },
    )(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_loose_mul_div(i: Span<'_>) -> Result<'_, Expression> {
    map(
        tuple((
            parse_tight_add_sub,
            many0(map(
                tuple((
                    comment_multispace1,
                    alt((char('*'), char('/'))),
                    comment_multispace1,
                    boxed(parse_tight_add_sub),
                )),
                |(_, tag, _, value)| (tag, value),
            )),
        )),
        |(lhs, rhs)| {
            rhs.into_iter().fold(lhs, |acc, (tag, value)| match tag {
                '*' => Expression::LooseMul(LooseMulExpr {
                    lhs: Box::new(acc),
                    rhs: value,
                }),
                '/' => Expression::LooseDiv(LooseDivExpr {
                    lhs: Box::new(acc),
                    rhs: value,
                }),
                _ => unreachable!(),
            })
        },
    )(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_tight_add_sub(i: Span<'_>) -> Result<'_, Expression> {
    map(
        tuple((
            parse_tight_mul_div,
            many0(map(
                tuple((alt((char('+'), char('-'))), boxed(parse_tight_mul_div))),
                |(tag, value)| (tag, value),
            )),
        )),
        |(lhs, rhs)| {
            rhs.into_iter().fold(lhs, |acc, (tag, value)| match tag {
                '+' => Expression::TightAdd(TightAddExpr {
                    lhs: Box::new(acc),
                    rhs: value,
                }),
                '-' => Expression::TightSub(TightSubExpr {
                    lhs: Box::new(acc),
                    rhs: value,
                }),
                _ => unreachable!(),
            })
        },
    )(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_tight_mul_div(i: Span<'_>) -> Result<'_, Expression> {
    map(
        tuple((
            parse_member_fn_call,
            many0(map(
                tuple((alt((char('*'), char('/'))), boxed(parse_member_fn_call))),
                |(tag, value)| (tag, value),
            )),
        )),
        |(lhs, rhs)| {
            rhs.into_iter().fold(lhs, |acc, (tag, value)| match tag {
                '*' => Expression::TightMul(TightMulExpr {
                    lhs: Box::new(acc),
                    rhs: value,
                }),
                '/' => Expression::TightDiv(TightDivExpr {
                    lhs: Box::new(acc),
                    rhs: value,
                }),
                _ => unreachable!(),
            })
        },
    )(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_member_fn_call(i: Span<'_>) -> Result<'_, Expression> {
    map(
        tuple((
            parse_postfix_op_expr,
            many0(map(
                tuple((
                    comment_multispace0,
                    tag("."),
                    comment_multispace0,
                    parse_free_fn_call,
                )),
                |(_, _, _, value)| match value {
                    Expression::FreeFnCall(fn_call) => fn_call,
                    _ => unreachable!(),
                },
            )),
        )),
        |(lhs, rhs)| {
            rhs.into_iter()
                .fold(lhs, |acc, FreeFnCallExpr { fn_name, params }| {
                    Expression::MemberFnCall(MemberFnCallExpr {
                        on: Box::new(acc),
                        fn_name,
                        params,
                    })
                })
        },
    )(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_postfix_op_expr(i: Span<'_>) -> Result<'_, Expression> {
    alt((
        parse_postfix_plus_plus,
        parse_postfix_minus_minus,
        parse_prefix_op_expr,
    ))(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_postfix_plus_plus(i: Span<'_>) -> Result<'_, Expression> {
    map(
        tuple((boxed(parse_prefix_op_expr), comment_multispace0, tag("++"))),
        |(expr, _, _)| Expression::PostfixPlusPlus(expr),
    )(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_postfix_minus_minus(i: Span<'_>) -> Result<'_, Expression> {
    map(
        tuple((boxed(parse_prefix_op_expr), comment_multispace0, tag("--"))),
        |(expr, _, _)| Expression::PostfixMinusMinus(expr),
    )(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_prefix_op_expr(i: Span<'_>) -> Result<'_, Expression> {
    alt((
        parse_prefix_plus_plus,
        parse_prefix_minus_minus,
        parse_not,
        parse_previous,
        parse_await_next,
        parse_new,
        parse_base_expr,
    ))(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_prefix_plus_plus(i: Span<'_>) -> Result<'_, Expression> {
    map(
        tuple((tag("++"), comment_multispace0, boxed(parse_prefix_op_expr))),
        |(_, _, expr)| Expression::PrefixPlusPlus(expr),
    )(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_prefix_minus_minus(i: Span<'_>) -> Result<'_, Expression> {
    map(
        tuple((tag("--"), comment_multispace0, boxed(parse_prefix_op_expr))),
        |(_, _, expr)| Expression::PrefixMinusMinus(expr),
    )(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_not(i: Span<'_>) -> Result<'_, Expression> {
    map(
        tuple((char(';'), comment_multispace0, boxed(parse_prefix_op_expr))),
        |(_, _, value)| Expression::Not(value),
    )(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_previous(i: Span<'_>) -> Result<'_, Expression> {
    map(
        tuple((tag("previous"), comment_multispace1, parse_ident)),
        |(_, _, variable)| Expression::Previous(PreviousExpr { variable }),
    )(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_await_next(i: Span<'_>) -> Result<'_, Expression> {
    map(
        tuple((
            tag("await"),
            comment_multispace1,
            tag("next"),
            comment_multispace1,
            parse_ident,
        )),
        |(_, _, _, _, variable)| Expression::AwaitNext(AwaitNextExpr { variable }),
    )(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_new(i: Span<'_>) -> Result<'_, Expression> {
    map(
        tuple((tag("new"), comment_multispace1, parse_free_fn_call)),
        |(_, _, fn_call)| match fn_call {
            Expression::FreeFnCall(FreeFnCallExpr {
                fn_name: class_name,
                params,
            }) => Expression::New(NewExpr { class_name, params }),
            _ => unreachable!(),
        },
    )(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_base_expr(i: Span<'_>) -> Result<'_, Expression> {
    alt((
        parse_array,
        parse_free_fn_call,
        parse_bool_lit_expr,
        parse_string_lit_expr,
        parse_numeric_lit_expr,
        parse_null,
        parse_undefined,
        map(
            tuple((
                char('('),
                comment_multispace0,
                parse_expr,
                comment_multispace0,
                char(')'),
            )),
            |(_, _, expr, _, _)| expr,
        ),
    ))(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_array(i: Span<'_>) -> Result<'_, Expression> {
    map(
        tuple((
            tag("["),
            comment_multispace0,
            separated_list0(
                tuple((comment_multispace0, tag(","), comment_multispace0)),
                parse_expr,
            ),
            comment_multispace0,
            opt(tag(",")),
            comment_multispace0,
            tag("]"),
        )),
        |(_, _, elems, _, _, _, _)| Expression::Array(ArrayExpr { elems }),
    )(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_free_fn_call(i: Span<'_>) -> Result<'_, Expression> {
    map(
        tuple((
            parse_ident,
            comment_multispace0,
            tag("("),
            comment_multispace0,
            separated_list0(
                tuple((comment_multispace0, tag(","), comment_multispace0)),
                parse_expr,
            ),
            comment_multispace0,
            opt(tag(",")),
            comment_multispace0,
            tag(")"),
        )),
        |(fn_name, _, _, _, params, _, _, _, _)| {
            Expression::FreeFnCall(FreeFnCallExpr { fn_name, params })
        },
    )(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_null(i: Span<'_>) -> Result<'_, Expression> {
    map(tag("null"), |_| Expression::Null)(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_undefined(i: Span<'_>) -> Result<'_, Expression> {
    map(tag("undefined"), |_| Expression::Undefined)(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_bool_lit_expr(i: Span<'_>) -> Result<'_, Expression> {
    map(parse_bool_lit, Expression::BoolLit)(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_string_lit_expr(i: Span<'_>) -> Result<'_, Expression> {
    map(parse_string_lit, Expression::StringLit)(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_numeric_lit_expr(i: Span<'_>) -> Result<'_, Expression> {
    map(parse_numeric_lit, Expression::NumericLit)(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_bool_lit(i: Span<'_>) -> Result<'_, BoolLit> {
    alt((
        map(tag("true"), |_| BoolLit::True),
        map(tag("false"), |_| BoolLit::False),
        map(tag("maybe"), |_| BoolLit::Maybe),
    ))(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_string_lit(i: Span<'_>) -> Result<'_, StringLit> {
    map(
        tuple((
            char('"'),
            map(is_not("\""), |contents: Span| contents.to_string()),
            char('"'),
        )),
        |(_, contents, _)| StringLit { contents },
    )(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_numeric_lit(i: Span<'_>) -> Result<'_, NumericLit> {
    map(
        tuple((
            digit1,
            opt(tuple((tag("."), digit1))),
            peek(verify(anychar, |char: &char| char.is_ascii())),
        )),
        |(whole, fractional, _)| NumericLit {
            digits: format!(
                "{whole}{}",
                match fractional {
                    Some((_, fractional)) => format!(".{fractional}"),
                    None => String::new(),
                }
            ),
        },
    )(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_literal(i: Span<'_>) -> Result<'_, Literal> {
    let (leftover, item) = alt((
        map(parse_bool_lit, Literal::Bool),
        map(parse_string_lit, Literal::String),
        map(parse_numeric_lit, Literal::Numeric),
        map(parse_ident, Literal::Ident),
    ))(i)?;

    Ok((leftover, item))
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_ident(i: Span<'_>) -> Result<'_, Ident> {
    let (leftover, ident) = many1(none_of(" \t\r\n,()"))(i)?;

    Ok((
        leftover,
        Ident {
            name: ident.into_iter().collect(),
        },
    ))
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn parse_end_statement(i: Span<'_>) -> Result<'_, StatementTermination> {
    alt((
        map(char('?'), |_| StatementTermination::Debug),
        map(many1_count(char('!')), StatementTermination::Importance),
        map(
            tuple((space0, comment0, many0(char('\r')), char('\n'))),
            |_| StatementTermination::Importance(0),
        ),
    ))(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
pub fn comment_multispace0(i: Span<'_>) -> Result<()> {
    map(opt(comment_multispace1), |_| ())(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn comment_multispace1(i: Span<'_>) -> Result<'_, ()> {
    map(
        many1(alt((
            map(multispace1, |_| ()),
            map(
                tuple((tag("//"), many0(none_of("\n")), opt(char('\n')))),
                |_| (),
            ),
        ))),
        |_| (),
    )(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
pub fn comment0(i: Span<'_>) -> Result<()> {
    map(opt(comment1), |_| ())(i)
}

#[cfg_attr(feature = "trace", tracable_parser)]
fn comment1(i: Span<'_>) -> Result<'_, ()> {
    map(
        map(tuple((tag("//"), many0(none_of("\n")))), |_| ()),
        |_| (),
    )(i)
}

fn boxed<I, O, E, F>(mut parser: F) -> impl FnMut(I) -> IResult<I, Box<O>, E>
where
    E: ParseError<I>,
    F: Parser<I, O, E>,
{
    move |input: I| parser.parse(input).map(|(i, o)| (i, Box::new(o)))
}

fn boxed_string<I, O, E, F>(mut parser: F) -> impl FnMut(I) -> IResult<I, String, E>
where
    E: ParseError<I>,
    F: Parser<I, O, E>,
    O: AsRef<str>,
{
    move |input: I| {
        parser
            .parse(input)
            .map(|(i, o)| (i, String::from(o.as_ref())))
    }
}
