use crate::{ast, token_map::TokenMap};
use anyhow::Result;
use std::fmt::Write;

pub fn generate(
    rust_source: &str,
    ast: crate::ast::Ast<'_>,
    token_map: crate::token_map::TokenMap,
) -> Result<String> {
    let parser_content = _generate(ast, token_map)?;

    Ok(format!(
        r###"mod parser{{
    #![allow(non_snake_case)]
    #![allow(unused_braces)]
    #![allow(clippy::all)]
    #![deny(clippy::correctness)]

    // ------------------------------------------------------------------
    // User code
    // ------------------------------------------------------------------

{}

    // ------------------------------------------------------------------
    // Parser
    // ------------------------------------------------------------------

{}

    // ------------------------------------------------------------------
    // Intern
    // ------------------------------------------------------------------

    #[allow(unused)]
    use ::toktok::Parser as _;
    mod __intern__ {{
        pub use ::toktok::{{combinator as c, State, Input, PResult, Parser, Error, ParserError}};
    }}
}}
    "###,
        rust_source, parser_content,
    ))
}

fn _generate(ast: ast::Ast<'_>, token_map: TokenMap) -> Result<String> {
    // Build output
    let mut output = String::new();
    for rule in ast.rules {
        output += &generate_rule(rule, &token_map)?;
        output += "\n\n";
    }

    Ok(output)
}

fn common_combinators(rule: &ast::Rule<'_>) -> usize {
    // If any production is fallible, common combinators can not be used
    if rule.productions.iter().any(|p| p.is_fallible) {
        return 0;
    }

    // If there is only one rule, common combinators can not be used
    if rule.productions.len() == 1 {
        return 0;
    }

    // Calc common combinators
    let mut n = 0;
    loop {
        let c = &rule.productions[0].combinator.combinators[n];
        for prod in rule.productions.iter().skip(1) {
            match prod.combinator.combinators.get(n) {
                Some(c2) if c == c2 => (),
                _ => return n,
            }
        }
        n += 1;
    }
}

fn generate_rule(rule: ast::Rule<'_>, token_map: &TokenMap) -> Result<String> {
    // Calc common combinators
    let common_combinators = common_combinators(&rule);

    // Body of the rule fn
    let mut body = String::new();

    // Check if we need to calc span. If yes, generate __span_start__
    let calc_span = rule.productions.iter().any(|p| p.rust_expression.0.contains("$span"));
    if calc_span {
        body += "let __span_start__ = __state__.input().positioned_start();\n\n";
    }

    // Generate common combinators
    if common_combinators != 0 {
        for idx in 1..=common_combinators {
            let combinator = &rule.productions[0].combinator.combinators[idx - 1];
            write!(
                &mut body,
                "// Common combinator {}: {}\n",
                idx,
                clear_line_breaks(combinator.source())
            )?;
            write!(&mut body, "let (__state__, __c_{}__) = ", idx)?;
            body += &generate_combinator(combinator, token_map)?;
            body += ".parse(__state__)?;\n";
        }
        body += "\n";
    }

    // Generate productions
    for (idx, production) in rule.productions.iter().enumerate() {
        write!(&mut body, "// Production {}: {}\n", idx + 1, clear_line_breaks(production.source))?;
        body += &generate_production_match_block(
            production,
            calc_span,
            idx == rule.productions.len() - 1,
            common_combinators,
            token_map,
        )?;
        if idx != rule.productions.len() - 1 {
            body += "\n\n";
        }
    }

    // Generate rule fn
    Ok(format!(
        r#"{}fn {}<'s, 't>(
    __state__: __intern__::State<'s, 't, Token>
) -> __intern__::PResult<'s, 't, Token, {}> where 's: 't {{
{}
}}"#,
        if rule.is_public { "pub " } else { "" },
        rule.name.0,
        rule.ret_type.0,
        body,
    ))
}

fn generate_production_match_block(
    production: &ast::Production<'_>,
    calc_span: bool,
    is_last: bool,
    skip_combinators: usize,
    token_map: &TokenMap,
) -> Result<String> {
    let (let_, input, error_expr, sem) = if is_last {
        ("", "", "Err(__e__)", "")
    } else {
        ("let __state__ = ", "let __input__ = __state__.input();", "__e__.recover(__input__)?", ";")
    };

    let output = format!(
        r#"{}{{
    {}
    match {} {{
        Ok((__state__, {})) => return {{
            {}
            {}
        }},
        Err(__e__) => {},
    }}
}}{}"#,
        let_,
        input,
        generate_production(production, skip_combinators, token_map)?,
        c_tuple(1 + skip_combinators, production.combinator.combinators.len()),
        if calc_span {
            r#"let __span_end__ = __state__.input().positioned_end(__span_start__);
            let __span__ = __span_start__..__span_end__;"#
        } else {
            ""
        },
        if production.is_fallible {
            format!(
                r#"// Closure is needed to ensure res_block errors are catched
            match (move || {})() {{
                Ok(__ok__) => Ok((__state__, __ok__)),
                Err(__e__) => Err(__state__.and_error(__e__)),
            }}"#,
                generate_rust_expression(production)
            )
        } else {
            format!(
                r#"// Closure is needed to ensure res_block does not return any errors
            Ok((__state__, (move || {})()))"#,
                generate_rust_expression(production)
            )
        },
        error_expr,
        sem,
    );

    Ok(output)
}

fn generate_production(
    production: &ast::Production<'_>,
    skip_combinators: usize,
    token_map: &TokenMap,
) -> Result<String> {
    let mut combinators = production.combinator.combinators.iter().skip(skip_combinators);
    let mut output = match combinators.next() {
        Some(combinator) => generate_combinator(combinator, token_map)?,
        None => return Ok("Ok::<_, __intern__::ParserError<Token>>((__state__, ()))".to_string()),
    };

    for combinator in combinators {
        let c = generate_combinator(combinator, token_map)?;
        output = format!("__intern__::c::pair({}, {})", output, c);
    }

    output += ".parse(__state__)";

    Ok(output)
}

fn c_tuple(from: usize, to: usize) -> String {
    if from > to {
        "()".to_string()
    } else if from == to {
        format!("__c_{}__", to)
    } else {
        format!("({}, __c_{}__)", c_tuple(from, to - 1), to,)
    }
}

fn generate_combinator(combinator: &ast::Combinator<'_>, token_map: &TokenMap) -> Result<String> {
    let output = match combinator {
        ast::Combinator::Seq(seq) => {
            let mut combinators = seq.combinators.iter();
            let init = generate_combinator(
                combinators.next().expect("seq with zero combinators"),
                token_map,
            )?;
            let pair_combinator = combinators.try_fold::<_, _, Result<_>>(init, |lhs, rhs| {
                Ok(format!(
                    "__intern__::c::pair({}, {})",
                    lhs,
                    generate_combinator(rhs, token_map)?
                ))
            })?;

            if seq.combinators.len() > 2 {
                // Flatten tuple
                // __pair__ = (((a, b), c), d) => (__pair__.0.0.0, __pair__.0.0.1, __pair__.0.1, __pair__.1, )
                let mut tuple = "(".to_string();
                for idx in 0..seq.combinators.len() {
                    tuple += "__pair__";
                    for _ in 0..(seq.combinators.len() - idx - 1) {
                        tuple += ".0";
                    }
                    if idx != 0 {
                        tuple += ".1";
                    }
                    tuple += ", ";
                }
                tuple += ")";

                format!("{}.map(|__pair__| {})", pair_combinator, tuple)
            } else {
                pair_combinator
            }
        }
        ast::Combinator::Choice(choice) => {
            let mut combinators = choice.combinators.iter();
            let init = generate_combinator(
                combinators.next().expect("choice with zero combinators"),
                token_map,
            )?;
            combinators.try_fold::<_, _, Result<_>>(init, |lhs, rhs| {
                Ok(format!("__intern__::c::alt({}, {})", lhs, generate_combinator(rhs, token_map)?))
            })?
        }
        ast::Combinator::Opt(opt) => {
            format!("__intern__::c::opt({})", generate_combinator(&opt.combinator, token_map)?)
        }
        ast::Combinator::Many0(many0) => {
            format!("__intern__::c::many0({})", generate_combinator(&many0.combinator, token_map)?)
        }
        ast::Combinator::Many1(many1) => {
            format!("__intern__::c::many1({})", generate_combinator(&many1.combinator, token_map)?)
        }
        ast::Combinator::Atom(atom) => match &atom.kind {
            ast::CombinatorAtomKind::Path(path) => path.0.to_string(),
            ast::CombinatorAtomKind::TokenShort(token_short) => token_map.map(token_short.0)?,
            ast::CombinatorAtomKind::FunctionCall(function_call) => {
                let mut output = String::new();
                output += function_call.name.0;
                output += "(";
                for arg in &function_call.args {
                    output += &generate_combinator(arg, token_map)?;
                    output += ",";
                }
                output += ")";
                output
            }
        },
    };

    Ok(output)
}

fn generate_rust_expression(production: &ast::Production<'_>) -> String {
    let mut res = production.rust_expression.0.replace("$span", "__span__");
    for idx in 1..=production.combinator.combinators.len() {
        res = res.replace(&format!("${}", idx), &format!("__c_{}__", idx));
    }
    res
}

fn clear_line_breaks(s: &str) -> String {
    let s = s.replace("\r\n", " ");
    let s = s.replace("\n", " ");
    s
}
