use std::collections::{BTreeMap, BTreeSet};
use std::io::stdin;

use once_cell::sync::OnceCell;
use regex::Regex;

mod lib;

fn main() {
    let engine = lib::LambdaCalculusEngine::new();
    let mut variables = BTreeMap::new();
    process_line(&engine, &mut variables, ":tru=/x./y.x");
    process_line(&engine, &mut variables, ":fls=/x./y.y");
    process_line(&engine, &mut variables, ":test=/l./m./n.l m n");
    process_line(&engine, &mut variables, ":and=/b./c.b c fls");
    process_line(&engine, &mut variables, ":or=/b./c.b tru c");
    process_line(&engine, &mut variables, ":not=/b.b fls tru");
    process_line(&engine, &mut variables, ":pair=/f./s./b.b f s");
    process_line(&engine, &mut variables, ":fst=/p.p tru");
    process_line(&engine, &mut variables, ":snd=/p.p fls");
    process_line(&engine, &mut variables, ":0=/s./z.z");
    process_line(&engine, &mut variables, ":scc=/n./s./z.s(n s z)");
    process_line(&engine, &mut variables, ":iszero=/m.m(/x.fls)tru");
    process_line(&engine, &mut variables, ":id=/x.x");
    loop {
        let mut s = String::new();
        stdin().read_line(&mut s).expect("failed to read input");
        if process_line(&engine, &mut variables, &s) { break; }
    }
}

fn process_line(engine: &lib::LambdaCalculusEngine, variables: &mut BTreeMap<String, lib::SimpleExpression>, s: &str) -> bool {
    static REGEX: OnceCell<Regex> = OnceCell::new();
    let s = s.trim();
    if s.is_empty() { return false; }
    let (acc, expr) = if let Some(cap) = REGEX.get_or_init(|| Regex::new("^:(?P<name>\\w+)\\s*=\\s*(?P<expr>.+)$").unwrap()).captures(s) {
        (Some(cap.name("name").unwrap().as_str()), Some(cap.name("expr").unwrap().as_str()))
    } else if s.chars().next().unwrap() != ':' {
        (None, Some(s))
    } else {
        (None, None)
    };
    if let Some(expr) = expr {
        match engine.parse(expr) {
            Ok(expr) => {
                if let Some(expr) = evaluate(variables, expr) {
                    let name = acc.unwrap_or("ans");
                    println!(">{}", name);
                    variables.insert(name.to_owned(), expr);
                }
            }
            Err(err) => println!("{:?}", err),
        }
        return false;
    }
    match s {
        ":vars" => {
            for (key, val) in variables {
                println!("{}\t= {}", key, val);
            }
        }
        ":exit" => {
            println!("bye");
            return true;
        }
        s => {
            println!("unknown command {}", s);
        }
    }
    false
}

fn evaluate(variables: &mut BTreeMap<String, lib::SimpleExpression>, mut expr: lib::SimpleExpression) -> Option<lib::SimpleExpression> {
    let mut set = BTreeSet::new();
    loop {
        println!("{}", expr);
        if set.contains(&expr) {
            println!("infinity loop");
            break None;
        }
        set.insert(expr.clone());
        match expr.reduction_first(variables) {
            Ok(result) => expr = result,
            Err(expr) => break Some(expr),
        }
    }
}
