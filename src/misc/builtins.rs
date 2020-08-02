use std::error::Error;
use std::collections::HashMap;

#[derive(Debug)]
struct CompilationError(String);

impl std::fmt::Display for CompilationError {
    fn fmt (&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "A compilation error has occured: {}", self.0)
    }
} 

impl Error for CompilationError {}

#[derive(Clone)]
enum EnvElement {
    Func(fn(SExp, &mut Env) -> SExp),
    SExp(SExp),
}

struct Env {
    map: HashMap<String, Vec<(EnvElement, bool)>>
}

impl Env {
    pub fn new() -> Self {
        Self {
            map: HashMap::<String, Vec<(EnvElement, bool)>>::new()
        }
    }

    pub fn insert_compiled_func(&mut self, key: &String, value: fn(SExp, &mut Env) -> SExp) {
        match self.map.get_mut(key) {
            Some(vector) => {
                vector.push((EnvElement::Func(value), true));
            }

            None => {
                let vector = vec![(EnvElement::Func(value), true)];
                let key_copy = key.clone();
                self.map.insert(key_copy, vector);
            }
        }
    }

    pub fn insert(&mut self, key: &String, value: SExp, is_func: bool) {
        match self.map.get_mut(key) {
            Some(vector) => {
                vector.push((EnvElement::SExp(value), is_func));
            }

            None => {
                let vector = vec![(EnvElement::SExp(value), is_func)];
                let key_copy = (*key).clone();
                self.map.insert(key_copy, vector);
            }
        }
    }

    pub fn get(&mut self, key: &String) -> Option<(EnvElement, bool)> {
        match self.map.get_mut(key) {
            Some(vector) => {
                match vector.last() {
                    Some(elem) => {
                        return Some((*elem).clone());
                    }

                    None => {
                        return None;
                    }
                }
            }

            None => {
                return None;
            }
        }
    }

    pub fn pop(&mut self, key: &String) {
        match self.map.get_mut(key) {
            Some(vector) => {
                vector.pop();
            }
            
            None => {}
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
enum Atom {
    SYMBOL(String),
    NUM(i64),
    NIL, 
}

#[derive(Clone, Debug, PartialEq)]
enum SExp {
    ATOM(Atom),
    CONS(Box<SExp>, Box<SExp>)
}

fn add_bindings_recursively(func_params: &SExp, args: SExp, env: &mut Env) {
    if let SExp::CONS(param, params) = func_params {
        if let SExp::ATOM(Atom::SYMBOL(name)) = &**param {
            if let SExp::CONS(arg, args_tail) = args {
                env.insert(&name, *arg, false);
                add_bindings_recursively(&*params, *args_tail, env);
            }
        }
    }
}

fn remove_bindings_from_env(params: SExp, env: &mut Env) {
    if let SExp::CONS(param, rest) = params {
        if let SExp::ATOM(atom) = *param {
            if let Atom::SYMBOL(name) = atom {
                env.pop(&name);
                remove_bindings_from_env(*rest, env);
            }
        }
    }
}

fn call_function(func: SExp, args: SExp, env: &mut Env) -> SExp {
    match func {
        SExp::CONS(params, func_decl) => {
            add_bindings_recursively(&*params, args, env);
            if let SExp::CONS(body, _) = *func_decl {
                let result = evaluate(*body, env);
                remove_bindings_from_env(*params, env);

                return result;
            } else {
                panic!("Invalid function params: {:?}", params);
            }
        }

        x => panic!("invalid func s-exp: {:?}", x),
    }
}

fn evaluate(sexp: SExp, env: &mut Env) -> SExp {
    match sexp {
        SExp::ATOM(atom) => {
            match atom {
                Atom::SYMBOL(name) => {
                    match env.get(&name).unwrap().0 {
                        EnvElement::SExp(sexp) => sexp,
                        _ => panic!("cannot evaluate compiled function"),
                    } 
                    // env.get(&name).unwrap().0
                }
                _ => SExp::ATOM(atom.clone())
            }
        }

        SExp::CONS(car, cdr) => {
            match *car {
                SExp::ATOM(atom) => {
                    match atom {
                        Atom::SYMBOL(name) => {
                            match name.as_str() {
                                "quote" => {
                                    match *cdr {
                                        SExp::CONS(param, _nil_terminator) => *param,
                                        _ => panic!("Invalid param in quote"),
                                    }
                                }

                                "eval" => evaluate(*cdr, env),
                                "read" => lisp_read(),

                                _ => match env.get(&name) {
                                    Some(value) => { 
                                        match value.0 {
                                            EnvElement::Func(f) => {
                                                f(*cdr, env)
                                            }

                                            EnvElement::SExp(sexp) => {
                                                if value.1 { // if it's a function
                                                    // call_function(value.0, *cdr, env)
                                                    panic!("calling functions not yet impl.")
                                                } else {
                                                    SExp::CONS(Box::new(sexp), Box::new(evaluate(*cdr, env)))
                                                }
                                            }
                                        }
                                    }
                                    None => panic!(format!("Symbol \"{}\" not found", name)),
                                }
                            }
                        }
                            
                        x => SExp::CONS(Box::new(SExp::ATOM(x.clone())), Box::new(*cdr.clone())),
                    }
                }

                x => SExp::CONS(Box::new(evaluate(x, env)), Box::new(evaluate(*cdr, env))),
            }
        }
    }
}

fn math_op(exp: SExp, env: &mut Env, func: fn(i64, i64) -> i64) -> SExp {
    if let SExp::CONS(first, rest) = exp {
        if let SExp::CONS(second, _) = *rest {
            if let SExp::ATOM(atom1) = evaluate(*first, env) {
                if let SExp::ATOM(atom2) = evaluate(*second, env) {
                    if let Atom::NUM(num1) = atom1 {
                        if let Atom::NUM(num2) = atom2 {
                            return SExp::ATOM(Atom::NUM(func(num1, num2)));
                        }
                    }
                }
            }
        }
    }

    panic!("invalid s-exp in math_op"); 
}

fn add(exp: SExp, env: &mut Env) -> SExp {
    math_op(exp, env, |x, y| x + y)
}

fn sub(exp: SExp, env: &mut Env) -> SExp {
    math_op(exp, env, |x, y| x - y)
}

fn mul(exp: SExp, env: &mut Env) -> SExp {
    math_op(exp, env, |x, y| x * y)
}

fn div(exp: SExp, env: &mut Env) -> SExp {
    math_op(exp, env, |x, y| x / y)
}

fn eq(exp: SExp, env: &mut Env) -> SExp {
    if let SExp::CONS(first_val, rest) = exp {
        if let SExp::CONS(second_val, _) = *rest {
            if evaluate(*first_val, env) == evaluate(*second_val, env) {
                return SExp::ATOM(Atom::NUM(1));
            } else {
                return SExp::ATOM(Atom::NIL);
            }
        }
    }

    panic!("invalid exp in eq");
}

fn lisp_print(exp: SExp, env: &mut Env) -> SExp {
    match exp {
        SExp::CONS(val, _) => {
            match *val {
                SExp::CONS(car, _) => {
                    match *car {
                        SExp::ATOM(atom) => {
                            match atom {
                                Atom::NIL => println!("Nil"),
                                Atom::NUM(num) => println!("{}", num),
                                Atom::SYMBOL(name) => println!("{}", name), // maybe eval before printing
                            }
                        }
        
                        SExp::CONS(car, cdr) => {
                            lisp_print(evaluate(SExp::CONS(car, cdr), env), env);
                            // panic!("cannot print nested cons: {:?}", car)
                        }
                    }
                }
        
                SExp::ATOM(atom) => {
                    match atom {
                        Atom::NIL => println!("Nil"),
                        Atom::NUM(num) => println!("{}", num),
                        Atom::SYMBOL(name) => println!("{}", name), // maybe eval before printing
                    }
                }
            }
        }

        x => panic!("invalid s-exp in print: {:?}", x),
    }

    SExp::ATOM(Atom::NIL)
}

fn car(exp: SExp, _env: &mut Env) -> SExp {
    match exp {
        SExp::CONS(args, _) => {
            match *args {
                SExp::CONS(car, _) => *car,
                x => panic!("atom in car: {:?}", x),
            }
        }
        x => panic!("invalid param in car: {:?}", x),
    }
}

fn cdr(exp: SExp, _env: &mut Env) -> SExp {
    match exp {
        SExp::CONS(args, _) => {
            match *args {
                SExp::CONS(_, cdr) => *cdr,
                x => panic!("atom in cdr: {:?}", x),
            }
        }
        x => panic!("invalid param in car: {:?}", x),
    }
}

fn lisp_read() -> SExp {
    let mut buf = String::new();
    match std::io::stdin().read_line(&mut buf) {
        Ok(_) => SExp::ATOM(Atom::SYMBOL(buf[..buf.len() - 1].to_string())),
        Err(error) => panic!("encountered error in read: {}", error),
    }
}

fn to_num(sexp: SExp, _env: &mut Env) -> SExp {
    match sexp {
        SExp::CONS(real_sexp, _) => {
            match *real_sexp {
                SExp::ATOM(atom) => {
                    match atom {
                        Atom::SYMBOL(string_value) => {
                                match string_value.parse::<i64>() {
                                    Ok(value) => SExp::ATOM(Atom::NUM(value)),
                                    Err(_) => SExp::ATOM(Atom::NIL),
                                }
                            }
                        x => SExp::ATOM(x),
                    }
                }

                SExp::CONS(_, _) => SExp::ATOM(Atom::NIL),
            }
        }

        SExp::ATOM(atom) => {
            match atom {
                Atom::SYMBOL(string_value) => {
                        match string_value.parse::<i64>() {
                            Ok(value) => SExp::ATOM(Atom::NUM(value)),
                            Err(_) => SExp::ATOM(Atom::NIL),
                        }
                    }
                x => SExp::ATOM(x),
            }
        }
    }
}

fn tokenize(input: &str) -> Vec<LexerToken> {
    let mut lexer = Lexer::new(input);
    let mut tokens = Vec::<LexerToken>::new();

    loop {
        let lexer_result = lexer.yylex();

        match lexer_result {
            Ok(token) => tokens.push(token),
            Err(err) => {
                match err {
                    LexerError::EOF => break,
                    x => panic!("encountered error in tokenization: {:?}", x),
                }
            }
        }
    }

    tokens
} 

fn parse_wrapper(mut tokens: &mut Vec<LexerToken>) -> SExp {
    tokens.reverse();
    parse(&mut tokens)
}

fn parse(tokens: &mut Vec<LexerToken>) -> SExp {
    match tokens.pop() {
        Some(head) => {
            match head {
                LexerToken::LPAR => {
                    let car = parse(tokens);
                    let cdr = parse(tokens);
    
                    SExp::CONS(Box::new(car), Box::new(cdr))
                }

                LexerToken::RPAR => SExp::ATOM(Atom::NIL),

                LexerToken::ID(identifier) => {
                    let car = SExp::ATOM(Atom::SYMBOL(identifier));
                    SExp::CONS(Box::new(car), Box::new(parse(tokens)))
                }

                LexerToken::NUM(num) => {
                    let car = SExp::ATOM(Atom::NUM(num));
                    let cdr = parse(tokens);

                    SExp::CONS(Box::new(car), Box::new(cdr))
                }

                LexerToken::NIL => {
                    let car = SExp::ATOM(Atom::NIL);

                    SExp::CONS(Box::new(car), Box::new(parse(tokens)))
                }
                // LexerToken::LexerError(_) => Err(CompilationError("Invalid Token in parsing".to_string())),
            }
        }

        None => SExp::ATOM(Atom::NIL),
    }
}

fn read_from_string(sexp: SExp, _env: &mut Env) -> SExp {
    match sexp {
        SExp::CONS(car, _) => {
            if let SExp::ATOM(atom) = *car {
                if let Atom::SYMBOL(string) = atom {
                    let mut tokens = tokenize(&string);
                    return parse_wrapper(&mut tokens);
                }
            }

            panic!("invalid s-exp in read-from-string")
        }

        _ => panic!("invalid s-exp in read-from-string"),
    }
}

fn defun(exp: SExp, env: &mut Env) -> SExp {
    if let SExp::CONS(name_sexp, func_def) = exp {
        if let SExp::ATOM(name_atom) = *name_sexp {
            if let Atom::SYMBOL(name) = name_atom {
                env.insert(&name, *func_def, true);
                return SExp::ATOM(Atom::NIL);
            }
        }
    }

    panic!("invalid function decl in defun");
}

fn call_if(exp: SExp, env: &mut Env) -> SExp {
    if let SExp::CONS(cond, rest_if_else_body) = exp {
        if let SExp::CONS(if_body, else_body_cons) = *rest_if_else_body {
            if let SExp::CONS(else_body, _) = *else_body_cons {
                if evaluate(*cond, env) != SExp::ATOM(Atom::NIL) {
                    return evaluate(*if_body, env);
                } else {
                    return evaluate(*else_body, env);
                }
            }
        }
    }

    panic!("invalid if s-exp");
}

fn call_setq(exp: SExp, env: &mut Env) -> SExp {
    if let SExp::CONS(name_sexp, body_sexp) = exp {
        if let SExp::ATOM(name_atom) = *name_sexp {
            if let Atom::SYMBOL(name) = name_atom {
                let evaluated_value = evaluate(*body_sexp, env);

                env.insert(&name.to_string(), evaluated_value, false);
                return SExp::ATOM(Atom::NIL);
            }
        }
    }

    panic!("invalid s-exp in setq");
}

// fn call_while(exp: SExp, env: &mut Env) -> SExp {
//     if let SExp::CONS(cond, body) = exp {
//         let mut cond_copy = *cond.clone();
//         println!("cond: {:?}", cond_copy);

//         while evaluate(cond_copy, env) != SExp::ATOM(Atom::NIL) {
//             let body_copy = *body.clone();
//             cond_copy = *cond.clone();

//             evaluate(body_copy, env);
//         }
//     }

//     SExp::ATOM(Atom::NIL)
// }
