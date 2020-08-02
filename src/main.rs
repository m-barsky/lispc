use logos::{Logos, Lexer};
use std::error::Error;
use std::collections::HashMap;

struct Output {
    buffers: Vec<String>,
    nesting_level: usize,
}

impl Output {
    pub fn new() -> Self {
        Self {
            buffers: vec![String::new()],
            nesting_level: 0usize,
        }
    }

    pub fn emit(&mut self, code: &str) {
        self.buffers[self.nesting_level] += format!("{}\n", code).as_str();
    }

    pub fn push_nesting(&mut self) {
        self.nesting_level += 1;
        if self.buffers.len() + 1usize >= self.nesting_level {
            self.buffers.push(String::new());
        }
    }

    pub fn pop_nesting(&mut self) {
        self.nesting_level -= 1;
    }
}

#[derive(Debug)]
struct CompilationError(String);

impl std::fmt::Display for CompilationError {
    fn fmt (&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "A compilation error has occured: {}", self.0)
    }
} 

impl Error for CompilationError {}

struct Env {
    map: HashMap<String, Vec<(SExp, bool)>>
}

impl Env {
    pub fn new() -> Self {
        Self {
            map: HashMap::<String, Vec<(SExp, bool)>>::new()
        }
    }

    pub fn insert(&mut self, key: &String, value: SExp, is_func: bool) {
        match self.map.get_mut(key) {
            Some(vector) => {
                vector.push((value, is_func));
            }

            None => {
                let vector = vec![(value, is_func)];
                let key_copy = (*key).clone();
                self.map.insert(key_copy, vector);
            }
        }
    }

    pub fn get(&mut self, key: &String) -> Option<(SExp, bool)> {
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

#[derive(Clone, Debug)]
enum Atom {
    SYMBOL(String),
    NUM(i64),
    NIL, 
}

#[derive(Clone, Debug)]
struct Node {
    sexp: SExp,
    place: String,
}

#[derive(Clone, Debug)]
enum SExp {
    ATOM(Atom),
    CONS(Box<Node>, Box<Node>)
}

fn parse_wrapper(mut tokens: &mut Vec<LexerToken>) -> Result<Node, CompilationError> {
    tokens.reverse();
    parse(&mut tokens)
}

fn parse(tokens: &mut Vec<LexerToken>) -> Result<Node, CompilationError> {
    match tokens.pop() {
        Some(head) => {
            match head {
                LexerToken::LPAR => {
                    let car = parse(tokens)?;
                    let cdr = parse(tokens)?;
    
                    let ret = Node {
                        sexp: SExp::CONS(Box::new(car), Box::new(cdr)),
                        place: String::new(),
                    };

                    Ok(ret)
                }

                LexerToken::RPAR => Ok(Node {sexp: SExp::ATOM(Atom::NIL), place: String::new()}),

                LexerToken::ID(identifier) => {
                    let car = Node {
                        sexp: SExp::ATOM(Atom::SYMBOL(identifier)),
                        place: String::new(),
                    };

                    let ret = Node {
                        sexp: SExp::CONS(Box::new(car), Box::new(parse(tokens)?)),
                        place: String::new(),
                    };

                    Ok(ret)
                }

                LexerToken::NUM(num) => {
                    let car = Node {
                        sexp: SExp::ATOM(Atom::NUM(num)),
                        place: String::new(),
                    };
                    let cdr = parse(tokens)?;

                    let ret = Node {
                        sexp: SExp::CONS(Box::new(car), Box::new(cdr)),
                        place: String::new(),
                    };

                    Ok(ret)
                }

                LexerToken::NIL => {
                    let car = Node {
                        sexp: SExp::ATOM(Atom::NIL),
                        place: String::new(),
                    };

                    let ret = Node {
                        sexp: SExp::CONS(Box::new(car), Box::new(parse(tokens)?)),
                        place: String::new(),
                    };

                    Ok(ret)
                }
                LexerToken::Err => Err(CompilationError("Invalid Token in parsing".to_string())),
            }
        }

        None => Ok(Node {sexp: SExp::ATOM(Atom::NIL), place: String::new()}),
    }
}

fn gen_var() -> String {
    static mut CURR_VAR: u64 = 0;
    unsafe {
        let var = format!("var_{}", CURR_VAR);
        CURR_VAR += 1;
        var
    }
}

fn token_identifier(lex: &mut Lexer<LexerToken>) -> String {
    lex.slice().to_string()
}

fn token_num(lex: &mut Lexer<LexerToken>) -> i64 {
    lex.slice().parse::<i64>().unwrap()
}

fn tokenize(line: &str) -> Vec<LexerToken> {
    let mut lex = LexerToken::lexer(line);

    let mut tokens = Vec::<LexerToken>::new();

    while let Some(token) = lex.next() {
        tokens.push(token);
    }

    return tokens;
}

#[derive(Logos, Debug, PartialEq, Clone)]
enum LexerToken {
    #[token("(")]
    LPAR,

    #[token(")")]
    RPAR,

    #[regex("_*[a-zA-Z](-*[a-zA-Z0-9_])*|(\\+|\\-|/|\\*|=)", token_identifier)]
    ID(String),

    #[regex("[0-9]+", token_num)]
    NUM(i64),

    #[token("Nil")]
    NIL,

    #[error]
    #[regex("[ \n\r\t]+", logos::skip)]
    Err,
}

fn load_file(filename: &str) -> String {
    use std::fs::File;
    use std::io::prelude::*;

    let mut file = match File::open(filename) {
        Ok(x) => x,
        Err(err) => panic!("could not open {}: {}", filename, err),
    };

    let mut code = String::new();

    file.read_to_string(&mut code).unwrap();

    code
}

// generates code to remove the bindings inserted in beginning of the function
fn gen_remove_parameter_bindings(params: &SExp, output: &mut Output) {
    if let SExp::CONS(car, cdr) = params {
        if let SExp::ATOM(atom) = &car.sexp {
            if let Atom::SYMBOL(name) = atom {
                output.emit(&format!("env.pop(&String::from(\"{}\"));", name));
                gen_remove_parameter_bindings(&cdr.sexp, output);
            }
        }
    } 
}

// generates the binding for the rest of the parameters (see gen_bind_parameters)
fn gen_bind_parameters_inner(params: &SExp, param_name: &str, output: &mut Output) {
    if let SExp::CONS(first_param, rest_params) = params {
        if let SExp::ATOM(atom) = &first_param.sexp {
            if let Atom::SYMBOL(name) = atom {
                let new_car = gen_var();
                let new_cdr = gen_var();
                output.emit(&format!(
"if let SExp::CONS({}, {}) = *{} {{
    env.insert(&\"{}\".to_string(), *{}, false);",
                    new_car, new_cdr, param_name, name, new_car));

                gen_bind_parameters_inner(&rest_params.sexp, &new_cdr, output);

                output.emit("}");
            }
        }
    }
}

// generates the binding for the first parameter
fn gen_bind_parameters(params: &SExp, output: &mut Output) {
    if let SExp::CONS(first_param, rest_params) = params {
        if let SExp::ATOM(atom) = &first_param.sexp {
            if let Atom::SYMBOL(name) = atom {
                let new_car = gen_var();
                let new_cdr = gen_var();
                output.emit(&format!(
"if let SExp::CONS({}, {}) = {} {{
    env.insert(&\"{}\".to_string(), *{}, false);",
                    new_car, new_cdr, "sexp", name, new_car));

                gen_bind_parameters_inner(&rest_params.sexp, &new_cdr, output);

                output.emit("}");
            }
        }
    }
}

fn gen_load_sexp(symbol_name: &str, output: &mut Output) -> String {
    let var = gen_var();
    output.emit(&format!(
"let {} = match env.get(&String::from(\"{}\")).unwrap().0 {{
    EnvElement::SExp(sexp) => sexp,
    _ => panic!(\"could not load function as SExp\"),
}};",  
    var, symbol_name));

    var
}

fn compile_defun(name_sexp: &mut SExp, node: &mut Node, env: &mut Env, compiled_functions: &mut HashMap<String, u64>, output: &mut Output) -> String {
    if let SExp::ATOM(atom) = name_sexp {
        if let Atom::SYMBOL(name) = atom {
            output.push_nesting();
            let name_index = *compiled_functions.get(name).unwrap_or(&0) + 1;
            compiled_functions.insert(name.to_string(), name_index);
            output.emit(&format!("fn {}_{} (sexp: SExp, mut env: &mut Env) -> SExp {{", name, compiled_functions.get(name).unwrap()));

            match &mut node.sexp {
                SExp::CONS(params, body) => {
                    gen_bind_parameters(&params.sexp, output);

                    if let SExp::CONS(real_body, _) = &mut body.sexp {
                        compile_node(real_body, env, compiled_functions, output);

                        gen_remove_parameter_bindings(&params.sexp, output);
                        output.emit(&format!("return {};", real_body.place));
                    } else {
                        panic!("could not compile function body: {:?}", body);
                    }
                }

                x => panic!("invalid function body: {:?}", x),
            }

            output.emit("}");
            output.pop_nesting();

            // now add the function to the environment
            output.emit(&format!("env.insert_compiled_func(&String::from(\"{0}\"), {0}_{1});", name, name_index));
            return "SExp::ATOM(Atom::NIL)".to_string();
        }
    }

    panic!("invalid function name s-exp: {:?}", name_sexp);
}

fn compile_symbol_lookup(name: &str, cdr: &mut Node, env: &mut Env, compiled_functions: &mut HashMap<String, u64>, output: &mut Output) -> String{
    compile_node(cdr, env, compiled_functions, output);

    let tmp_var = gen_var();
    let ret_var = gen_var();
    output.emit(&format!(
"let {0} = env.get(&String::from(\"{2}\")).unwrap();
let {1} = if {0}.1 {{
    match {0}.0 {{
        EnvElement::SExp(sexp) => call_function(sexp, {3}, &mut env),
        EnvElement::Func(func) => func({3}, &mut env),
    }}
}} else {{
    match {0}.0 {{
        EnvElement::SExp(sexp) => SExp::CONS(Box::new(sexp), Box::new({3})),
        _ => panic!(\"could not lookup symbol\"),
    }}
}};",
    tmp_var, ret_var, name, cdr.place));

    ret_var
}

fn compile_quoted_node(sexp: &mut SExp, output: &mut Output) -> String {
    let ret_var = gen_var();

    match sexp {
        SExp::CONS(car, cdr) => {
            let car_place = compile_quoted_node(&mut car.sexp, output);
            let cdr_place = compile_quoted_node(&mut cdr.sexp, output);

            // not sure this is correct
            output.emit(&format!("let {} = SExp::CONS(Box::new({}), Box::new({}));", ret_var, car_place, cdr_place));
        }

        SExp::ATOM(atom) => {
            match atom {
                Atom::NUM(num) => output.emit(&format!("let {} = SExp::ATOM(Atom::NUM({}));", ret_var, num)),
                Atom::SYMBOL(symbol) => output.emit(&format!("let {} = SExp::ATOM(Atom::SYMBOL(String::from(\"{}\")));", ret_var, symbol)),
                Atom::NIL => output.emit(&format!("let {} = SExp::ATOM(Atom::NIL);", ret_var)),
            }
        }
    }

    ret_var
}

fn compile_setq(node: &mut Node, env: &mut Env, compiled_functions: &mut HashMap<String, u64>, output: &mut Output) -> String {
    match &mut node.sexp {
        SExp::CONS(name_node, cons_node) => {
            match &mut cons_node.sexp {
                SExp::CONS(real_value, _) => {

                    compile_node(real_value, env, compiled_functions, output);

                    if let SExp::ATOM(atom) = &mut name_node.sexp {
                        if let Atom::SYMBOL(name) = atom {

                            // what about setq'ing a function?
                            output.emit(&format!("env.insert(&String::from(\"{}\"), {}, false);", name, real_value.place));

                            return String::from("SExp::ATOM(Atom::NIL)");
                        } else {
                            panic!("invalid value in setq");
                        }
                    } else {
                        panic!("invalid value in setq");
                    }

                }

                _ => panic!("invalid value in setq"),
            }
        }

        _ => panic!("invalid value in setq"),
    }
}

fn compile_if(node: &mut Node, env: &mut Env, compiled_functions: &mut HashMap<String, u64>, output: &mut Output) -> String {
    if let SExp::CONS(cond, body_cons) = &mut node.sexp {
        if let SExp::CONS(body, else_body_cons) = &mut body_cons.sexp {
            if let SExp::CONS(else_body, _) = &mut else_body_cons.sexp {
                let ret_var = gen_var();
                compile_node(cond, env, compiled_functions, output);
        
                output.emit(&format!("let {} = if {} != SExp::ATOM(Atom::NIL) {{", ret_var, cond.place));
                compile_node(body, env, compiled_functions, output);
                output.emit(&format!(
"{}
}} else {{",
                body.place));

                compile_node(else_body, env, compiled_functions, output);

                output.emit(&format!(
"{}
}};",
                else_body.place));
        
                return ret_var;
            }
        }
    }

    panic!("invalid s-exp in if");
}

fn compile_read(output: &mut Output) -> String {
    let ret_var = gen_var();
    output.emit(&format!("let {} = lisp_read();", ret_var));

    ret_var
}

fn compile_node(node: &mut Node, env: &mut Env, compiled_functions: &mut HashMap<String, u64>, output: &mut Output) {
    output.emit(&format!("// compiling node: {:?}", node));
    match &mut node.sexp {
        SExp::CONS(car, cdr) => {
            match &mut car.sexp {
                SExp::ATOM(atom) => {
                    match atom {
                        Atom::SYMBOL(name) => {
                            match name.as_str() {
                                "defun" => {
                                    match &mut cdr.sexp {
                                        SExp::CONS(defun_name_node, func_def) => node.place = compile_defun(&mut defun_name_node.sexp, &mut *func_def, env, compiled_functions, output),
                                        x => panic!("invalid s-exp in defun:{:?}", x),
                                    }
                                }

                                "read" => node.place = compile_read(output),
                                "quote" => node.place = compile_quoted_node(&mut cdr.sexp, output),
                                "setq" => node.place = compile_setq(cdr, env, compiled_functions, output),
                                "if" => node.place = compile_if(cdr, env, compiled_functions, output),
                                function_name => node.place = compile_symbol_lookup(&function_name, cdr, env, compiled_functions, output),
                            }
                        }

                        Atom::NUM(num) => {
                            let var = gen_var();
                            compile_node(cdr, env, compiled_functions, output);

                            output.emit(&format!("let {} = SExp::CONS(Box::new(SExp::ATOM(Atom::NUM({}))), Box::new({}));", var, num, cdr.place));
                            node.place = var;
                        }

                        Atom::NIL => {
                            let var = gen_var();
                            compile_node(cdr, env, compiled_functions, output);

                            output.emit(&format!("let {} = SExp::CONS(Box::new(SExp::ATOM(Atom::NIL)), Box::new({}));", var, cdr.place));
                            node.place = var;
                        }
                    }
                }

                _ => {
                    compile_node(car, env, compiled_functions, output);
                    let compiled_car = car.place.to_string();
                    // what about cdr?
                    compile_node(cdr, env, compiled_functions, output);
                    let compiled_cdr = cdr.place.to_string();

                    let var = gen_var();
                    output.emit(&format!("let {} = SExp::CONS(Box::new({}), Box::new({}));", var, compiled_car, compiled_cdr));
                    node.place = var;
                }
            }
        }

        SExp::ATOM(atom) => {
            match atom {
                Atom::NUM(num) => node.place = format!("SExp::ATOM(Atom::NUM({}))", num),
                Atom::SYMBOL(symbol) => node.place = gen_load_sexp(symbol, output),
                Atom::NIL => node.place = format!("SExp::ATOM(Atom::NIL)"),
            }
        }
    }
}

fn compile_predifined(predefined_code: &str, output: &mut Output) {
    output.emit(predefined_code);
}

fn compile(filename: &str) {
    let code = load_file(filename);

    let mut output = Output::new();

    let predefined_code = load_file("builtins.rs");
    compile_predifined(&predefined_code, &mut output);

    let lexer_code = load_file("lisp_lex.rs");
    compile_predifined(&lexer_code, &mut output);

    let mut compiled_functions = HashMap::<String, u64>::new();
    let predefined_functions = ["add", "sub", "mul", "div", "eq", "car", "cdr", "defun"];
    let predefined_aliases = [("+", "add"), ("-", "sub"), ("*", "mul"), ("/", "div"), ("print", "lisp_print"), ("=", "eq"), 
        ("read-from-string", "read_from_string"), ("to-num", "to_num"), ("eval", "evaluate"), ("if", "call_if"),
        ("setq", "call_setq")];

    for func in &predefined_functions {
        compiled_functions.insert(func.to_string(), 0);
    }

    let mut env = Env::new();

    let mut tokens = tokenize(&code);
    let mut asts: Vec<Node> = Vec::new();
    
    while tokens.len() > 0 {
        let node = parse_wrapper(&mut tokens).unwrap();
        println!("// parsed s-exp: \n// {:?}\n", node);
        asts.push(node);
    }

    output.emit(&format!("// ast len: {}", asts.len()));
    output.emit("fn main() {");
    output.emit("let mut env = Env::new();");

    for func in &predefined_functions {
        output.emit(&format!("env.insert_compiled_func(&String::from(\"{0}\"), {0});", func));
    }

    for (alias, aliased_name) in &predefined_aliases {
        output.emit(&format!("env.insert_compiled_func(&String::from(\"{0}\"), {1});", alias, aliased_name));
    }
    
    for mut node in asts {
        compile_node(&mut node, &mut env, &mut compiled_functions, &mut output);
    }

    output.emit("}");

    // TODO: change to files
    for buff in output.buffers.iter().rev() {
        println!("{}", buff);
    }
}

fn main() {
    use std::env;

    let arg = env::args().last();
    match arg {
        Some(filename) =>  compile(&filename),
        None => panic!("No file specified!"),
    }
}
