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

    #[regex("_?[a-zA-Z]([a-zA-Z0-9]|-)*|(\\+|\\-|\\\\|\\*)", token_identifier)]
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

// fn get_function_name(sexp: &SExp) -> String {
//     match sexp {
//         SExp::ATOM(atom) => {
//             match atom {
//                 Atom::SYMBOL(name) => name.to_string(),
//                 _ => panic!("invalid atom in function call"),
//             }
//         }

//         SExp::CONS(car, _) => get_function_name(&car.sexp),
//     }
// }

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
    // output.emit(&format!("let {} = env.get(&String::from(\"{}\")).unwrap().0;", var, symbol_name));
    var
}

fn compile_defun(name_sexp: &mut SExp, node: &mut Node, env: &mut Env, compiled_functions: &mut HashMap<String, u64>, output: &mut Output) {
    if let SExp::ATOM(atom) = name_sexp {
        if let Atom::SYMBOL(name) = atom {
            // "(env.insert(\"{}\", {}, true), name, to_runtime_sexp(node))"
            // insert function to env ? (at least uncompiled version so we can invoke it in interpreted code (eval an such))

            output.push_nesting();
            let name_index = *compiled_functions.get(name).unwrap_or(&0) + 1;
            compiled_functions.insert(name.to_string(), name_index);
            output.emit(&format!("fn {}_{} (sexp: SExp, mut env: &mut Env) -> SExp {{", name, compiled_functions.get(name).unwrap()));
            // compiled_functions.insert(name.to_string());

            match &mut node.sexp {
                SExp::CONS(params, body) => {
                    gen_bind_parameters(&params.sexp, output);
                    // output.emit(&format!("// compiling body: {:?}", body));

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
            return;
        }
    }

    panic!("invalid function name s-exp: {:?}", name_sexp);
}

// fn compile_add(node: &mut Node, env: &mut Env, compiled_functions: &mut HashMap<String, u64>, output: &mut Output) {
//     match &mut node.sexp {
//         SExp::CONS(first, rest) => {
//             compile_node(&mut *first, env, compiled_functions, output);
//             let val1 = &first.place;

//             match &mut rest.sexp {
//                 SExp::CONS(second, _) => {
//                     compile_node(&mut *second, env, compiled_functions, output);
//                     let val2 = &second.place;

//                     let new_var = gen_var();
//                     output.emit(&format!("let mut {} = add({}, {}, &mut env);", &new_var, val1, val2));
//                     node.place = new_var;
//                 }

//                 x => panic!("invalid right s-exp in add: {:?}", x),
//             }   
//         }

//         x => panic!("invalid left s-exp in add: {:?}", x),
//     }
// }

fn compile_function_call(name: &str, params: &mut Node, env: &mut Env, compiled_functions: &mut HashMap<String, u64>, output: &mut Output) -> String {
    compile_node(params, env, compiled_functions, output);

    match compiled_functions.get(name) {
        Some(_) => {}
        None => panic!("undefined function: {}", name),
    }

    let ret_var = gen_var();
    let tmp_var = gen_var();

    output.emit(&format!(
"let mut {1} = env.get(&String::from(\"{0}\")).unwrap();
let mut {2} = if {1}.1 {{
    match {1}.0 {{
        EnvElement::Func(func) => func({3}, &mut env),
        EnvElement::SExp(sexp) => call_function(sexp, {3}, &mut env),
    }}
}} else {{
    panic!(\"s-exp called is not a function\")
}};",
    name, tmp_var, ret_var, params.place));

    // output.emit(&format!("let mut {} = {}({}, &mut env);", ret_var, name, params.place));

    ret_var
}

fn compile_print(node: &mut Node, env: &mut Env, compiled_functions: &mut HashMap<String, u64>, output: &mut Output) -> String {
    compile_node(node, env, compiled_functions, output);

    let var = gen_var();
    output.emit(&format!("let mut {} = lisp_print({}, &mut env);", var, node.place));
    
    var
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
                                        SExp::CONS(defun_name_node, func_def) => {
                                            compile_defun(&mut defun_name_node.sexp, &mut *func_def, env, compiled_functions, output);

                                            node.place = String::from("SExp::ATOM(Atom::NIL)");
                                        }

                                        x => panic!("invalid s-exp in defun:{:?}", x),
                                    }
                                }

                                "print" => {
                                    let new_place = compile_print(cdr, env, compiled_functions, output);
                                    node.place = new_place;
                                }

                                // "+" => node.place = compile_function_call("add", cdr, env, compiled_functions, output),
                                // "-" => node.place = compile_function_call("sub", cdr, env, compiled_functions, output),
                                // "*" => node.place = compile_function_call("mul", cdr, env, compiled_functions, output),
                                // "/" => node.place = compile_function_call("div", cdr, env, compiled_functions, output),

                                function_name => {
                                    node.place = compile_symbol_lookup(&function_name, cdr, env, compiled_functions, output);

                                    // TODO: replace this with an emit of code that searches the env dynamically
                                    // let val = env.get(&String::from(function_name)).unwrap_or((SExp::ATOM(Atom::SYMBOL(function_name.to_string())), false));
                                    // if val.1 || compiled_functions.contains_key(function_name) { // if it's a function
                                    //     node.place = compile_function_call(&function_name, cdr, env, compiled_functions, output);
                                    // } else {
                                    //     compile_node(car, env, compiled_functions, output);
                                    //     compile_node(cdr, env, compiled_functions, output);
                                    //     node.place = format!("SExp::CONS(Box::new({}), Box::new({}))", car.place, cdr.place);
                                    // }
                                    // output.emit(&format!("// compiling args: {:?}", cdr));
                                    // compile_node(cdr, env, compiled_functions, output);

                                    // node.place = compile_function_call(&function_name, cdr, env, compiled_functions, output);
                                }
                            }
                        }

                        Atom::NUM(num) => {
                            let var = gen_var();
                            compile_node(cdr, env, compiled_functions, output);

                            output.emit(&format!("let mut {} = SExp::CONS(Box::new(SExp::ATOM(Atom::NUM({}))), Box::new({}));", var, num, cdr.place));
                            node.place = var;
                        }

                        Atom::NIL => {
                            let var = gen_var();
                            compile_node(cdr, env, compiled_functions, output);

                            output.emit(&format!("let mut {} = SExp::CONS(Box::new(SExp::ATOM(Atom::NIL))), Box::new({}));", var, cdr.place));
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
                    output.emit(&format!("let mut {} = SExp::CONS(Box::new({}), Box::new({}));", var, compiled_car, compiled_cdr));
                    node.place = var;
                }
            }
        }

        SExp::ATOM(atom) => {
            match atom {
                Atom::NUM(num) => {
                    node.place = format!("SExp::ATOM(Atom::NUM({}))", num);
                    // node.place = num.to_string();
                }
    
                Atom::SYMBOL(symbol) => {
                    let new_place = gen_load_sexp(symbol, output);

                    node.place = new_place;
                }
    
                Atom::NIL => {
                    node.place = format!("SExp::ATOM(Atom::NIL)");
                }
            }
        }
    }
}

// fn compile_node(node: &mut Node, env: &mut Env, compiled_functions: &mut HashSet<String>, output: &mut Output) {
//     output.emit(&format!("// compiling node: {:?}", node));
//     match &mut node.sexp {
//         SExp::CONS(car, cdr) => {
//             match &mut car.sexp {
//                 SExp::CONS(_, _) => {
//                     output.emit(&format!("// nested cons: {:?}", car));
//                     compile_node(car, env, compiled_functions, output);
//                     node.place = car.place.to_string();
//                 }

//                 SExp::ATOM(_) => {
//                     let func_name = get_function_name(&car.sexp);
            
//                     match func_name.as_str() {
//                         "defun" => {
//                             match &mut cdr.sexp {
//                                 SExp::CONS(defun_name_node, func_def) => {
//                                     compile_defun(&mut defun_name_node.sexp, func_def, env, compiled_functions, output)
//                                 }

//                                 _ => panic!("TODO: this"),
//                             }
//                         }

//                         "+" => {
//                             compile_add(&mut *cdr, env, compiled_functions, output);
//                             node.place = cdr.place.to_string();
//                         }
//                         function_name => {
//                             println!("// function name: {}", function_name);
//                             node.place = compile_function_call(&func_name, &cdr, env, compiled_functions, output);
//                             // node.place = cdr.place.to_string();
//                         }
//                     }
//                 }
//             }

//             output.emit(&format!("attempted to compile {:?}", cdr));
//             // compile_node(cdr, env, compiled_functions, output);
            
            
//         }

//         SExp::ATOM(atom) => {
//             match atom {
//                 Atom::NUM(num) => {
//                     node.place = format!("SExp::ATOM(Atom::NUM({}))", num);
//                     // node.place = num.to_string();
//                 }
    
//                 Atom::SYMBOL(symbol) => {
//                     let new_place = gen_load_sexp(symbol, output);

//                     node.place = new_place;
//                 }
    
//                 Atom::NIL => {
//                     node.place = format!("SExp::ATOM(Atom::NIL)");
//                 }
//             }
//         }
//     }
// }

fn compile_predifined(predefined_code: &str, output: &mut Output) {
    output.emit(predefined_code);
}

fn compile(filename: &str) {
    let code = load_file(filename);

    let mut output = Output::new();

    let predefined_code = load_file("builtins.rs");
    compile_predifined(&predefined_code, &mut output);

    let mut compiled_functions = HashMap::<String, u64>::new();
    let predefined_functions = ["add", "sub", "mul", "div"];
    let predefined_aliases = [("+", "add"), ("-", "sub"), ("*", "mul"), ("/", "div")];

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
        // match &mut node.sexp {
        //     SExp::CONS(real_node, next_node) => compile_node(real_node, &mut env, &mut compiled_functions, &mut output),
        //     SExp::ATOM(_) => {
        //         // compile_node(&mut node, &mut env, &mut compiled_functions), // not sure this is valid
        //         panic!("invalid node in compile");
        //     }
        // }
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
