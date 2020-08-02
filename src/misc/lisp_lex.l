#[derive(Debug)]
pub enum LexerToken {
    LPAR,
    RPAR,
    ID(String),
    NUM(i64),
    NIL,
}

fn main() {
    use std::io;
    let mut buf = String::new();
    std::io::stdin().read_line(&mut buf).unwrap();

    let mut lexer = Lexer::new(&buf);

    while let Ok(val) = lexer.yylex() {
        println!("token: {:?}", val);
    }
     
}

// following code has been generated using rflex (and edited manually)

%%
%class Lexer
%result_type LexerToken
"\)"                                                return Ok(LexerToken::RPAR);
"\("  	                                            return Ok(LexerToken::LPAR);
Nil                                                 return Ok(LexerToken::NIL);
(_*[a-zA-Z](-*[a-zA-Z0-9_])*)|[-+/*=]               return Ok(LexerToken::ID(self.yytext()));
[0-9]+                                              return Ok(LexerToken::NUM(self.yytext().parse::<i64>().unwrap()));
[\t\r\n ]                                           /* do nothing on whitespace */
.                                                   return Err(Error::Unmatch);
%%
