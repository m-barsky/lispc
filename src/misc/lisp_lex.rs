#[derive(Debug)]
pub enum LexerToken {
    LPAR,
    RPAR,
    ID(String),
    NUM(i64),
    NIL,
}

// following code has been generated using rflex (and edited manually)

#[derive(Debug)]
pub enum LexerError {
    EOF,
    Unmatch,
}

pub struct Lexer<'a> {
    cmap: Vec<usize>,
    cmap2: HashMap<usize, usize>,
    start: std::str::Chars<'a>,
    current: std::str::Chars<'a>,
    max_len: usize,


    zz_state: usize,
    zz_lexical_state: usize,
    zz_marked_pos: usize,
    zz_current_pos: usize,
    zz_start_read: usize,

    zz_at_eof: bool,

}

impl<'a> Lexer<'a> {
    pub const ZZ_ROW: [usize; 14] = [0, 13, 13, 13, 26, 39, 52, 13, 65, 13, 78, 39, 52, 39];
    pub const ZZ_TRANS: [i32; 91] = [1, 2, 3, 4, 5, 5, 6, 5, 7, 8, 7, 9, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 5, 10, 5, 5, 5, 11, 5, -1, -1, -1, -1, -1, -1, 5, 5, 5, 5, 5, 11, 5, -1, -1, -1, -1, -1, -1, 5, 5, 5, 12, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 8, -1, -1, -1, -1, -1, -1, 5, 5, 13, 5, 5, 11, 5, -1, -1, -1];
    pub const ZZ_ATTR: [i32; 14] = [0, 9, 9, 9, 1, 1, 1, 9, 1, 9, 1, 0, 0, 1];
    pub const ZZ_ACTION: [i32; 14] = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 0, 0, 11];
    pub const ZZ_LEXSTATE: [i32; 2] = [0, 0];
    pub const YYINITIAL: usize = 0;


    pub const YYEOF: i32 = -1;

    pub fn new(input: &'a str) -> Lexer<'a> {
        let max_len = input.chars().clone().count();
        let chars = input.chars();
        let mut cmap: Vec<usize> = Vec::with_capacity(256);
        cmap.resize(256, 0);
        let mut cmap2: HashMap<usize, usize> = HashMap::new();
        cmap[10] = 12;
        cmap[11] = 12;
        cmap[12] = 12;
        cmap[13] = 11;
        cmap[32] = 11;
        cmap[40] = 2;
        cmap[41] = 1;
        cmap[42] = 10;
        cmap[43] = 10;
        cmap[45] = 8;
        cmap[47] = 10;
        cmap[48] = 9;
        cmap[49] = 9;
        cmap[50] = 9;
        cmap[51] = 9;
        cmap[52] = 9;
        cmap[53] = 9;
        cmap[54] = 9;
        cmap[55] = 9;
        cmap[56] = 9;
        cmap[57] = 9;
        cmap[61] = 10;
        cmap[65] = 7;
        cmap[66] = 7;
        cmap[67] = 7;
        cmap[68] = 7;
        cmap[69] = 7;
        cmap[70] = 7;
        cmap[71] = 7;
        cmap[72] = 7;
        cmap[73] = 7;
        cmap[74] = 7;
        cmap[75] = 7;
        cmap[76] = 7;
        cmap[77] = 7;
        cmap[78] = 3;
        cmap[79] = 7;
        cmap[80] = 7;
        cmap[81] = 7;
        cmap[82] = 7;
        cmap[83] = 7;
        cmap[84] = 7;
        cmap[85] = 7;
        cmap[86] = 7;
        cmap[87] = 7;
        cmap[88] = 7;
        cmap[89] = 7;
        cmap[90] = 7;
        cmap[92] = 11;
        cmap[95] = 6;
        cmap[97] = 7;
        cmap[98] = 7;
        cmap[99] = 7;
        cmap[100] = 7;
        cmap[101] = 7;
        cmap[102] = 7;
        cmap[103] = 7;
        cmap[104] = 7;
        cmap[105] = 4;
        cmap[106] = 7;
        cmap[107] = 7;
        cmap[108] = 5;
        cmap[109] = 7;
        cmap[110] = 7;
        cmap[111] = 7;
        cmap[112] = 7;
        cmap[113] = 7;
        cmap[114] = 7;
        cmap[115] = 7;
        cmap[116] = 7;
        cmap[117] = 7;
        cmap[118] = 7;
        cmap[119] = 7;
        cmap[120] = 7;
        cmap[121] = 7;
        cmap[122] = 7;
        cmap[133] = 12;
        cmap2.insert(8232, 12);
        cmap2.insert(8233, 12);


        Lexer {
            cmap,
            cmap2,
            start: chars.clone(),
            current: chars.clone(),

            max_len,
            zz_state: 0,
            zz_lexical_state: Lexer::YYINITIAL,
            zz_marked_pos: 0,
            zz_current_pos: 0,
            zz_start_read: 0,

            zz_at_eof: false,

        }
    }


    pub fn is_eof(&self) -> bool {
        self.zz_at_eof
    }

    pub fn yybegin(&mut self, new_state: usize) {
        self.zz_lexical_state = new_state;
    }

    pub fn yystate(&self) -> usize {
        self.zz_lexical_state
    }

    pub fn yylength(&self) -> usize {
        self.zz_marked_pos - self.zz_start_read
    }

    pub fn yycharat(&self, pos: usize) -> Option<char> {
        self.start.clone().nth(pos)
    }

    pub fn yytext(&self) -> String {
        let len = self.zz_marked_pos - self.zz_start_read;
        let mut text = String::with_capacity(len);
        let mut chars = self.start.clone();

        for _ in 0..len {
            text.push(match chars.next() { Some(c) => c, _ => break,});
        }
        text
    }

    pub fn yypushback(&mut self, num: usize) {
        if num <= self.yylength() {
            self.zz_marked_pos -= num;
        }
    }

    pub fn yylex(&mut self) -> Result<LexerToken, LexerError> {
        let mut zz_input: i32;

        // cached
        loop {
            let mut zz_marked_pos_l = self.zz_marked_pos;
            let mut zz_action = -1;
            let mut zz_current_pos_l = self.zz_marked_pos;
            let mut current = self.current.clone();
            

            self.zz_start_read = self.zz_marked_pos;
            self.zz_current_pos = self.zz_marked_pos;
            self.start = self.current.clone();

            self.zz_state = Lexer::ZZ_LEXSTATE[self.zz_lexical_state] as usize;

            // set up zz_action for empty match case:
            let zz_attributes = Lexer::ZZ_ATTR[self.zz_state];
            if (zz_attributes & 1) == 1 {
                zz_action = self.zz_state as i32;
            }

            'zz_for_action: loop {
                if zz_current_pos_l < self.max_len {
                    zz_input = current.next().unwrap() as i32;
                    zz_current_pos_l += 1;
                } else if self.zz_at_eof {
                    zz_input = Lexer::YYEOF;
                    break 'zz_for_action;
                } else {
                    self.zz_current_pos = zz_current_pos_l;

                    if self.max_len <= zz_current_pos_l {
                        zz_input = Lexer::YYEOF;
                        break 'zz_for_action;
                    } else {
                        zz_input = current.next().unwrap() as i32;
                        zz_current_pos_l += 1;
                    }
                }

                let cidx = if zz_input <= 0xFF {
                    self.cmap[zz_input as usize]
                } else {
                    *self.cmap2.get(&(zz_input as usize)).unwrap_or(&0usize)
                };
                let idx = Lexer::ZZ_ROW[self.zz_state] + cidx;
                let zz_next = Lexer::ZZ_TRANS[idx];
                if zz_next == -1 {
                    break 'zz_for_action;
                }
                self.zz_state = zz_next as usize;

                let zz_attributes = Lexer::ZZ_ATTR[self.zz_state];
                if (zz_attributes & 1) == 1 {
                    zz_action = self.zz_state as i32;
                    zz_marked_pos_l = zz_current_pos_l;
                    self.current = current.clone();
                    if (zz_attributes & 8) == 8 {
                        break 'zz_for_action;
                    }
                }
            }   // loop 'zz_for_action

            // store back cached position
            self.zz_marked_pos = zz_marked_pos_l;

            if zz_input == Lexer::YYEOF && self.zz_start_read == self.zz_current_pos {
                self.zz_at_eof = true;

                return Err(LexerError::EOF);
            } else {
                let action = if zz_action < 0 {
                    zz_action
                } else {
                    Lexer::ZZ_ACTION[zz_action as usize]
                };
                match action {
                    1 => { return Err(LexerError::Unmatch); }
                    12 => { /* nothing */ }
                    2 => { return Ok(LexerToken::RPAR); }
                    13 => { /* nothing */ }
                    3 => { return Ok(LexerToken::LPAR); }
                    14 => { /* nothing */ }
                    4 => { return Ok(LexerToken::ID(self.yytext())); }
                    15 => { /* nothing */ }
                    5 => { return Ok(LexerToken::ID(self.yytext())); }
                    16 => { /* nothing */ }
                    6 => { return Err(LexerError::Unmatch); }
                    17 => { /* nothing */ }
                    7 => { return Ok(LexerToken::ID(self.yytext())); }
                    18 => { /* nothing */ }
                    8 => { return Ok(LexerToken::NUM(self.yytext().parse::<i64>().unwrap())); }
                    19 => { /* nothing */ }
                    9 => { /* do nothing on whitespace */ }
                    20 => { /* nothing */ }
                    10 => { return Ok(LexerToken::ID(self.yytext())); }
                    21 => { /* nothing */ }
                    11 => { return Ok(LexerToken::NIL); }
                    22 => { /* nothing */ }

                    _ => {
                        return Err(LexerError::Unmatch);
                    }
                }
            }
        }   // loop
        // never reach end of function
    }

}
