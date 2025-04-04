// We are finally making some progress
use super::*;
use std::collections::HashMap;

pub type Result<T, E = ErrorMessage> = std::result::Result<T, E>;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum StringType {
    SingleQuoted,
    DoubleQuoted,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StringToken {
    pub raw: String,
    pub contents: String,
    pub typ: StringType,
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    Ident {
        raw: String,
        name: String,
        is_raw: bool,
    },
    Punctuation {
        raw: String,
    },
    String(StringToken),
    Number {
        raw: String,
    },
    Comment {
        raw: String,
        contents: String,
    },
    Eof,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.raw())
    }
}

impl Token {
    pub fn into_raw(self) -> String {
        match self {
            Token::Ident { raw, .. } => raw,
            Token::Punctuation { raw } => raw,
            Token::String(StringToken { raw, .. }) => raw,
            Token::Number { raw } => raw,
            Token::Comment { raw, .. } => raw,
            Token::Eof => "<EOF>".to_owned(),
        }
    }
    pub fn raw(&self) -> &str {
        match self {
            Token::Ident { raw, .. } => raw,
            Token::Punctuation { raw } => raw,
            Token::String(StringToken { raw, .. }) => raw,
            Token::Number { raw } => raw,
            Token::Comment { raw, .. } => raw,
            Token::Eof => "<EOF>",
        }
    }
    pub fn is_eof(&self) -> bool {
        matches!(self, Self::Eof)
    }
    pub fn is_comment(&self) -> bool {
        matches!(self, Self::Comment { .. })
    }
}

struct Lexer {
    // source: SourceFile,
    reader: peek2::Reader<char>,
    next_recording_id: u64,
    recordings: HashMap<u64, String>,
}

impl Lexer {
    fn next_token(&mut self) -> Result<SpannedToken, Error> {
        match self.next_token_impl() {
            Ok(result) => Ok(result),
            Err(message) => {
                let start = self.reader.position();
                Err(message.at(Span {
                    filename: self.reader.filename().to_owned(),
                    start,
                    end: match self.reader.peek() {
                        Some('\n') => Position {
                            index: start.index + 1,
                            line: start.line + 1,
                            column: 1,
                        },
                        Some(_) => Position {
                            index: start.index + 1,
                            line: start.line,
                            column: start.column + 1,
                        },
                        None => start,
                    },
                }))
            }
        }
    }
    fn next_token_impl(&mut self) -> Result<SpannedToken> {
        self.skip_whitespace();
        let start = self.reader.position();
        let token = [
            Self::read_simple_comment,
            Self::read_long_comment,
            Self::read_string,
            Self::read_ident,
            Self::read_number,
            Self::read_punctuation,
        ]
        .into_iter()
        .find_map(|f| f(self).transpose())
        .transpose()?;
        let token = match token {
            None => {
                if let Some(c) = self.reader.peek() {
                    return error!("Unexpected char {c:?}");
                }
                Token::Eof
            }
            Some(token) => token,
        };
        let end = self.reader.position();
        Ok(SpannedToken {
            token,
            span: Span {
                start,
                end,
                filename: self.reader.filename().to_owned(),
            },
        })
    }
    fn skip_whitespace(&mut self) {
        while self.reader.peek().map_or(false, |c| c.is_whitespace()) {
            self.next().unwrap();
        }
    }
    fn skip_char(&mut self, expected: char) -> Result<()> {
        match self.reader.peek() {
            None => error!("expected {expected:?}, got EOF"),
            Some(&actual) if actual == expected => {
                self.next().unwrap();
                Ok(())
            }
            Some(&actual) => error!("expected {expected:?}, got {actual:?}"),
        }
    }

    fn read_while(&mut self, mut f: impl FnMut(char) -> bool) -> Result<String> {
        let mut result = String::new();
        while let Some(&c) = self.reader.peek() {
            if f(c) {
                result.push(c);
                self.next().unwrap();
            } else {
                break;
            }
        }
        Ok(result)
    }
}

struct RecordingToken(u64);

impl Lexer {
    fn start_recording(&mut self) -> RecordingToken {
        let id = self.next_recording_id;
        self.next_recording_id += 1;
        self.recordings.insert(id, String::new());
        RecordingToken(id)
    }
    fn stop_recording(&mut self, token: RecordingToken) -> String {
        self.recordings.remove(&token.0).unwrap()
    }
    fn next(&mut self) -> Option<char> {
        let next = self.reader.next();
        if let Some(c) = next {
            for recording in self.recordings.values_mut() {
                recording.push(c);
            }
        }
        next
    }
}

impl Lexer {
    fn read_long_comment(&mut self) -> Result<Option<Token>> {
        if self.reader.peek() != Some(&'/') {
            return Ok(None);
        }
        if self.reader.peek2() != Some(&'*') {
            return Ok(None);
        }
        let raw = self.start_recording();
        self.skip_char('/')?;
        self.skip_char('*')?;
        let mut prev = ['?', '?']; // just some random symbol
        Ok(Some(Token::Comment {
            contents: self.read_while(|c| {
                if prev == ['*', '/'] {
                    return false;
                }
                let [_prev1, prev2] = prev;
                prev = [prev2, c];
                true
            })?,
            raw: self.stop_recording(raw),
        }))
    }
    fn read_simple_comment(&mut self) -> Result<Option<Token>> {
        if self.reader.peek() != Some(&'#') {
            return Ok(None);
        }
        let raw = self.start_recording();
        self.skip_char('#')?;
        Ok(Some(Token::Comment {
            contents: self.read_while(|c| c != '\n')?,
            raw: self.stop_recording(raw),
        }))
    }
    fn read_string(&mut self) -> Result<Option<Token>> {
        [StringType::SingleQuoted, StringType::DoubleQuoted]
            .into_iter()
            .find_map(|typ| self.read_string_of(typ).transpose())
            .transpose()
    }
    fn read_string_of(&mut self, typ: StringType) -> Result<Option<Token>> {
        let quote_char = match typ {
            StringType::SingleQuoted => '\'',
            StringType::DoubleQuoted => '"',
        };
        if self.reader.peek() != Some(&quote_char) {
            return Ok(None);
        }
        let raw = self.start_recording();
        self.skip_char(quote_char)?;
        let mut contents = String::new();
        while let Some(&c) = self.reader.peek() {
            if c == quote_char {
                break;
            }
            self.next().unwrap();
            if c == '\\' {
                contents.push(match self.next() {
                    None => return error!("Expected escaped character, got EOF"),
                    Some('n') => '\n',
                    Some('r') => '\r',
                    Some('t') => '\t',
                    Some('\\') => '\\',
                    Some('x') => {
                        let mut read_digit = || match self.next() {
                            Some(c) => match c.to_digit(16) {
                                Some(digit) => Ok(digit),
                                None => error!("Expected a hex digit, got {c:?}"),
                            },
                            None => error!("Expected a hex digit, got EOF"),
                        };
                        let digit1 = read_digit()?;
                        let digit2 = read_digit()?;
                        let char_code = digit1 * 16 + digit2;
                        char::from_u32(char_code)
                            .ok_or(error_fmt!("{char_code:?} is not a valid char code"))?
                    }
                    Some(c) => c,
                });
            } else {
                contents.push(c);
            }
        }
        self.skip_char(quote_char)?;
        Ok(Some(Token::String(StringToken {
            raw: self.stop_recording(raw),
            contents,
            typ,
        })))
    }
    fn read_ident(&mut self) -> Result<Option<Token>> {
        let peeked = match self.reader.peek() {
            Some(&c) => c,
            None => return Ok(None),
        };
        match peeked {
            '@' => {
                let raw = self.start_recording();
                self.next().unwrap();
                let Some(Token::String(StringToken { contents: name, .. })) = self.read_string()?
                else {
                    return error!("Expected a string token after '@' for raw identifier");
                };
                Ok(Some(Token::Ident {
                    name,
                    raw: self.stop_recording(raw),
                    is_raw: true,
                }))
            }
            c if c.is_alphabetic() || c == '_' => {
                let mut name = String::new();
                while let Some(&c) = self.reader.peek() {
                    let is_good = |c: char| c.is_alphanumeric() || c == '_';
                    if is_good(c) || c == '-' && self.reader.peek2().map_or(false, |&c| is_good(c))
                    {
                        name.push(c);
                        self.next().unwrap();
                    } else {
                        break;
                    }
                }
                Ok(Some(Token::Ident {
                    raw: name.clone(),
                    name,
                    is_raw: false,
                }))
            }
            _ => Ok(None),
        }
    }
    fn read_number(&mut self) -> Result<Option<Token>> {
        let peeked = match self.reader.peek() {
            Some(&c) => c,
            None => return Ok(None),
        };
        if !peeked.is_ascii_digit() {
            return Ok(None);
        }
        let mut seen_dot = false;
        let raw = self.read_while(|c| {
            c.is_ascii_digit() || c == '.' && !std::mem::replace(&mut seen_dot, true) || c == '_'
        })?;
        Ok(Some(Token::Number { raw }))
    }
    fn read_punctuation(&mut self) -> Result<Option<Token>> {
        let is_single_punctuation = |c: char| "(){}[]&^".contains(c);
        let is_single_char_punctuation = |c: char| ";".contains(c);
        match self.reader.peek() {
            Some(&first) if is_punctuation(first) => {
                if is_single_punctuation(first) {
                    self.next().unwrap();
                    Ok(Some(Token::Punctuation {
                        raw: first.to_string(),
                    }))
                } else if is_single_char_punctuation(first) {
                    let raw = self.read_while(|c| c == first)?;
                    Ok(Some(Token::Punctuation { raw }))
                } else {
                    let raw = self.read_while(|c| {
                        is_punctuation(c)
                            && !is_single_punctuation(c)
                            && !is_single_char_punctuation(c)
                    })?;
                    Ok(Some(Token::Punctuation { raw }))
                }
            }
            _ => Ok(None),
        }
    }
}

pub fn is_punctuation(c: char) -> bool {
    !(c.is_alphanumeric() || "_'\"@".contains(c) || c.is_whitespace())
}

#[derive(Debug)]
pub struct SpannedToken {
    pub token: Token,
    pub span: Span,
}

impl peek2::ReadableItem for SpannedToken {
    fn advance_position(&self) -> peek2::AdvancePosition {
        peek2::AdvancePosition::SetTo(self.span.start)
    }
}

impl std::ops::Deref for SpannedToken {
    type Target = Token;
    fn deref(&self) -> &Self::Target {
        &self.token
    }
}

pub fn lex(source: SourceFile) -> Result<peek2::Reader<SpannedToken>, Error> {
    let filename = source.filename.clone();
    let mut lexer = Lexer {
        next_recording_id: 0,
        recordings: HashMap::new(),
        reader: peek2::Reader::read(source),
    };
    let mut tokens = Vec::new();
    loop {
        let token = lexer.next_token()?;
        let eof = token.token.is_eof();
        tokens.push(token);
        if eof {
            break;
        }
    }
    Ok(peek2::Reader::new(filename, tokens))
}
