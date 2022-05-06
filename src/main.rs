use std::{collections::HashSet, env::current_exe, fmt::format, io};

fn main() {
    let mut line = String::new();
    io::stdin()
        .read_line(&mut line)
        .expect("failed to read from stdin");

    let tokens = tokenize(line);
    println!("{:?}", tokens);
    let mut parser = Parser::new(tokens);
    let node = parser.parse_expr();
    println!("{:?}", node);
}

#[derive(Debug)]
enum Token {
    TkLetter(String),
    TkReserved(String),
}

fn tokenize(input: String) -> Vec<Token> {
    // tokenize
    use Token::*;
    let input = input.chars().collect::<Vec<char>>();

    let mut tokens = Vec::<Token>::new();
    let mut i = 0;

    while let Some(current_char) = input.get(i) {
        dbg!(current_char);
        // skip white space
        if input[i].is_whitespace() {
            i += 1;
            continue;
        }

        match *current_char {
            '-' | '!' | '(' | ')' => {
                // punctuator at least
                // count length of punctuator
                let len = input[(i + 1)..]
                    .iter()
                    .take_while(|&&c| !matches!(c, ' ' | '\n' | '\t' | 'a'..='z' | 'A'..='Z' | '!'))
                    .count()
                    + 1;
                dbg!(len);
                let slice = input[i..i + len].into_iter().collect::<String>();
                dbg!(&slice);
                let reserved = (vec!["->", "!", "(", ")"])
                    .into_iter()
                    .map(|s| s.to_owned())
                    .collect::<HashSet<String>>();
                if reserved.contains(&slice) {
                    tokens.push(TkReserved(slice));
                }
                i += len;
            }
            _ => {
                let len = input[i..]
                    .iter()
                    .take_while(|&&c| !c.is_whitespace() && !matches!(c, ')' | '-'))
                    .filter(|c| {
                        assert!(c.is_ascii_alphabetic());
                        true
                    }) // nothing
                    .count();
                dbg!(len);
                let slice = input[i..i + len].into_iter().collect::<String>();
                let reserved = (vec!["and", "or"])
                    .into_iter()
                    .map(|s| s.to_owned())
                    .collect::<HashSet<String>>();
                if reserved.contains(&slice) {
                    tokens.push(TkReserved(slice));
                } else {
                    tokens.push(TkLetter(slice));
                }
                i += len;
            }
        }
    }
    tokens
}

#[derive(Debug)]
struct Parser {
    tokens: Vec<Token>,
    nodes: Vec<Node>,
    position: usize,
}

#[derive(Debug, PartialEq, Eq)]
enum Node {
    NdThen(Box<Node>, Box<Node>),
    NdAnd(Box<Node>, Box<Node>),
    NdOr(Box<Node>, Box<Node>),
    NdNot(Box<Node>),
    NdLetter(String),
}

impl Node {
    fn new_then(lhs: Box<Self>, rhs: Box<Self>) -> Box<Self> {
        Box::new(Self::NdThen(lhs, rhs))
    }

    fn new_and(lhs: Box<Self>, rhs: Box<Self>) -> Box<Self> {
        Box::new(Self::NdAnd(lhs, rhs))
    }

    fn new_or(lhs: Box<Self>, rhs: Box<Self>) -> Box<Self> {
        Box::new(Self::NdOr(lhs, rhs))
    }

    fn new_not(operand: Box<Self>) -> Box<Self> {
        Box::new(Self::NdNot(operand))
    }

    fn new_letter(name: String) -> Box<Self> {
        Box::new(Self::NdLetter(name))
    }
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            position: 0,
            nodes: Vec::new(),
        }
    }

    // if current_token is TK_RESERVED(punct) then increment self.position and return Ok(true)
    // tokens.get(self.position) is None then return Err()
    fn consume(&mut self, punct: &str) -> Result<bool, String> {
        dbg!("consume!!");
        use Token::*;
        dbg!(&self.tokens);
        dbg!(self.tokens.get(self.position));
        if let Some(token) = self.tokens.get(self.position) {
            match token {
                TkReserved(s) => {
                    if dbg!(s) == dbg!(punct) {
                        self.position += 1;
                        Ok(true)
                    } else {
                        Ok(false)
                    }
                }
                TkLetter(_) => Ok(false),
            }
        } else {
            Err(format!("self.tokens.get(self.position) must not be None"))
        }
    }

    fn expect_name(&mut self) -> String {
        let name = if let Some(Token::TkLetter(name)) = self.get_token() {
            name.clone()
        } else {
            panic!("This is not TkLetter");
        };
        self.position += 1;
        name
    }

    fn get_token(&self) -> Option<&Token> {
        self.tokens.get(self.position)
    }

    fn expect_letter(&mut self) {
        if let Some(token) = self.get_token() {
            match token {
                Token::TkLetter(_) => return,
                Token::TkReserved(_) => panic!("Expected TkLetter but got TkReserved"),
            }
        } else {
            panic!("Token is None");
        }
    }

    fn parse_expr(&mut self) -> Box<Node> {
        let lhs = dbg!(self.parse_and_or());
        println!("{:?}", self);
        if let Ok(true) = dbg!(self.consume("->")) {
            // consume
            Node::new_then(lhs, self.parse_and_or())
        } else {
            lhs
        }
    }

    fn parse_and_or(&mut self) -> Box<Node> {
        let lhs = self.parse_not();
        if let Ok(true) = self.consume("and") {
            Node::new_and(lhs, self.parse_and_or())
        } else if let Ok(true) = self.consume("or") {
            Node::new_or(lhs, self.parse_and_or())
        } else {
            lhs
        }
    }

    fn parse_not(&mut self) -> Box<Node> {
        if let Ok(true) = self.consume("!") {
            Node::new_not(self.parse_primary())
        } else {
            self.parse_primary()
        }
    }

    fn parse_primary(&mut self) -> Box<Node> {
        if let Ok(true) = self.consume("(") {
            let node = self.parse_expr();
            assert!(self.consume(")").expect("`)` expected"));
            node
        } else {
            Node::new_letter(self.expect_name())
        }
    }
}

fn tokenize_and_parse(s: &str) -> Box<Node> {
    let tokens = tokenize(s.to_string());
    let mut parser = Parser::new(tokens);
    let node = parser.parse_expr();
    node
}

fn letter(s: &str) -> Box<Node> {
    Box::new(Node::NdLetter(s.to_string()))
}

fn not(node: Box<Node>) -> Box<Node> {
    Box::new(Node::NdNot(node))
}

fn and(lhs: Box<Node>, rhs: Box<Node>) -> Box<Node> {
    Box::new(Node::NdAnd(lhs, rhs))
}

fn or(lhs: Box<Node>, rhs: Box<Node>) -> Box<Node> {
    Box::new(Node::NdOr(lhs, rhs))
}

fn then(lhs: Box<Node>, rhs: Box<Node>) -> Box<Node> {
    Box::new(Node::NdThen(lhs, rhs))
}

#[test]
fn test1() {
    assert_eq!(tokenize_and_parse("P"), letter("P"));
}
#[test]
fn test2() {
    assert_eq!(tokenize_and_parse("!P"), not(letter("P")));
}
#[test]
fn test3() {
    assert_eq!(tokenize_and_parse("P and Q"), and(letter("P"), letter("Q")));
}

#[test]
fn test4() {
    assert_eq!(tokenize_and_parse("P or Q"), or(letter("P"), letter("Q")));
}

#[test]
fn test5() {
    assert_eq!(tokenize_and_parse("P -> Q"), then(letter("P"), letter("Q")));
}

#[test]
fn test6() {
    assert_eq!(
        tokenize_and_parse("!P -> Q"),
        then(not(letter("P")), letter("Q"))
    );

    assert_eq!(
        tokenize_and_parse("!P -> Q and R"),
        then(not(letter("P")), and(letter("Q"), letter("R")))
    );

    assert_eq!(
        tokenize_and_parse("(S -> Q) and R"),
        and(then(letter("S"), letter("Q")), letter("R"))
    );

    assert_eq!(tokenize_and_parse("P->Q"), then(letter("P"), letter("Q")));

    assert_eq!(
        tokenize_and_parse("(!S->Q)"),
        then(not(letter("S")), letter("Q"))
    );

    assert_eq!(
        tokenize_and_parse("(!S->Q)and (R or C)"),
        and(
            then(not(letter("S")), letter("Q")),
            or(letter("R"), letter("C"))
        )
    );

    assert_eq!(
        tokenize_and_parse("(((!P) and Q) -> R) "),
        then(and(not(letter("P")), letter("Q")), letter("R"))
    );
}
