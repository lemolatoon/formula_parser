use std::{
    collections::{BTreeMap, BTreeSet, HashSet},
    io,
};

fn main() {
    let mut line = String::new();
    io::stdin()
        .read_line(&mut line)
        .expect("failed to read from stdin");

    let tokens = tokenize(line);
    println!("{:?}", tokens);
    let mut parser = Parser::new(tokens);
    parser.parse();
    println!("{:?}", parser.exprs);
}

#[derive(Debug)]
enum Token {
    Letter(String),
    Reserved(String),
}

fn tokenize(input: String) -> Vec<Token> {
    // tokenize
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
            '-' => {
                assert!(matches!(input.get(i + 1), Some('>')));
                tokens.push(Token::Reserved("->".to_string()));
                i += 2;
                continue;
            }
            '!' | '(' | ')' => {
                tokens.push(Token::Reserved((*current_char).to_string()));
                i += 1;
                continue;
            }
            _ => {
                // token written in alphabet
                let len = input[i..]
                    .iter()
                    .take_while(|&&c| !c.is_whitespace() && !matches!(c, ')' | '-'))
                    .filter(|c| {
                        assert!(c.is_ascii_alphabetic());
                        true
                    }) // nothing
                    .count();
                dbg!(len);
                let slice = input[i..i + len].iter().collect::<String>();
                let reserved = (vec!["and", "or"])
                    .into_iter()
                    .map(|s| s.to_owned())
                    .collect::<HashSet<String>>();
                if reserved.contains(&slice) {
                    tokens.push(Token::Reserved(slice));
                } else {
                    tokens.push(Token::Letter(slice));
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
    exprs: Vec<Expr>,
    position: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Node {
    Then(Box<Node>, Box<Node>),
    And(Box<Node>, Box<Node>),
    Or(Box<Node>, Box<Node>),
    Not(Box<Node>),
    Letter(String),
}

impl Node {
    fn new_then(lhs: Box<Self>, rhs: Box<Self>) -> Box<Self> {
        Box::new(Self::Then(lhs, rhs))
    }

    fn new_and(lhs: Box<Self>, rhs: Box<Self>) -> Box<Self> {
        Box::new(Self::And(lhs, rhs))
    }

    fn new_or(lhs: Box<Self>, rhs: Box<Self>) -> Box<Self> {
        Box::new(Self::Or(lhs, rhs))
    }

    fn new_not(operand: Box<Self>) -> Box<Self> {
        Box::new(Self::Not(operand))
    }

    fn new_letter(name: String) -> Box<Self> {
        Box::new(Self::Letter(name))
    }
}

#[derive(Clone, Debug, PartialEq)]
struct Expr {
    node: Box<Node>,
    vars: BTreeSet<Box<Node>>,
}

impl Expr {
    pub const fn new(node: Box<Node>, vars: BTreeSet<Box<Node>>) -> Self {
        Self { node, vars }
    }

    pub fn calc(&self, input: BTreeMap<String, bool>) -> Result<bool, ()> {
        if input.len() != self.vars.len() {
            dbg!(input);
            return Err(());
        }

        Ok(Self::calc_expr(&self.node, &input))
    }

    fn calc_expr(node: &Node, input: &BTreeMap<String, bool>) -> bool {
        match &*node {
            Node::Then(lhs, rhs) => {
                Node::calc_then(Self::calc_expr(lhs, input), Self::calc_expr(rhs, input))
            }
            Node::And(lhs, rhs) => {
                Node::calc_and(Self::calc_expr(lhs, input), Self::calc_expr(rhs, input))
            }
            Node::Or(lhs, rhs) => {
                Node::calc_or(Self::calc_expr(lhs, input), Self::calc_expr(rhs, input))
            }
            Node::Not(expr) => Node::calc_not(Self::calc_expr(expr, input)),
            Node::Letter(str) => *input
                .get(str)
                .unwrap_or_else(|| panic!("Unexpected Node::Letter, input was {:?}", input)),
        }
    }
}

impl Node {
    fn calc_then(lhs: bool, rhs: bool) -> bool {
        match (lhs, rhs) {
            (true, true) => true,
            (true, false) => false,
            (false, true) => true,
            (false, false) => true,
        }
    }

    fn calc_and(lhs: bool, rhs: bool) -> bool {
        match (lhs, rhs) {
            (true, true) => true,
            (true, false) => false,
            (false, true) => false,
            (false, false) => false,
        }
    }

    fn calc_or(lhs: bool, rhs: bool) -> bool {
        match (lhs, rhs) {
            (true, true) => true,
            (true, false) => true,
            (false, true) => true,
            (false, false) => false,
        }
    }

    #[inline]
    fn calc_not(expr: bool) -> bool {
        !expr
    }
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            position: 0,
            exprs: Vec::new(),
        }
    }

    // if current_token is TK_RESERVED(punct) then increment self.position and return Ok(true)
    // tokens.get(self.position) is None then return Err()
    fn consume(&mut self, punct: &str) -> Result<bool, String> {
        dbg!("consume!!");
        dbg!(&self.tokens);
        dbg!(self.tokens.get(self.position));
        if let Some(token) = self.tokens.get(self.position) {
            match token {
                Token::Reserved(s) => {
                    if dbg!(s) == dbg!(punct) {
                        self.position += 1;
                        Ok(true)
                    } else {
                        println!("{:?}", token);
                        dbg!(token);
                        Ok(false)
                    }
                }
                Token::Letter(_) => Ok(false),
            }
        } else {
            Err("self.tokens.get(self.position) must not be None".to_string())
        }
    }

    fn expect_name(&mut self) -> String {
        let name = if let Some(Token::Letter(name)) = self.get_token() {
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

    fn parse(&mut self) {
        let mut vars = BTreeSet::new();

        let node = self.parse_expr(&mut vars);
        self.exprs.push(Expr::new(node, vars));
    }

    fn parse_expr(&mut self, vars: &mut BTreeSet<Box<Node>>) -> Box<Node> {
        let lhs = dbg!(self.parse_and_or(vars));
        println!("{:?}", self);
        if let Ok(true) = dbg!(self.consume("->")) {
            // consume
            Node::new_then(lhs, self.parse_and_or(vars))
        } else {
            lhs
        }
    }

    fn parse_and_or(&mut self, vars: &mut BTreeSet<Box<Node>>) -> Box<Node> {
        let lhs = self.parse_not(vars);
        if let Ok(true) = self.consume("and") {
            Node::new_and(lhs, self.parse_and_or(vars))
        } else if let Ok(true) = self.consume("or") {
            Node::new_or(lhs, self.parse_and_or(vars))
        } else {
            lhs
        }
    }

    fn parse_not(&mut self, vars: &mut BTreeSet<Box<Node>>) -> Box<Node> {
        if let Ok(true) = self.consume("!") {
            Node::new_not(self.parse_primary(vars))
        } else {
            self.parse_primary(vars)
        }
    }

    fn parse_primary(&mut self, vars: &mut BTreeSet<Box<Node>>) -> Box<Node> {
        if let Ok(true) = self.consume("(") {
            let node = self.parse_expr(vars);
            dbg!(node.clone());
            assert!(self
                .consume(")")
                .unwrap_or_else(|_| panic!("`)` expected, but got {:?}", self.get_token())));
            node
        } else {
            let node = Node::new_letter(self.expect_name());
            dbg!(node.clone());
            vars.insert(node.clone());
            node
        }
    }
}

fn gen_table(expr: Expr) -> String {
    let var_len = expr.vars.len();
    let num_row = 2u128.pow(expr.vars.len().try_into().unwrap());
    let mut vec_map = Vec::with_capacity(num_row as usize);

    let mut table = String::with_capacity(num_row as usize * 100);
    for (counter, letter) in (0u128..num_row).zip(expr.vars.iter()) {
        let name = if let Node::Letter(name) = *letter.clone() {
            name
        } else {
            panic!("Letter is not allowed here");
        };
        vec_map.push(Vec::new());
        for i in 0..var_len {
            let input = match (counter >> i) & 1 {
                0 => true,
                1 => false,
                _ => panic!("Illegal bit number"),
            };
            vec_map.last_mut().unwrap().push((name.clone(), input));
        }
        println!("{:?}", vec_map);
        let last = vec_map.last().unwrap();
        if counter == 1 {
            let mut length = 0;
            for (name, _) in last.iter() {
                let str = &format!("| {} ", name);
                length += str.len();
                table.push_str(str);
            }
            table.push_str("\n");
            for _ in 0..length {
                table.push_str("|-");
            }
            table.push_str("|\n");
        }
        for (name, b) in last.iter() {}
    }

    unimplemented!()
}

#[cfg(test)]
fn calc(s: &str, input: BTreeMap<String, bool>) -> bool {
    let expr = tokenize_and_parse(s);
    println!("{:?}", expr.clone());
    dbg!(expr.clone());
    expr.calc(input).expect("Failed to calc")
}

#[cfg(test)]
fn tokenize_and_parse(s: &str) -> Expr {
    let tokens = tokenize(s.to_string());
    let mut parser = Parser::new(tokens);
    parser.parse();
    parser.exprs.pop().unwrap()
}

#[cfg(test)]
fn letter(s: &str) -> Box<Node> {
    Box::new(Node::Letter(s.to_string()))
}

#[cfg(test)]
fn not(node: Box<Node>) -> Box<Node> {
    Box::new(Node::Not(node))
}

#[cfg(test)]
fn and(lhs: Box<Node>, rhs: Box<Node>) -> Box<Node> {
    Box::new(Node::And(lhs, rhs))
}

#[cfg(test)]
fn or(lhs: Box<Node>, rhs: Box<Node>) -> Box<Node> {
    Box::new(Node::Or(lhs, rhs))
}

#[cfg(test)]
fn then(lhs: Box<Node>, rhs: Box<Node>) -> Box<Node> {
    Box::new(Node::Then(lhs, rhs))
}

#[cfg(test)]
macro_rules! vars {
    ( $( $str:expr ), *) => {{
        let mut temp_set = BTreeSet::new();
        $(
            temp_set.insert(letter($str));
        )*
        temp_set
    }};
}

#[test]
fn test1() {
    assert_eq!(tokenize_and_parse("P"), Expr::new(letter("P"), vars!["P"]));
}
#[test]
fn test2() {
    assert_eq!(
        tokenize_and_parse("!P"),
        Expr::new(not(letter("P")), vars!["P"])
    );
}
#[test]
fn test3() {
    assert_eq!(
        tokenize_and_parse("P and Q"),
        Expr::new(and(letter("P"), letter("Q")), vars!["P", "Q"])
    );
}

#[test]
fn test4() {
    assert_eq!(
        tokenize_and_parse("P or Q"),
        Expr::new(or(letter("P"), letter("Q")), vars!["P", "Q"])
    );
}

#[test]
fn test5() {
    assert_eq!(
        tokenize_and_parse("P -> Q"),
        Expr::new(then(letter("P"), letter("Q")), vars!["P", "Q"])
    );
}

#[test]
fn test6() {
    assert_eq!(
        tokenize_and_parse("!P -> Q"),
        Expr::new(then(not(letter("P")), letter("Q")), vars!["P", "Q"])
    );

    assert_eq!(
        tokenize_and_parse("!P -> Q and R"),
        Expr::new(
            then(not(letter("P")), and(letter("Q"), letter("R"))),
            vars!["P", "Q", "R"]
        )
    );

    assert_eq!(
        tokenize_and_parse("(S -> Q) and R"),
        Expr::new(
            and(then(letter("S"), letter("Q")), letter("R")),
            vars!["S", "Q", "R"]
        )
    );

    assert_eq!(
        tokenize_and_parse("P->Q"),
        Expr::new(then(letter("P"), letter("Q")), vars!["P", "Q"])
    );

    assert_eq!(
        tokenize_and_parse("(!S->Q)"),
        Expr::new(then(not(letter("S")), letter("Q")), vars!["S", "Q"])
    );

    assert_eq!(
        tokenize_and_parse("(!S->Q)and (R or C)"),
        Expr::new(
            and(
                then(not(letter("S")), letter("Q")),
                or(letter("R"), letter("C"))
            ),
            vars!["S", "Q", "R", "C"]
        )
    );

    assert_eq!(
        tokenize_and_parse("(((!P) and Q) -> R) "),
        Expr::new(
            then(and(not(letter("P")), letter("Q")), letter("R")),
            vars!["P", "Q", "R"]
        )
    );
}

#[cfg(test)]
fn inmap(names: Vec<&str>, inputs: Vec<bool>) -> BTreeMap<String, bool> {
    names
        .into_iter()
        .zip(inputs.into_iter())
        .fold(BTreeMap::new(), |mut map, (name, input)| {
            map.insert(name.to_string(), input);
            map
        })
}

#[test]
fn test7() {
    assert_eq!(calc("P", inmap(vec!["P"], vec![true])), true);

    assert_eq!(calc("!P", inmap(vec!["P"], vec![true])), false);

    assert_eq!(
        calc("P and Q", inmap(vec!["P", "Q"], vec![true, true])),
        true
    );

    assert_eq!(
        calc("P or Q", inmap(vec!["P", "Q"], vec![false, true])),
        true
    );

    assert_eq!(
        calc("P -> Q", inmap(vec!["P", "Q"], vec![false, false])),
        true
    );

    assert_eq!(
        calc(
            "(P or Q) and (! P or ! Q)",
            inmap(vec!["P", "Q"], vec![false, false])
        ),
        false
    )
}
