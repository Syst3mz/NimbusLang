use crate::nimbus_lexer::TokenType::String;

pub struct Language<T:ToString> {
    lang: Vec<Rule<T>>
}

impl<T: ToString> Language<T> {
    fn add(&mut self, rule: Rule<T>) {
        self.lang.push(rule);
    }
}

impl<T: ToString> ToString for Language<T> {
    fn to_string(&self) -> String {
        self.lang.iter().map(|x| x.to_string()).collect::<Vec<String>>().join("\n")
    }
}

pub enum Rule<T:ToString> {
    NonTerminal {
        name: String,
        inside: Box<NonTerminal<T>>
    },
    Terminal {
        name: Option<String>,
        token: T
    }
}

impl<T:ToString> ToString for Rule<T> {
    fn to_string(&self) -> String {
        match self {
            Rule::NonTerminal { name, inside } => {
                format!("{name}:{}", indent(inside.to_string(), "\t"))
            }
            Rule::Terminal { name, token } => {
                format!("{}{}", match name {
                    None => String::new(),
                    Some(name) => {format!("{}: ", name)}
                }, token.to_string())
            }
        }
    }
}

pub enum NonTerminal<T:ToString> {
    Application(Rule<T>),
    Any(Vec<Rule<T>>),
    All(Vec<Rule<T>>),
    Optional(Rule<T>),
    Star(Rule<T>),
    Plus(Rule<T>)
}

impl<T:ToString> ToString for NonTerminal<T> {
    fn to_string(&self) -> String {
        match self {
            NonTerminal::Application(a) => {a.to_string()}
            NonTerminal::Any(rules) => {
                rules
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join("\n\t|")}
            NonTerminal::All(rules) => {
                rules.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join("\n\t&")}
            NonTerminal::Optional(rule) => {format!("{}?", rule.to_string())}
            NonTerminal::Star(rule) => {format!("{}*", rule.to_string())}
            NonTerminal::Plus(rule) => {format!("{}+", rule.to_string())}
        }
    }
}

fn indent(what: String, with: &str) -> String {
    let mut q = String::from(with);
    q.push_str(what.replace("\n", format!("\n{with}").as_str()).as_str());
    q
}