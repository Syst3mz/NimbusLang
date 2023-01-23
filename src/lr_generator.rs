pub enum Rule<'a, T:ToString> {
    NonTerminal {
        name: Option<&'a str>,
        inside: &'a NonTerminal<'a, T>
    },
    Terminal {
        name: Option<&'a str>,
        token: T
    }
}



impl<'a, T:ToString> ToString for Rule<'a, T> {
    fn to_string(&self) -> String {
        match self {
            Rule::NonTerminal { name, inside } => {
                match name {
                    None => {format!("{}", inside.to_string())}
                    Some(a) => {
                        format!("{a} -> {}", inside.to_string())
                    }
                }
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

pub enum NonTerminal<'a, T:ToString> {
    Application(&'a Rule<'a, T>),
    Any(Vec<&'a Rule<'a, T>>),
    All(Vec<&'a Rule<'a, T>>),
    Optional(&'a Rule<'a, T>),
    Star(&'a Rule<'a, T>),
    Plus(&'a Rule<'a, T>)
}

impl<'a, T:ToString> ToString for NonTerminal<'a, T> {
    fn to_string(&self) -> String {
        match self {
            NonTerminal::Application(a) => {a.to_string()}
            NonTerminal::Any(rules) => {
                indent(format!("\n  {}", rules.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join("\n| ")), "\t")}
            NonTerminal::All(rules) => {
                indent(format!("\n  {}", rules.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join("\n& ")), "\t")}
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