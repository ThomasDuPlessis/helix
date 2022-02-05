use regex::Regex;

enum ParsedValue <'a> {
    ParsedInt(i32),
    ParsedStr(&'a str),
    Empty,
}

enum ParseResult<'a, V> {
    Parsed(usize, V),
    SuccessNoVal(usize),
    Error(&'a str),
}


struct Parser<'a, O> {
    fun: Box<dyn Fn(&'a str, usize) -> ParseResult<'a, O> + 'a>,
}

impl<'a, O> Parser<'a, O> {
    fn parse(self, text: &'a str, pos: usize) -> ParseResult<'a, O> {
        self.fun.call_mut(text, pos)
    }

    fn new<P>(parser: P) -> Self
    where
        P: Fn(&'a str, usize) -> ParseResult<'a, O> + 'a,
    {
        Parser {
            fun: Box::new(parser),
        }
    }
}

fn seq<'a, O>(parsers: Vec<Parser<'a, O>>) -> Parser<'a, Vec<O>>
where
    O: 'a,
{
    Parser::new(move |text: &'a str, pos: usize| {
        let result = Vec::new();
        let mut _pos = pos;
        for parser in parsers {
            match parser.parse(text, pos) {
                ParseResult::Parsed(new_pos, val) => {
                    result.push(val);
                    _pos = new_pos;
                }
                ParseResult::SuccessNoVal(new_pos) => {
                    _pos = new_pos;
                }
                ParseResult::Error(e) => return ParseResult::Error(e),
            }
        }
        if result.len() > 0 {
            return ParseResult::Parsed(_pos, result);
        }
        ParseResult::SuccessNoVal(_pos)
    })
}

fn or<'a, O>(parsers: Vec<Parser<'a, O>>) -> Parser<'a, O>
where
    O: 'a,
{
    Parser::new(move |text: &'a str, pos: usize| {
        let errs = Vec::new();
        for p in parsers {
            match p.parse(text, pos) {
                o @ ParseResult::Parsed(_, _) => o,
                n @ ParseResult::SuccessNoVal(_) => n,
                ParseResult::Error(e) => {
                    errs.push(e);
                    continue;
                }
            };
        }
    ParseResult::Error(format!("Failed all parsings: {} ",errs.join("\n")).as_str())
})
}

// fn any<'a, P, O>(parsers: Vec<P>) -> impl Parser<'a, O>
// where
//     P: Parser<'a, O>,
// {
//     move |text, pos| {
//         let mut result = Err(String::new());
//         for parser in parsers {
//             match parser.parse(text, pos) {
//                 o @ Ok(_) => {
//                     result = o;
//                     break;
//                 }
//                 Err(err) => {
//                     result = match result {
//                         Err(err1) => Err(String::new() + &err1 + ". " + &err),
//                         Ok(_) => {
//                             unreachable!("If result is Ok, this fn would have already returned")
//                         }
//                     }
//                 }
//             }
//         }
//         result
//     }
// }

fn sym<'a, O>(s: &'a str) -> Parser<'a, O> {
    Parser::new(move |text: &str, pos| {
        if &text[pos..pos + s.chars().count()] == s {
            ParseResult::SuccessNoVal(pos + s.chars().count())
        } else {
            ParseResult::Error(
                format!("Could not find {} at spot {} in {}.", s, pos, text).as_str(),
            )
        }
    })
}

fn pattern<'a>(pattern: &'a str) -> Parser<'a, &'a str> {
    Parser::new(
        move |text: &'a str, pos| match Regex::new(pattern).unwrap().find(&text[pos..]) {
            Some(mat) => {
                if mat.start() == 0 {
                    ParseResult::Parsed(mat.end() + pos, mat.as_str())
                } else {
                    ParseResult::Error(
                        format!(
                            "Found regex match at {} in {}. Expected it at {}",
                            mat.start() + pos,
                            text,
                            pos
                        )
                        .as_str(),
                    )
                }
            }
            None => ParseResult::Error(
                format!(
                    "Did not find match for regex {} in text {}",
                    pattern,
                    &text[pos..]
                )
                .as_str(),
            ),
        },
    )
}

fn take_until<'a>(c: char) -> Parser<'a, &'a str> {
    Parser::new(move |text: &'a str, pos| {
        for i in pos..text.len() {
            if text.chars().nth(i).unwrap() == c {
                return ParseResult::Parsed(i, &text[pos..i]);
            }
        }
        ParseResult::Error(format!("Did not find char {} in {}", c, text).as_str())
    })
}

fn map<'a, F, A, B>(parser: Parser<'a, A>, f: F) -> Parser<'a, B>
where
    F: Fn(A) -> B + 'a,
    A: 'a,
{
    Parser::new(move |text: &'a str, pos| match parser.parse(text, pos) {
        ParseResult::Parsed(new_pos, result) => ParseResult::Parsed(new_pos, f(result)),
        ParseResult::SuccessNoVal(new_pos) => ParseResult::SuccessNoVal(new_pos),
        ParseResult::Error(err) => ParseResult::Error(err),
    })
}

fn int<'a>() -> Parser<'a, PartialResult<'a>> {
    map(pattern(r"^\d+"), |s: &str| {
        PartialResult::Int(s.parse::<i32>().unwrap())
    })
}

fn separated<'a, A>(parser: Parser<'a, A>, sep: char) -> Parser<'a, Vec<A>>
where
    A: 'a,
{
    Parser::new(move |text: &'a str, pos| {
        let mut _pos = pos;
        let mut vals: Vec<A> = Vec::new();
        match parser.parse(text, _pos) {
            ParseResult::Parsed(new_pos, result) => {
                _pos = new_pos;
                vals.push(result);
            }
            ParseResult::SuccessNoVal(new_pos) => {
                _pos = new_pos;
            }
            ParseResult::Error(err) => {
                return ParseResult::Error(
                    format!("failed to parse repeated list once: {}", err).as_str(),
                )
            }
        }
        while let ParseResult::SuccessNoVal(new_pos) = sym::<()>(&sep.to_string()).parse(text, _pos) {
            _pos = new_pos;
            match parser.parse(text, _pos) {
                ParseResult::Parsed(new_pos, result) => {
                    _pos = new_pos;
                    vals.push(result);
                }
                ParseResult::SuccessNoVal(new_pos) => {
                    _pos = new_pos;
                }
                ParseResult::Error(err) => {
                    return ParseResult::Error(
                        format!("Trailing '{}' followed by unparseable text: {}", sep, err)
                            .as_str(),
                    )
                }
            }
        }
        ParseResult::Parsed(_pos, vals)
    })
}

fn var<'a>() -> Parser<'a, &'a str> {
    pattern("^[_a-zA-Z][_a-zA-Z0-9]*")
}

#[derive(Clone, Debug, PartialEq)]
pub enum Snippet<'a> {
    Tabstop(i32),
    Placeholder(i32, Box<Snippet<'a>>),
    Variable(&'a str, VariableBody<'a>),
    Choice(i32, Vec<&'a str>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum VariableBody<'a> {
    Empty,
    NestedSnippet(Box<Snippet<'a>>),
    Transform {
        regex: &'a str,
        format: Vec<Transformation<'a>>,
        options: &'a str,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Transformation<'a> {
    Text(&'a str),
    Format(Format<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Format<'a> {
    Int(i32, FormatCase),
    If(i32, &'a str),
    IfElse(i32, &'a str, &'a str),
    Else(i32, &'a str),
}

#[derive(Clone, Debug, PartialEq)]
enum FormatCase {
    None,
    Upcase,
    Downcase,
    Capitalize,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token<'a> {
    None,
    Int(i32),
    Var(&'a str),
}

pub fn tabstop<'a>() -> Parser<'a, Snippet<'a>> {
    let dollar_int = map(
        seq(vec![sym("$"), int()]),
        |v| v[1],
    );
    let enclosed = map(
        seq(vec![sym("${"), int(), sym("}")]),
        |v| v[1],
    );
    map(or(vec![dollar_int, enclosed]), |v| Snippet::Tabstop(v.parse::<i32>().unwrap()))
}

enum PartialResult<'a> {
    Int(i32),
    Str(&'a str),
    Snippet(Snippet<'a>)
}

pub fn placeholder<'a>() -> Parser<'a, Snippet<'a>> {
    map(seq(vec![sym("${"),
       int(),
        map(anything(), |v| PartialResult::Snippet(v)),
        sym("}")
        ]), |v| {
            match (v[1], v[2]) {
                (PartialResult::Str(s), PartialResult::Snippet(snip)) =>
                    Snippet::Placeholder(s.parse::<i32>().unwrap(), Box::new(snip)),
                _ => panic!("unreachable")
            }
          
        })
}

pub fn choice<'a>() -> Parser<'a, Snippet<'a>> {
    map(seq(vec![
        sym("${"),
        int(),
        sym("|"),
        map(separated(or(vec![take_until('|'), take_until(',')]), ','), |v| PartialResult::Snippet(Snippet::Choice(0, v))),
        sym("|}")
    ]), |v| { 
        match (v[1], v[3]) {
            (PartialResult::Int(i), PartialResult::Snippet(Snippet::Choice(_, vs))) =>  Snippet::Choice(i, vs),
            _ => panic!("unhreachable")
    }
})
}


macro_rules! must_match{
    ($e: expr, $p:pat, $b: block) => {
        match ($e) {
         $p => $b,
         _ => unreachable!("unreachable")
        }
    };
}

fn format<'a>() -> Parser<'a, Format<'a>> {
    or(vec![
        map(seq(vec![sym("$"), int()]), |v|{ must_match!(v[1], PartialResult::Int(i), {
            Format::Int(i, FormatCase::None)})
        }),
        map(seq(vec![sym("${"), int(), sym("}")]), |v|{ if let PartialResult::Int(i) = v[1] {
            Format::Int(i, FormatCase::None)
        } else {
            unreachable!("unreachable")
        }}), 
        map(seq(vec![sym("${"), int(), sym(":/"), or(vec![sym("upcase"), sym("downcase"), sym("capitalize")])]),
        |v|{ if let PartialResult::Int(i) = v[1] {
            Format::Int(i, FormatCase::None)
        } else {
            unreachable!("unreachable")
        }}
    )
        ])
}

// pub fn variable<'a>() -> ParseResult<'a, Snippet<'a>> {
//     (sym("$").seq(var()).or(sym("${").seq(var()).seq(sym("}")))
// }

pub fn anything<'a>() -> Parser<'a, Snippet<'a>> {
    tabstop().or(placeholder()).or(choice()).or(choice())
    // .or(variable())
}

#[test]
fn test_tabstop_parser() {
    assert_eq!(tabstop().parse("${123}", 0), Ok((6, Snippet::Tabstop(123))));
    assert_eq!(tabstop().parse("$123", 0), Ok((4, Snippet::Tabstop(123))));
}

#[test]
fn test_choice() {
    let s = "${1|one,two,three|}";
    assert_eq!(
        choice().parse(s, 0),
        Ok((s.len() + 1, Snippet::Choice(1, vec!["one", "two", "three"])))
    );
}
