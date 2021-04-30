use crate::expr::{Exp, pair, var};
use std::iter::Peekable;

fn is_single_token_lexeme(c: char) -> bool {
    c == '(' || c == ')' || c == ','
}

enum State {
    Ambient,
    Started(usize)
}

pub(crate) fn tokenize(input: String) -> Vec<(usize, String)>
{
    let mut result = Vec::new();

    // TODO: this probably is not going to work for non-ascii characters since enumerating chars is
    // not going to return the right byte offsets. Probably should fix this later.
    let mut state = State::Ambient;
    for (i, c) in input.chars().enumerate() {
        match &state {
            State::Ambient => {
                if c == ' ' || c == '\n' {
                    continue
                }
                else if is_single_token_lexeme(c) {
                    result.push((i, c.to_string()))
                }
                else {
                    state = State::Started(i)
                }
            }
            State::Started(s) => {
                if c == ' ' || c == '\n' {
                    result.push((*s, (&input[*s..i]).to_string()));
                    state = State::Ambient
                }
                else if is_single_token_lexeme(c) {
                    result.push((*s, (&input[*s..i]).to_string()));
                    result.push((i, c.to_string()));
                    state = State::Ambient
                }
            }
        }
    }

    result
}

pub(crate) fn parse<'a, Iter>(input: &mut Peekable<Iter>) -> Result<Exp, String>
where Iter: Iterator<Item=&'a (usize, String)> {
    match input.next() {
        None => Err("unexpected EOF".to_string()),
        Some(s) => {
            if s.1 == "(" {
                let mut ls = parse_list(input)?;
                if ls.len() == 1 {
                    Ok(ls.remove(0))
                }
                else if ls.len() == 2 {
                    let x2 = ls.pop().unwrap();
                    let x1 = ls.pop().unwrap();
                    Ok(pair(x1, x2))
                } else {
                    Err("wrong tuple".to_string())
                }
            }
            else {
                Ok(var(s.1.clone()))
            }
        }
    }
}

fn parse_list<'a, Iter>(input: &mut Peekable<Iter>) -> Result<Vec<Exp>, String>
where Iter: Iterator<Item=&'a (usize, String)> {
    let mut result = Vec::new();
    result.push(parse(input)?);
    loop {
        match input.next() {
            None => return Err("unexpected EOF".to_string()),
            Some(s) => {
                if s.1 == "," {
                    result.push(parse(input)?);
                    continue
                }
                else if s.1 == ")" {
                    break
                }
                else {
                    return Err("unexpected symbol".to_string())
                }
            }
        }
    }
    Ok(result)
}