use chrono::prelude::*;
use pulldown_cmark::{CodeBlockKind, CowStr, Event, Parser, Tag};
use std::fs;
use yaml_rust::yaml::Hash;
use yaml_rust::{Yaml, YamlLoader};

use note_rusty::*;

#[derive(Debug)]
enum ParseError {
    MissingKey(&'static str),
    ExpectedStringOnKey(&'static str),
    NotHash,
}

fn get_text_value(v: &Hash, key: &'static str) -> Result<String, ParseError> {
    let text = v
        .get(&Yaml::String(key.to_string()))
        .ok_or(ParseError::ExpectedStringOnKey("text not found"))?;
    match text {
        Yaml::String(t) => Ok(t.to_string()),
        _ => Err(ParseError::MissingKey(key)),
    }
}

fn parse_simple_card(v: &Hash) -> Result<CardData, ParseError> {
    let front = get_text_value(v, "front")?;
    let back = get_text_value(v, "back")?;
    Ok(CardData::Simple { front, back })
}

fn parse_cloze_card(v: &Hash) -> Result<CardData, ParseError> {
    let text = get_text_value(v, "text")?;
    let hint = get_text_value(v, "hint")?;
    let answer = get_text_value(v, "answer")?;
    Ok(CardData::Cloze { text, hint, answer })
}

fn parse_card(v: &Yaml) -> Result<Card, ParseError> {
    match v {
        Yaml::Hash(h) => {
            let data = parse_simple_card(h).or_else(|_| parse_cloze_card(h))?;
            Ok(Card {
                data,
                bucket: 0,
                source_filename: "note.md".to_string(),
                last_reviewed: Utc::now(),
            })
        }
        _ => Err(ParseError::NotHash),
    }
}

fn main() {
    let input = fs::read_to_string("note.md").expect("oops");
    let parser = Parser::new(&input);
    let mut cards: Vec<Card> = Vec::new();

    let mut parsing_card = false;
    for token in parser {
        match token {
            Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced(s))) => {
                if s == CowStr::from("card") {
                    parsing_card = true
                }
            }
            Event::End(Tag::CodeBlock(_)) => parsing_card = false,
            Event::Text(s) => {
                let yaml = YamlLoader::load_from_str(&s).expect("invalid yaml");
                if parsing_card {
                    for doc in yaml {
                        cards.push(parse_card(&doc).expect("invalid card"));
                    }
                }
            }
            _ => {}
        }
    }

    let json = serde_json::to_string(&cards).expect("serialize failed");
    fs::write("cards.json", json).expect("write failed");
}
