pub mod pdf_annotations;

use chrono::prelude::*;
use pulldown_cmark::{CodeBlockKind, CowStr, Event, Parser, Tag};
use serde::{Deserialize, Serialize};
use std::fs;
use yaml_rust::yaml::Hash;
use yaml_rust::{Yaml, YamlLoader};

#[derive(Debug)]
enum ParseError {
    MissingKey(&'static str),
    ExpectedStringOnKey(&'static str),
    NotHash,
}

#[derive(Serialize, Deserialize, Debug)]
pub enum CardData {
    Simple {
        front: String,
        back: String,
    },
    Cloze {
        text: String,
        hint: String,
        answer: String,
    },
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Card {
    pub data: CardData,
    pub bucket: u8, // 2^255 days between card repetitions ought to be enough for anybody
    pub source_filename: String,
    pub last_reviewed: DateTime<Utc>,
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

fn parse_card(file_name: &str, v: &Yaml) -> Result<Card, ParseError> {
    match v {
        Yaml::Hash(h) => {
            let data = parse_simple_card(h).or_else(|_| parse_cloze_card(h))?;
            Ok(Card {
                data,
                bucket: 0,
                source_filename: file_name.to_string(),
                last_reviewed: Utc::now(),
            })
        }
        _ => Err(ParseError::NotHash),
    }
}

pub fn parse_file(file_name: &str) -> Vec<Card> {
    let input = fs::read_to_string(file_name).expect("oops");
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
                        cards.push(parse_card(file_name, &doc).expect("invalid card"));
                    }
                }
            }
            _ => {}
        }
    }

    cards
}
