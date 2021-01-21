use std::fs;
use pulldown_cmark::{Parser, Event, Tag, CodeBlockKind, CowStr};
use serde::{Serialize, Deserialize};
use chrono::prelude::*;
use yaml_rust::{YamlLoader, Yaml};
use yaml_rust::yaml::Hash;

#[derive(Serialize, Deserialize, Debug)]
enum CardData {
    Simple { front: String, back: String },
    Cloze { text: String, hint: String, answer: String },
}

#[derive(Serialize, Deserialize, Debug)]
struct Card {
    data: CardData,
    bucket: u8, // 2^255 days between card repetitions ought to be enough for anybody
    source_filename: String,
    last_reviewed: DateTime<Utc>
}

// TODO: figure out what this type signature actually means
fn get_text_value<'a>(v: &'a Hash, key: &'a str) -> Result<&'a String, &'a str> {
    let text = v.get(&Yaml::String(key.to_string())).ok_or("text not found")?;
    match text {
        Yaml::String(t) => Ok(t),
        _ => Err("value is not a string")
    }
}

fn parse_simple_card(v: &Hash) -> Result<CardData, &str> {
    let front = get_text_value(v, "front")?;
    let back = get_text_value(v, "back")?;
    Ok(CardData::Simple { front: front.clone(), back: back.clone() })
}

fn parse_cloze_card(v: &Hash) -> Result<CardData, &str> {
    let text = get_text_value(v, "text")?;
    let hint = get_text_value(v, "hint")?;
    let answer = get_text_value(v, "answer")?;
    Ok(CardData::Cloze { text: text.clone(), hint: hint.clone(), answer: answer.clone() })
}

fn parse_card(v: &Yaml) -> Result<Card, &str> {
    match v {
        Yaml::Hash(h) => {
            let data = parse_simple_card(h).or_else(|_| parse_cloze_card(h))?;
            Ok(Card {
                data,
                bucket: 0,
                source_filename: "note.md".to_string(),
                last_reviewed: Utc::now()
            })
        }
        _ => Err("yaml hash expected")
    }
}

fn main() {
    let input = fs::read_to_string("note.md").expect("oops");
    let parser = Parser::new(&*input);
    let mut cards: Vec<Card> = Vec::new();

    let mut parsing_card = false;
    for token in parser {
        match token {
            Event::Start(e) => {
                if let Tag::CodeBlock(c) = e {
                    if let CodeBlockKind::Fenced(s) = c {
                        if s == CowStr::from("card") {
                            parsing_card = true
                        }
                    }
                }
            }
            Event::End(e) => {
                if let Tag::CodeBlock(_) = e {
                    parsing_card = false
                }
            }
            Event::Text(s) => {
                let yaml = YamlLoader::load_from_str(&*s).expect("invalid yaml");
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
