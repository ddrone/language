pub mod cloze;
pub mod pdf_annotations;

use crate::cloze::{parse_cloze, Cloze, ClozeChunk};
use chrono::prelude::*;
use pulldown_cmark::{CodeBlockKind, CowStr, Event, Parser, Tag};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::default::Default;
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
pub struct ReviewInfo {
    pub bucket: u8, // 2^255 days between card repetitions ought to be enough for anybody
    pub last_reviewed: DateTime<Utc>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Card {
    pub data: Cloze,
    pub review: HashMap<String, ReviewInfo>,
    pub source_filename: String,
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
                match parse_cloze(&s) {
                    None => {
                        // TODO: Implement better error handling
                        panic!("Invalid card!")
                    }
                    Some(card) => {
                        let mut review_infos = HashMap::new();
                        for chunk in &card.chunks {
                            match chunk {
                                ClozeChunk::Open(_) => {}
                                ClozeChunk::Close { id, .. } => {
                                    // TODO: add a check for duplicate keys
                                    review_infos.insert(
                                        id.clone(),
                                        ReviewInfo {
                                            bucket: 0,
                                            last_reviewed: Utc::now(),
                                        },
                                    );
                                }
                            }
                        }
                        cards.push(Card {
                            data: card,
                            review: review_infos,
                            source_filename: file_name.to_string(),
                        })
                    }
                }
            }
            _ => {}
        }
    }

    cards
}
