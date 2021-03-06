use std::collections::HashMap;
use std::fs;
use std::path::Path;

use chrono::prelude::*;
use pulldown_cmark::{CodeBlockKind, CowStr, Event, Parser, Tag};
use serde::{Deserialize, Serialize};

use crate::cloze::{parse_cloze, Cloze, ClozeChunk};

pub mod cloze;
pub mod pdf_annotations;
pub mod render_cards;
pub mod review_cards;
mod lexer;
mod latex_reader;

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ReviewInfo {
    pub bucket: u8, // 2^255 days between card repetitions ought to be enough for anybody
    pub last_reviewed: DateTime<Utc>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Card {
    pub text: String,
    pub data: Cloze,
    pub review: Vec<ReviewInfo>,
    pub source_filename: String,
}

pub fn read_cards_index() -> HashMap<String, Card> {
    let input = match fs::read_to_string("cards.json") {
        Ok(input) => input,
        Err(_) => return HashMap::new(),
    };
    let cards: Vec<Card> = serde_json::from_str(&input).expect("parse failed");
    let mut index = HashMap::new();
    for card in cards {
        index.insert(card.text.clone(), card);
    }
    index
}

pub fn parse_file<P: AsRef<Path>>(file_name: P) -> Vec<Card> {
    let input = fs::read_to_string(&file_name).expect("oops");
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
                if !parsing_card {
                    continue;
                }

                match parse_cloze(&s) {
                    None => {
                        // TODO: Implement better error handling
                        panic!("Invalid card!")
                    }
                    Some(card) => {
                        let mut review_infos = Vec::new();
                        for chunk in &card.chunks {
                            match chunk {
                                ClozeChunk::Open(_) => {}
                                ClozeChunk::Close { .. } => {
                                    review_infos.push(ReviewInfo {
                                        bucket: 0,
                                        last_reviewed: Utc::now(),
                                    });
                                }
                            }
                        }
                        cards.push(Card {
                            text: s.to_string(),
                            data: card,
                            review: review_infos,
                            source_filename: file_name.as_ref().to_str().unwrap().to_string(),
                        })
                    }
                }
            }
            _ => {}
        }
    }

    cards
}
