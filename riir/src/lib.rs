use chrono::prelude::*;
use serde::{Deserialize, Serialize};

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
