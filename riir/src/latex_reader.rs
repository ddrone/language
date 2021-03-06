use regex::Regex;

struct LatexReader {
    pub cards_regex: Regex,
    pub cloze_regex: Regex,
}

struct CardInfo {
    // LaTeX code with white color for deletion
    pub code_blank: String,
    // LaTeX code with red color for deletion
    pub code_highlight: String,
    // Cloze contents, used as plain-text for comparison purposes
    pub answer: String,
}

impl LatexReader {
    fn new() -> Self {
        LatexReader {
            cards_regex: Regex::new(
                r"(?s)\\begin\{card}(.*?)\\end\{card}"
            ).unwrap(),
            cloze_regex: Regex::new(
                r"(?s)\\cloze\{(.*?)}"
            ).unwrap(),
        }
    }

    fn parse_cards(&self, card_code: &str) -> Vec<CardInfo> {
        let result = Vec::new();
        let mut spans = Vec::new();
        for m in self.cloze_regex.captures(card_code) {
            let group = m.get(1).unwrap();
            spans.push((group.start(), group.end()))
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use crate::latex_reader::LatexReader;

    #[test]
    fn test_reader() {
        let reader = LatexReader::new();
        let text = r#"
            \begin{card}One-line\end{card}
            \begin{card}
                This is a paragraph \cloze{with} formulas. E.g., $e = mc^2$. Also,
                {\color{white} $\int x = \frac{x^2}{2}$}
                with some text after the formula.
            \end{card}

            \begin{card}
                This is another card
            \end{card}
        "#;

        for m in reader.cards_regex.captures_iter(text) {
            println!("{}", m.get(1).unwrap().as_str());
        }
        for m in reader.cloze_regex.captures_iter(text) {
            println!("{}", m.get(1).unwrap().as_str());
        }
    }
}