use crate::Card;
use std::fs;
use std::io::{Error, Read, Write};
use std::process::Command;
use std::process::Stdio;

/// Rendering markdown via pandoc while using another library for parsing
/// Markdown in the same codebase is extremely cursed, but I need to render some
/// math equations and pulldown-cmark doesn't support that, so what you're going
/// to do.
pub fn render_markdown(text: &str, buffer: &mut String) -> Result<(), Error> {
    let process = Command::new("pandoc")
        .args(&["--katex"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();

    process.stdin.unwrap().write_all(text.as_bytes())?;
    process.stdout.unwrap().read_to_string(buffer)?;
    Ok(())
}

pub fn render_cards() -> Result<(), Error> {
    let input = fs::read_to_string("cards.json")?;
    let cards: Vec<Card> = serde_json::from_str(&input)?;

    let mut output = r#"
    <!DOCTYPE html>
    <html>
    <head>
    <meta charset="utf-8" />
    <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/katex@0.12.0/dist/katex.min.css" integrity="sha384-AfEj0r4/OFrOo5t7NnNe46zW/tFgW6x/bCJG8FqQCEo3+Aro6EYUG4+cU+KJWu/X" crossorigin="anonymous">
    <script defer src="https://cdn.jsdelivr.net/npm/katex@0.12.0/dist/katex.min.js" integrity="sha384-g7c+Jr9ZivxKLnZTDUhnkOnsh30B4H0rpLUpJ4jAIKs4fnJI+sEnkvrMWph2EDg4" crossorigin="anonymous"></script>
    </head>
    <body>
    <style>.card { border: 1px solid black; }</style>
    "#.to_string();
    for card in &cards {
        output.push_str(r#"<div class="card">"#);
        render_markdown(&card.text, &mut output)?;
        output.push_str("</div>");
    }
    output.push_str("</body></html>");

    fs::write("cards.html", output)
}

#[cfg(test)]
mod tests {
    use crate::render_cards::render_markdown;

    #[test]
    fn test_simple() {
        let mut output = String::new();
        render_markdown("Eqn: $1 + 2 = 3$", &mut output).unwrap();
        let expected = "<p>Eqn: <span class=\"math inline\">1 + 2 = 3</span></p>\n";
        assert_eq!(output, expected);
    }
}
