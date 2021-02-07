use encoding::all::UTF_16BE;
use encoding::{DecoderTrap, Encoding};
use lopdf::Error;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let mut annotations: Vec<String> = Vec::new();
    let document = lopdf::Document::load(&args[1]).unwrap();
    let contents_key = b"Contents";
    for (_, obj) in &document.objects {
        if let Ok(name) = obj.type_name() {
            if name != "Annot" {
                continue;
            }
            if let lopdf::Object::Dictionary(dict) = obj {
                if let Ok(value) = dict.get(contents_key) {
                    if let Ok(Ok(value)) = value
                        .as_str()
                        .map(|s| UTF_16BE.decode(&s[2..], DecoderTrap::Strict))
                    {
                        annotations.push(value);
                    }
                }
            }
        }
    }

    for annotation in annotations {
        println!("{}", annotation)
    }
}
