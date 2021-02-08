use encoding::all::UTF_16BE;
use encoding::{DecoderTrap, Encoding};

pub fn get_annotations(file_name: &str) -> Vec<String> {
    let mut annotations: Vec<String> = Vec::new();
    let document = lopdf::Document::load(file_name).unwrap();
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
                        if !value.is_empty() {
                            annotations.push(value);
                        }
                    }
                }
            }
        }
    }

    annotations
}
