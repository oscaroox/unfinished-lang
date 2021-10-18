#[derive(Debug, PartialEq, Clone)]
pub struct Identifier(String);

impl Identifier {
    pub fn new(name: String) -> Identifier {
        Identifier { 0: name }
    }
}
