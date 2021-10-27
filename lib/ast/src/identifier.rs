#[derive(Debug, PartialEq, Clone)]
pub struct Identifier(pub String);

impl Identifier {
    pub fn new(name: String) -> Identifier {
        Identifier { 0: name }
    }
}
