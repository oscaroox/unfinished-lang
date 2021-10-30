#[derive(Debug, PartialEq, Clone)]
pub struct Identifier(pub String);

impl Identifier {
    pub fn new(name: String) -> Identifier {
        Identifier { 0: name }
    }

    pub fn value(&self) -> String {
        self.0.to_string()
    }
}
