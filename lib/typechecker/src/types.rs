#[derive(Debug, Clone, PartialEq)]
pub struct DataStructType {
    pub name: String,
    pub ttype: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    String,
    Bool,
    Unknown,
    Unit,
    Array(Box<Type>),
    Function(Vec<Type>, Box<Type>),
    DataStruct(String, Vec<DataStructType>),
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::String => write!(f, "string"),
            Type::Bool => write!(f, "bool"),
            Type::Unknown => write!(f, "unknown"),
            Type::Unit => write!(f, "unit"),
            Type::Array(a) => write!(f, "{}[]", a),
            Type::Function(params, ret) => {
                let p: Vec<String> = params.into_iter().map(|e| e.to_string()).collect();
                let out = format!("Fun({}): {}", p.join(", "), ret);
                write!(f, "{}", out)
            }
            Type::DataStruct(d, _) => {
                write!(f, "{}", d)
            }
        }
    }
}

impl Type {
    pub fn is_array(&self) -> bool {
        matches!(self, Type::Array(_))
    }

    pub fn is_int(&self) -> bool {
        matches!(self, Type::Int)
    }

    pub fn is_function(&self) -> bool {
        matches!(self, Type::Function(_, _))
    }

    pub fn is_subtype(&self, ttype: Type) -> bool {
        match (self, ttype) {
            (Type::Int, Type::Int) => true,
            (Type::Float, Type::Float) => true,
            (Type::String, Type::String) => true,
            (Type::Bool, Type::Bool) => true,
            (Type::Unit, Type::Unit) => true,
            (Type::Array(a1), Type::Array(a2)) => *a1 == a2,
            (Type::Function(a1, ret1), Type::Function(a2, ret2)) => *a1 == a2 && *ret1 == ret2,
            _ => false,
        }
    }

    pub fn from_ast_type(atype: ast::Type) -> Type {
        match atype {
            ast::Type::Int => Type::Int,
            ast::Type::Float => Type::Float,
            ast::Type::String => Type::String,
            ast::Type::Bool => Type::Bool,
            ast::Type::Unit => Type::Unit,
            ast::Type::Array(t) => Type::from_ast_type(*t),
            ast::Type::Fun(params, ret) => {
                let p: Vec<Type> = params
                    .iter()
                    .map(|i| Type::from_ast_type(i.value_type.as_ref().unwrap().clone()))
                    .collect();
                let ret = Type::from_ast_type(*ret);
                Type::Function(p, Box::new(ret))
            }
            ast::Type::Identifier(_) => todo!(),
        }
    }
}
