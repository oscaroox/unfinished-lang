use std::{cell::RefCell, rc::Rc};

use crate::env::Env;

#[derive(Debug, Clone, PartialEq)]
pub struct DataStructField {
    pub name: String,
    pub ttype: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DataStructMethod {
    pub name: String,
    pub ttype: Type,
    pub is_static: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DataStruct {
    pub name: String,
    pub fields: Vec<DataStructField>,
    pub methods: Vec<DataStructMethod>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Singleton(Singleton),
    Unknown,
    Unit,
    Array(Box<Type>),
    Identifier(Box<Type>),
    Function(Vec<Type>, Box<Type>),
    DataStruct(DataStruct),
    DataStructInstance(DataStruct),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Singleton {
    Int,
    Float,
    String,
    Bool,
}

impl std::fmt::Display for Singleton {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Singleton::Int => write!(f, "int"),
            Singleton::Float => write!(f, "float"),
            Singleton::String => write!(f, "string"),
            Singleton::Bool => write!(f, "bool"),
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Singleton(lit) => write!(f, "{}", lit),
            Type::Unknown => write!(f, "unknown"),
            Type::Unit => write!(f, "unit"),
            Type::Array(t) => write!(f, "{}", t),
            Type::Function(params, ret) => {
                let p: Vec<String> = params.into_iter().map(|e| e.to_string()).collect();
                let out = format!("Fun({}): {}", p.join(", "), ret);
                write!(f, "{}", out)
            }
            Type::DataStruct(d) => {
                write!(f, "{}", d.name)
            }
            Type::DataStructInstance(t) => write!(f, "{}", t.name),
            Type::Identifier(i) => write!(f, "{}", i),
        }
    }
}

macro_rules! impl_type {
    ($_meth:ident, $tok:ident) => {
        pub fn $_meth() -> Type {
            Type::Singleton(Singleton::$tok)
        }
    };
}
macro_rules! impl_is_literal_type {
    ($_meth:ident, $_en:ident) => {
        pub fn $_meth(&self) -> bool {
            matches!(self, Type::Singleton(Singleton::$_en))
        }
    };
}

impl Type {
    impl_type!(int, Int);
    impl_type!(float, Float);
    impl_type!(string, String);
    impl_type!(bool, Bool);

    pub fn array(t: Type) -> Type {
        Type::Array(Box::new(t))
    }

    pub fn data_struct(
        name: String,
        fields: Vec<DataStructField>,
        methods: Vec<DataStructMethod>,
    ) -> Type {
        Type::DataStruct(DataStruct {
            fields,
            methods,
            name,
        })
    }

    impl_is_literal_type!(is_int, Int);
    impl_is_literal_type!(is_float, Float);
    impl_is_literal_type!(is_string, String);
    impl_is_literal_type!(is_bool, Bool);

    pub fn is_literal(&self) -> bool {
        matches!(self, Type::Singleton(_))
    }

    pub fn is_array(&self) -> bool {
        matches!(self, Type::Array(_))
    }

    pub fn is_array_unknown(&self) -> bool {
        match self {
            Type::Array(a) => **a == Type::Unknown,
            _ => false,
        }
    }

    pub fn is_function(&self) -> bool {
        matches!(self, Type::Function(_, _))
    }

    pub fn is_data_struct_instance(&self) -> bool {
        matches!(self, Type::DataStructInstance(_))
    }

    pub fn is_subtype(&self, ttype: Type) -> bool {
        match (self, ttype) {
            (Type::Singleton(Singleton::Int), Type::Singleton(Singleton::Int)) => true,
            (Type::Singleton(Singleton::Float), Type::Singleton(Singleton::Float)) => true,
            (Type::Singleton(Singleton::String), Type::Singleton(Singleton::String)) => true,
            (Type::Singleton(Singleton::Bool), Type::Singleton(Singleton::Bool)) => true,
            (Type::Unit, Type::Unit) => true,
            (Type::Array(a1), Type::Array(a2)) => *a1 == a2,
            (Type::Function(a1, ret1), Type::Function(a2, ret2)) => *a1 == a2 && *ret1 == ret2,
            (Type::Identifier(i1), Type::Identifier(i2)) => *i1 == i2,
            (Type::DataStruct(d1), Type::DataStruct(d2))
            | (Type::DataStructInstance(d1), Type::DataStructInstance(d2)) => {
                d1.name == d2.name && d1.fields == d2.fields
            }
            _ => false,
        }
    }

    pub fn from_ast_type(atype: ast::Type, env: &Rc<RefCell<Env>>) -> Type {
        match atype {
            ast::Type::Int => Type::Singleton(Singleton::Int),
            ast::Type::Float => Type::Singleton(Singleton::Float),
            ast::Type::String => Type::Singleton(Singleton::String),
            ast::Type::Bool => Type::Singleton(Singleton::Bool),
            ast::Type::Unit => Type::Unit,
            ast::Type::Array(t) => Type::from_ast_type(*t, env),
            ast::Type::Fun(params, ret) => {
                let p: Vec<Type> = params
                    .iter()
                    .map(|i| Type::from_ast_type(i.value_type.as_ref().unwrap().clone(), env))
                    .collect();
                let ret = Type::from_ast_type(*ret, env);
                Type::Function(p, Box::new(ret))
            }
            ast::Type::Identifier(s) => match env.borrow_mut().get(&s) {
                Some(t) => match &t {
                    // Type::DataStruct(_, _, _) => Type::DataStructInstance(Box::new(t)),
                    Type::DataStruct(d) => Type::DataStructInstance(d.clone()),
                    _ => t,
                },
                None => panic!("Undefined type {}", s),
            },
        }
    }
}
