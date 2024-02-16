#![allow(unused)]

use std::{
    collections::{hash_map::DefaultHasher, HashSet},
    hash::{BuildHasherDefault, Hasher},
    sync::Arc,
};

mod ast;
mod error;
mod lex;
mod str_intern;
mod tokens;

fn main() {
}
