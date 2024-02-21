use lazy_static::lazy_static;
use std::{
    collections::hash_map::DefaultHasher,
    hash::{BuildHasher, BuildHasherDefault, Hasher},
    path::Iter,
    process::ExitStatus,
    string,
    sync::{Arc, Mutex},
};

// This is a simple implementation of a string interner.
// You might be asking why I implemented one myself rather than using a crate like lasso.
// The answer is, I wanted to learn how to implement a string interner myself, and I had a few ideas I wanted to test out.
// I don't know if this is the best implementation, it was more just a creative exercise for me.

pub type InternedStr = Arc<String>;

lazy_static! {
    static ref INTERNER: Mutex<Interner<BuildHasherDefault<DefaultHasher>>> =
        Mutex::new(Interner::default());
}

pub fn intern<S: AsRef<str>>(string: S) -> Arc<String> {
    INTERNER.lock().unwrap().intern(string)
}

pub fn get<S: AsRef<str>>(string: S) -> Option<Arc<String>> {
    INTERNER.lock().unwrap().get(string)
}

const DEFAULT_INIT_SIZE: usize = 16;
const DEFAULT_LOAD_FACTOR: f32 = 0.75;
const DEFAULT_SHRINK_FACTOR: f32 = 0.35;

struct Entry {
    hash: usize,
    value: Arc<String>,
    next: Option<Box<Entry>>,
}

impl Entry {
    fn insert_at_last(&mut self, entry: Box<Entry>) {
        match &mut self.next {
            Some(next_entry) => next_entry.insert_at_last(entry),
            None => self.next = Some(entry),
        }
    }
}

// really just a hashmap with some custom implementation that I couldn't do with the standard hashmap
// for O(1) lookups and O(1) insertions, and learning purposes
struct Interner<H: BuildHasher> {
    hash_builder: H,
    table: Box<[Option<Box<Entry>>]>,
    size: usize,
    num_entries: usize,
    load_factor: f32,
    shrink_factor: f32,
}

impl Default for Interner<BuildHasherDefault<DefaultHasher>> {
    fn default() -> Self {
        Self {
            hash_builder: BuildHasherDefault::<DefaultHasher>::default(),
            table: (0..DEFAULT_INIT_SIZE).map(|_| None).collect(),
            size: DEFAULT_INIT_SIZE,
            num_entries: 0,
            load_factor: DEFAULT_LOAD_FACTOR,
            shrink_factor: DEFAULT_SHRINK_FACTOR,
        }
    }
}

impl<H: BuildHasher> Interner<H> {
    fn hash(&self, string: &str) -> u64 {
        let mut hasher = self.hash_builder.build_hasher();
        hasher.write(string.as_bytes());
        hasher.finish()
    }

    fn maybe_realloc(&mut self) {
        if ((self.size as f32 * self.load_factor) as usize) <= self.num_entries {
            self.size *= 2;
        } else {
            return;
        }
        self.realloc();
    }

    fn realloc(&mut self) {
        let new_table = (0..self.size).map(|_| None).collect();
        let mut table = std::mem::replace(&mut self.table, new_table);
        table.iter_mut().for_each(|root_entry| {
            let mut root_entry = std::mem::take(root_entry);
            while root_entry.is_some() {
                let mut entry = root_entry.unwrap();
                root_entry = std::mem::take(&mut entry.next);
                self.insert_entry(entry);
            }
        });
    }

    fn insert_entry(&mut self, entry: Box<Entry>) {
        let index = (entry.hash % self.size);
        match &mut self.table[index] {
            Some(table_entry) => table_entry.insert_at_last(entry),
            None => self.table[index] = Some(entry),
        }
    }

    #[allow(clippy::borrowed_box)]
    fn get_entry(&self, string: &str) -> Option<&Box<Entry>> {
        let hash = self.hash(string) as usize;
        let index = hash % self.size;
        let mut found_entry = None;
        let mut entry = &self.table[index];
        while entry.is_some() {
            let current_entry = entry.as_ref().unwrap();
            if current_entry.hash == hash && *current_entry.value == string {
                found_entry = Some(current_entry);
                break;
            } else {
                entry = &current_entry.next;
            }
        }
        found_entry
    }

    pub fn get<S: AsRef<str>>(&self, string: S) -> Option<Arc<String>> {
        self.get_entry(string.as_ref())
            .map(|entry| entry.value.clone())
    }

    pub fn intern<S: AsRef<str>>(&mut self, string: S) -> Arc<String> {
        let string = string.as_ref();
        self.get(string).unwrap_or_else(|| {
            let hash = self.hash(string) as usize;
            let arc_str = Arc::new(string.to_string());
            let entry = Box::new(Entry {
                hash,
                value: arc_str.clone(),
                next: None,
            });
            self.num_entries += 1;
            self.maybe_realloc();
            self.insert_entry(entry);
            arc_str
        })
    }
}

#[test]
fn test_interner_string_can_be_allocated_and_retrieved() {
    let string = "hello";
    let mut interner = Interner::default();
    interner.intern(string);
    assert!(interner.get(string).is_some());
}

#[test]
fn test_interning_the_same_string_doesnt_duplicate() {
    let string = "hello";
    let mut interner = Interner::default();
    interner.intern(string);
    interner.intern(string);
    interner.intern(string);
    assert_eq!(interner.num_entries, 1);
}

#[test]
fn test_reallocation_is_correct() {
    let mut interner = Interner::default();
    for i in 0..11 {
        let string = format!("hello world {}", i);
        interner.intern(string);
    }
    assert_eq!(interner.size, 16);
    interner.intern("one more string");
    assert_eq!(interner.size, 32);
}
