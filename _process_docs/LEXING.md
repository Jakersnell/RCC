# Lexical Analysis

The lexer is a large system that creates tokens from source text.

### Tokens

Tokens are the core of the lexer. They represent bite sized chunks of usable data from the source text, analogous to
words in a human language. The lexers job is to read a source text file and output just the readable 'words' or tokens.
These tokens each have metadata of what type they are (keyword, symbol, identifier, literal), and also contain their
usable information (i.e. a Literal::string contains a string).

### Keyword identification / identifier

Keywords are words reserved by the language, identifiers are words placed by the user. For the compiler to read a
keyword or identifier a
state machine is used to read the next available valid identifier like text from the
input stream (i.e. must start with a _ or an ascii letter and can contain only the following characters a-z A-Z _ 0-9).
The result is then compared against the list of valid keywords in the language, if it matches, it is treated as a
keyword, else it is an identifier.

### Literals

Literals are pieces of data that the program will use. At this phase they are just text data that must be analyzed,
tagged, and stored for future use by the compiler. The types of literal data at this phase are characters, strings,
integers, and floating numbers. There is no distinction between different types of Ints and Floats in this phase.

### Symbols

Symbols at the lexing phase are operational symbols such as (+-/*;() and more). The full extent of symbols can be viewed
in symbols.rs. The process in which the symbols are tokenized is done by a large match statement to cover all patterns
of input.

### Trivial

Trivial characters are pieces of text data in the source code that can be ignored by the compiler all together. This
includes all whitespace and comments. Trivial data is removed using a state machine.
