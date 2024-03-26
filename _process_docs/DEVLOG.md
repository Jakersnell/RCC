### 2024/03/22 12:15pm - Current Phase: Semantic Analysis

I started this dev-log pretty late in the development process. Unfortunate, I am nearly
fully complete with this project. Oh well, I am glad I am doing this now. I have been working on this
project for nearly a month now, and I'm reaching the final stage, code generation.
What I have done so far is created the lexer to tokenize the input text and validate simple
things such as strings being closed, items being keywords, operators, etc. Then I made the parser, to parse and
analyze the syntactic nature of the source code, this means verifying which keywords can go where, and what they mean,
and order of operations for binary operations. Then there is the stage that I am finishing right now, semantic analysis.
This stage involves checking that variables exist, type checking, casting, verifying that functions and structs exist
and
their parameters, and more. I just finished up casting right now. I am planning on finishing control flow analysis next.
After that I want to work on constant folding, which is where you take things like `4 + 6 / 2` and you evaluate it so
that the operation does not need to be done during runtime. I am very excited about the progress being made in this
project, and I am pleased with the work I have done thus far. If all goes well I should be able to run basic programs
like fizzbuzz next week. 