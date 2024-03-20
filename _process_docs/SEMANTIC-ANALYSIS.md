# Semantic Analysis: Current phase in progress

Semantic analysis is the phase in which the nuanced or more logically complex rules of the language are analyzed and
enforced.
These rules involve elements that may be syntactically correct, but are not correct within the language. This phase also
includes
processing the abstract syntax tree (AST) into a more complete intermediate representation,
the high level intermediate representation (HLIR). This structure holds more information that will be necessary in the
codegen phase,
removes certain high level elements and replaces them with structurally correct representation. For example, sizeof,
during analysis sizeof is replaced with a literal integer value of the type it was evaluating. Other examples include
converting for loops into while loops, converting casts into a proper cast structure, and more. These elements are the
guts of the language, and make it function properly.

### Elements verified or processed during this step.

    - Symbol resolving: Verifying variables/identifiers exist and hold data of the proper type.
    - Casting: Validating and structuring explicit and implicit casting.
    - Type checking: Verfifying types are valid for the operations being done on them, i.e assignment, comparison between types, etc.
    - Control flow analysis: Analysis of control flow, and verifying functions have valid return statements.
    
