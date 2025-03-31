# Mini-Ocaml-Interpreter
A mini Interpreter of Ocaml , highly inefficient , just for Eductional Purposes

todo: 
Lexer / Tokenizer
Parser
Check for free Vars
Type Checking / Guessing
Evaluation

Workflow:

It goes through every layer as follows:

Lexical Syntax: A provided string (assuming it's a mini-ocaml script) will first of all be converted to a list of so-called tokens (read more about tokenizer/lexer)
↓
Phrasal Syntax: As a next step the token list will be interpreted as syntax tree, called parsing (what is a parser?)
↓
Static Semantics: checkStr then does something, what is called algorithmic reading, what will check the type for the provided script and ofc for free variables.
↓
Dynamic Semantics: evalStr will now take the lexed and parsed syntax tree and execute it in its logical order. After that, you will finally get a value as result.
