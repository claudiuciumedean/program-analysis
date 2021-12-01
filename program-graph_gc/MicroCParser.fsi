// Signature file for parser generated by fsyacc
module MicroCParser
type token = 
  | ELSE
  | ASS
  | SEMI
  | IF
  | WHILE
  | INT
  | FIRST
  | SECOND
  | READ
  | WRITE
  | DOT
  | COMMA
  | OR
  | NEG
  | EQ
  | NEQ
  | GT
  | GET
  | LT
  | LET
  | TRUE
  | FALSE
  | TIMES
  | DIV
  | MOD
  | PLUS
  | MINUS
  | LPAR
  | RPAR
  | LCB
  | RCB
  | LSB
  | RSB
  | AND
  | VAR of (string)
  | BOL
  | NUM of (int)
type tokenId = 
    | TOKEN_ELSE
    | TOKEN_ASS
    | TOKEN_SEMI
    | TOKEN_IF
    | TOKEN_WHILE
    | TOKEN_INT
    | TOKEN_FIRST
    | TOKEN_SECOND
    | TOKEN_READ
    | TOKEN_WRITE
    | TOKEN_DOT
    | TOKEN_COMMA
    | TOKEN_OR
    | TOKEN_NEG
    | TOKEN_EQ
    | TOKEN_NEQ
    | TOKEN_GT
    | TOKEN_GET
    | TOKEN_LT
    | TOKEN_LET
    | TOKEN_TRUE
    | TOKEN_FALSE
    | TOKEN_TIMES
    | TOKEN_DIV
    | TOKEN_MOD
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_LPAR
    | TOKEN_RPAR
    | TOKEN_LCB
    | TOKEN_RCB
    | TOKEN_LSB
    | TOKEN_RSB
    | TOKEN_AND
    | TOKEN_VAR
    | TOKEN_BOL
    | TOKEN_NUM
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_decl
    | NONTERM_expra
    | NONTERM_exprl
    | NONTERM_exprb
    | NONTERM_stat
    | NONTERM_program
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val start : (FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> FSharp.Text.Lexing.LexBuffer<'cty> -> (program) 
