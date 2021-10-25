
module ParserTypes

type decl = 
  | VariableDeclaration of string
  | ArrayDeclaration of (int * string)
  | RecordDeclaration of string 
  | Epsilon
  | Declarations of (decl * decl)

and expra = 
  | Num of int
  | VariableA of string
  | ArrayExpressionA of (string * expra)
  | FirstRecordA of string
  | SecondRecordA of string
  | PlusExpr of (expra * expra)
  | MinusExpr of (expra * expra)
  | TimesExpr of (expra * expra)  
  | DivExpr of (expra * expra)
  | ModExpr of (expra * expra)

and exprl = 
  | VariableL of string
  | ArrayExpressionL of (string * expra)
  | FirstRecordL of string
  | SecondRecordL of string

and exprb =
  | True
  | False
  | LeThan of (expra * expra)
  | GrThan of (expra * expra)
  | LeEqThan of (expra * expra)
  | GrEqThan of (expra * expra)
  | Equals of (expra * expra)
  | NotEquals of (expra * expra)
  | AndExpr of (exprb * exprb)
  | OrExpr of (exprb * exprb)
  | NegExpr of exprb

and stat =
  | AssX of (string * expra)
  | Ass of (exprl * expra)
  | RecordAss of (string * expra * expra)
  | Stats of (stat * stat)
  | IfStat of (exprb * stat)
  | IfElseStat of (exprb * stat * stat)
  | WhileStat of (exprb * stat) 
  | Read of exprl
  | Write of expra

and program = 
  | Program of (decl * stat)
