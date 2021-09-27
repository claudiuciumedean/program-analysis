
module ParserTypes

//data type "expression" representing "a" in the given the GCL language
type expr =
  | Num of int
  | TimesExpr of (expr * expr)
  | DivExpr of (expr * expr)
  | ModExpr of (expr * expr)
  | PlusExpr of (expr * expr)
  | MinusExpr of (expr * expr)
  | UPlusExpr of (expr)
  | UMinusExpr of (expr)
  | Variable of string
// data type "boolean" representing "b" in the given the GCL language
//and boolean =  
  | True
  | False
  | Bol of expr
  | AndExpr of (expr * expr)
  | OrExpr of (expr * expr)
  | NegExpr of expr
  | Equals of (expr * expr)
  | NotEquals of (expr * expr)
  | GrThan of (expr * expr)
  | GrEqThan of (expr * expr)
  | LeThan of (expr * expr)
  | LeEqThan of (expr * expr)
  | BolPar of expr
// data type "C" representing its namesake in the given the GCL language
//and statement =
  | Ass of (expr * expr)
  | ArrayAss of (expr * expr * expr)
  | Skip
  | Stats of (expr * expr)
  | IfElseStat of (expr * expr * expr)
  | IfStat of (expr * expr)
  | WhileStat of (expr * expr) 
