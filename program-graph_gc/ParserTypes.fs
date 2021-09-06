
module ParserTypes

//data type "expression" representing "a" in the given the GCL language
type expression =
  | Num of float
  | TimesExpr of (expression * expression)
  | DivExpr of (expression * expression)
  | PlusExpr of (expression * expression)
  | MinusExpr of (expression * expression)
  | PowExpr of (expression * expression)
  | UPlusExpr of (expression)
  | UMinusExpr of (expression)
  | Variable of string

// data type "boolean" representing "b" in the given the GCL language
and boolean =  
  | True
  | False
  | Bol of bool
  | AndExpr of (boolean * boolean)
  | OrExpr of (boolean * boolean)
  | ShortAndExpr of (boolean * boolean)
  | ShortOrExpr of (boolean * boolean)
  | NegExpr of boolean
  | Equals of (expression * expression)
  | NotEquals of (expression * expression)
  | GrThan of (expression * expression)
  | GrEqThan of (expression * expression)
  | LeThan of (expression * expression)
  | LeEqThan of (expression * expression)
  | BolPar of boolean
// data type "C" representing its namesake in the given the GCL language
and C =
    | Ass of (string * expression)
    | Skip
    | Semi of (C * C)
    | IfStat of (GC)
    | DoStat of (GC)
// data type "GC" representing its namesake in the given the GCL language
and GC =
    | Func of (boolean * C)
    | ElseStat of (GC * GC)