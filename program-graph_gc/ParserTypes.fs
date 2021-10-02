
module ParserTypes

type p =
  | Program of (d * s)
and s =
  | Assign of (l * a)
  | RecordAssign of (l * a * a)
  | Stats of (s * s)
  | IfStat of (b * s)
  | IfElseStat of (b * s * s)
  | WhileStat of (b * s) 
  | Read of l
  | Write of a 
and l = 
  | Var of string
  | Array of (l * a) 
  | Fst of l
  | Snd of l
and a =
  | Num of int
  //| Var of string
  | PlusExpr of (a * a)
  | MinusExpr of (a * a)
  | TimesExpr of (a * a)
  | DivExpr of (a * a)
  | ModExpr of (a * a)
  | UPlusExpr of a
  | UMinusExpr of a
and b =
  | True
  | False
  | Bol of b
  | AndExpr of (b * b)
  | OrExpr of (b * b)
  | NegExpr of b
  | Equals of (a * a)
  | NotEquals of (a * a)
  | GrThan of (a * a)
  | GrEqThan of (a * a)
  | LeThan of (a * a)
  | LeEqThan of (a * a)
  | BolPar of b
and d =
  | VarInt of (l)
  | ArrayInt of (a * l)
  | Decl of (d * d)


//Init("int", Var "x")
//(Equals(Num 2,Num 2))
//((Program(VarInt(Var "x"),Assign(Var "x", Num 2))))
//Init("int", "x")