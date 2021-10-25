//path to run fsLexYacc
//should be changed depending on which system the script is run
// Windows (Stina)
//#r "FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"
// Julien 
#r "/Users/Julien/F#/FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"

// import of modules, including lexer and parser
open FSharp.Text.Lexing
open System
#load "ParserTypes.fs"
open ParserTypes
#load "GCLParser.fs"
open GCLParser
#load "GCLLexer.fs"
open GCLLexer

let mutable fresh = 0

// Parser function for user input string, accepting only GCL valid syntax
let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = GCLParser.start GCLLexer.tokenize lexbuf
    // return the result of parsing
    res

(*
// prints the grafical string representation of a (set) pg
let rec printSet s = Set.iter (fun (qI,st,qF) -> printfn "q%d -> q%d [label = \"%s\"];" qI qF st) s
let printGraphviz s = printfn "\nGraphviz:"
                      printfn  "digraph program_graph {rankdir=LR; \nnode [shape = circle]; q0; \nnode [shape = doublecircle]; q00; \nnode [shape = circle] "
                      printSet s
                      printfn "}"
*)

(*let rec isDone e = 
    match e with 
    | WhileStat(b,s) -> (NegExpr b)
    //| ElseIfExpr(GC1,GC2) -> (AndExpr (isDone GC1, isDone GC2))
    | _ -> failwith "no matchcase for input isDone"
*)

type Action = 
    | Program of program
    | Declaration of decl
    | ExprA of expra
    | ExprL of exprl
    | ExprB of exprb
    | Statement of stat

let rec edges qI qF action = 
    match action with
    | Program(p) -> edgesP qI qF p
    | Declaration(d) -> edgesD qI qF d
    | Statement(s) -> edgesS qI qF s
    | _ -> Set.empty

and edgesP qI qF p = 
    match p with 
    | Prog(d,s) -> fresh <- fresh + 1
                   let q = fresh
                   let e1 = edgesD qI q d
                   let e2 = edgesS q qF s
                   Set.union e1 e2

and edgesD qI qF d =
    match d with 
    | VariableDeclaration(l)  -> set [qI, Declaration(d), qF]
    | ArrayDeclaration(a,l)   -> set [qI, Declaration(d), qF]
    | RecordDeclaration(r)    -> set [qI, Declaration(d), qF]
    | Declarations(d1,d2)     -> fresh <- fresh + 1
                                 let q = fresh
                                 let e1 = edgesD qI q d1
                                 let e2 = edgesD q qF d2 
                                 Set.union e1 e2
    | Epsilon -> Set.empty

and edgesS qI qF s =
    match s with
    | AssX(l, a)            -> set [qI, Statement(s), qF]
    | Ass(l,a)              -> set [qI, Statement(s), qF]
    | RecordAss(x,a1,a2)    -> set [qI, Statement(s), qF] 
    | Stats(s1,s2)          -> fresh <- fresh + 1
                               let q = fresh
                               let e1 = edgesS qI q s1
                               let e2 = edgesS q qF s2 
                               Set.union e1 e2
    
    | IfStat(b,s0)          -> fresh <- fresh + 1
                               let q = fresh
                               let e = edgesS q qF s0
                               Set.union (set [qI, ExprB(b), q]) e
    
    | IfElseStat(b, s1, s2) -> fresh <- fresh + 1
                               let q1 = fresh             
                               let e = edgesS q1 qF s1
                               let E1 = Set.union (set [qI, ExprB(b), q1]) e

                               fresh <- fresh + 1
                               let q2 = fresh
                               let e = edgesS q2 qF s2
                               let E2 = Set.union (set [qI, ExprB(NegExpr b), q2]) e  
                               Set.union E1 E2  

    | WhileStat(b,s0)       -> fresh <- fresh + 1
                               let q = fresh
                               let e = edgesS q qI s0 
                               Set.union e (set [(qI, ExprB(b), q); (qI, ExprB(NegExpr b), qF)])
    | Read(l)               -> set [qI, Statement (s), qF]
    | Write(x)              -> set [qI, Statement (s), qF]

// test: 
 //edges 0 1 (IfElseStat((GrThan(Var "x", Num 0)), (Assign(Var "x", Num 2)),(Assign(Var "y", Num 2)))) 
// edges "▷" "◀" (Stats((Ass(Variable "x", Num 2.0)),(Ass(Variable "y", Num 2.0))))

 
(*
// not done !!!
(*
let rec fv a =
    match a with
    | Variable(x)            -> Set [x]
    | Num(n)                 -> Set.empty
    //| Array(a1,a2)           -> 
    | PlusExpr(a1,a2)         
    | MinusExpr(a1,a2)        
    | ModExpr(a1,a2) 
    | TimesExpr(a1,a2)       
    | DivExpr(a1,a2)         -> Set.union (fv a1) (fv a2)  
    | UMinusExpr(a0)         -> fv a0
    | True 
    | False                  -> Set.empty
    | AndExpr(b1,b2)          
    | OrExpr(b1,b2)          -> Set.union (fv b1) (fv b2) 
    | NegExpr(b0)            -> fv b0
    | Equals(a1,a2)       
    | NotEquals(a1,a2)     
    | GrThan(a1,a2)       
    | GrEqThan(a1,a2)  
    | LeThan(a1,a2)         
    | LeEqThan(a1,a2)        -> Set.union (fv a1) (fv a2)   
    |_ -> failwith "error in fv"

//fvb (LeThan (Variable "y", Variable "x"))

// not done see p 27
let rec killLV e =
    match e with 
    | Ass(x,a) -> fv x 
    |_ -> Set.empty

// not done see p 27
let rec genLV e =
    match e with 
    | Ass(x,a) -> fv a 
    | e -> fv e

    //| _ -> Set.empty
 
let e1 = Ass (Variable "x", Num 2)
let e2 = NegExpr (GrThan (Variable "x", Num 0))
let e3 = GrThan (Variable "x", Num 0)
let e4 = Ass (Variable "x", MinusExpr (Variable "x", Num 1))

killLV (Ass (Variable "r", MinusExpr (Variable "r", Variable "y")))
genLV (Ass (Variable "r", MinusExpr (Variable "r", Variable "y")))

killLV e3
genLV e3
*)

// TODO - Create this function to get variable in assignments
let def alpha = 
    match alpha with 
    | Ass(Variable(x),_) -> x
    | _ -> ""

let getNodes edgesSet = 
    let mutable nodes = Set.empty
    for (q1, _, q2) in Set.toList edgesSet do
        nodes <- nodes.Add(q1).Add(q2)
    Set.toList nodes

let getVariables edges = 
    let mutable variables = Set.empty
    for (_, x , _) in edges do
        variables <- variables.Add(def x)
    Set.toList variables

let reachingDefinitions edges = 
    let nodes = getNodes edges
    let variables = getVariables edges
    let rd = Array.create (nodes.Length) (Set.empty)
    for variable in variables do 
        if (variable <> "") then 
            rd.[0] <- rd.[0].Add(variable, -1, 0)
    let mutable over = false
    while not over do 
        let mutable newRd = false
        for (q1, alpha, q2) in (Set.toList edges) do
            let variable =  def alpha
            let mutable kill = Set.empty
            let mutable gen = Set.empty  
            if (variable <> "") then 
                gen <- gen.Add(variable, q1, q2)
            for q in (List.append nodes [-1]) do
                for q' in nodes do
                    kill <- kill.Add(variable, q, q') 
            if not (Set.isSubset (Set.union (Set.difference rd.[q1] kill) gen) rd.[q2]) then
                newRd <- true
                rd.[q2] <- (Set.union rd.[q2] (Set.union (Set.difference rd.[q1] kill) gen))
        if not newRd then 
            over <- true
    rd

//y:= 1; x:=2;  while (x<=100) { if (y <10) { y := y +1; } else {x := x +10;}}
//printfn "%A" (reachingDefinitions (set [(0, "y", 1); (1, "x", 2); (2, "", 3); (3, "", 4); (4, "y", 2); (3, "", 5); (5, "x", 2); (2, "", 6)]))
*)

//function that takes input from user and prints corresponding graphviz file if the given string has valid GCL syntax
// and gets an error otherwise
let rec compute n =
    if n = 0 then
        printfn "Bye bye"
    else
        printfn "Enter a MicroC code: "
        // parse the input string (program)
        let ast = parse (Console.ReadLine())
        printfn "HELOOO"
        printfn "AST:\n%A" ast

        let pg = (edges 0 6 (Program ast))
        printfn "PG:\n%A" pg
        //printfn "RD:\n%A" (reachingDefinitions pg)
        //let pg = (edges 0 -1 ast) 
        //printfn "PG:\n%A" pg
        //printGraphviz pg

        //fresh <- 0
        compute n 
compute 3

//getNodes (set [(0, "y := 1", 2); (2, "!(x>0)", 1); (2, "x>0", 3); (3, "y := y*x", 4); (4, "x := x-1", 2)])

// {int x; int[3] A; {int fst; int snd} R; while (not x == 3) {x := 3 + 5;}}