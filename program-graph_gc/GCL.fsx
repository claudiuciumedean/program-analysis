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

//Functions for making appropriate strings of arithmetic, boolean and statement expressions
(*
let rec expr ex =
    match ex with
    | Num(fl) -> Num fl
    | TimesExpr(ex1,ex2) -> TimesExpr(ex1,ex2)
    | DivExpr(ex1,ex2) -> DivExpr(ex1,ex2)
    | ModExpr(ex1,ex2) -> ModExpr(ex1,ex2)
    | PlusExpr(ex1,ex2) -> PlusExpr(ex1,ex2)
    | MinusExpr(ex1,ex2) -> MinusExpr(ex1,ex2)
    | UPlusExpr(ex1) -> UPlusExpr(ex1)
    | UMinusExpr(ex1) -> UMinusExpr(ex1)
    | Variable(st) -> Variable(st)
and stat s =
    match s with
    | Ass(x,a) -> (x,a)
    | ArrayAss(x,a1,a2) -> runExpr(x)+"["+runExpr(a1)+"]" + ":=" + runExpr(a2)
and runBool bl =
    match bl with
    | True -> "true"
    | False -> "false"
    | Bol bl1 -> string bl1
    | AndExpr(bl1,bl2) -> runBool bl1 + "&" + runBool bl2
    | OrExpr(bl1,bl2) -> runBool bl1 + "|" + runBool bl2
    | NegExpr bl1 -> "!(" + runBool bl1 + ")"
    | Equals(ex1,ex2) -> runExpr ex1 + "=" + runExpr ex2
    | NotEquals(ex1,ex2) -> runExpr ex1 + "!=" + runExpr ex2
    | GrThan(ex1,ex2) -> runExpr ex1 + ">" + runExpr ex2
    | GrEqThan(ex1,ex2) -> runExpr ex1 + ">=" + runExpr ex2
    | LeThan(ex1,ex2) -> runExpr ex1 + "<" +  runExpr ex2
    | LeEqThan(ex1,ex2) -> runExpr ex1 + "<=" + runExpr ex2
    | BolPar(bl1) -> "(" + runBool bl1 + ")"
and runStat s = 
    match s with 

//runExpr (Variable "x") // test

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

(*
let rec edges qI qF e = 
    match e with
    | Ass(x,a)              -> set [qI, Ass(x,a) , qF]
    | ArrayAss(x,a1,a2)     -> set [qI, (ArrayAss(x,a1,a2)), qF] // not sure how to include array assign otherwise?

    | Stats(S1,S2)          -> fresh <- fresh + 1
                               let q = fresh
                               let E1 = edges qI q S1
                               let E2 = edges q qF S2 
                               Set.union E1 E2
    | IfElseStat(b, s1, s2) -> fresh <- fresh + 1
                               let q1 = fresh             
                               let e1 = edges q1 qF s1
                               let E1 = Set.union (set [qI, b, q1]) e1

                               fresh <- fresh + 1
                               let q2 = fresh
                               let e2 = edges q2 qF s2
                               let E2 = Set.union (set [qI, (NegExpr b), q2]) e2    

                               Set.union E1 E2  

    | IfStat(b,s)           -> fresh <- fresh + 1
                               let q = fresh
                               let E = edges q qF s
                               Set.union (set [qI, b, q]) E

    | WhileStat(b,s)        -> let E = edges qI qI (IfStat(b,s))
                               Set.union E (set [qI, (NegExpr b), qF]) 
    | _ -> failwith "no matchcase for edges input"

// test: 
// edges 0 1 (IfElseStat((GrThan(Variable "x", Num 0)), (Ass(Variable "x", Num 2)),(Ass(Variable "y", Num 2)))) 
// edges "▷" "◀" (Stats((Ass(Variable "x", Num 2.0)),(Ass(Variable "y", Num 2.0))))
 *)

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
        printfn "AST:\n%A" ast

        (*let pg = (edges 0 6 ast) 
        printfn "PG:\n%A" pg
        printfn "RD:\n%A" (reachingDefinitions pg) *)
        //printGraphviz pg

        fresh <- 0
        compute n 
compute 3

//getNodes (set [(0, "y := 1", 2); (2, "!(x>0)", 1); (2, "x>0", 3); (3, "y := y*x", 4); (4, "x := x-1", 2)])