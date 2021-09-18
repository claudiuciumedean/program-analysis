//path to run fsLexYacc
//should be changed depending on which system the script is run
// Windows (Stina)
#r "FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"
// for mac maybe?
//#r "C:/Users/pc/.nuget/packages/fslexyacc.runtime/7.0.6/lib/portable-net45+netcore45+wpa81+wp8+MonoAndroid10+MonoTouch10/FsLexYacc.Runtime.dll"

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
let rec runExpr ex =
    match ex with
    | Num(fl) -> string fl
    | TimesExpr(ex1,ex2) -> (runExpr ex1) + "*" + (runExpr ex2)
    | DivExpr(ex1,ex2) -> (runExpr ex1) + "/" + (runExpr ex2)
    | ModExpr(ex1,ex2) -> (runExpr ex1) + "%" + (runExpr ex2)
    | PlusExpr(ex1,ex2) -> (runExpr ex1) + "+" + (runExpr ex2)
    | MinusExpr(ex1,ex2) -> (runExpr ex1) + "-" + (runExpr ex2)
    | UPlusExpr(ex1) -> "+" + runExpr ex1
    | UMinusExpr(ex1) -> "-" + runExpr ex1
    | Variable(st) -> st
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
    | Ass(x,a) -> runExpr(x) + ":=" + runExpr(a)
    | ArrayAss(x,a1,a2) -> runExpr(x)+"["+runExpr(a1)+"]" + ":=" + runExpr(a2)

//runExpr (Variable "x") // test

// prints the grafical string representation of a (set) pg
let rec printSet s = Set.iter (fun (qI,st,qF) -> printfn "q%s -> q%s [label = \"%s\"];" qI qF st) s
let printGraphviz s = printfn "\nGraphviz:"
                      printfn  "digraph program_graph {rankdir=LR; \nnode [shape = circle]; q▷; \nnode [shape = doublecircle]; q◀; \nnode [shape = circle] "
                      printSet s
                      printfn "}"

(*let rec isDone e = 
    match e with 
    | WhileStat(b,s) -> (NegExpr b)
    //| ElseIfExpr(GC1,GC2) -> (AndExpr (isDone GC1, isDone GC2))
    | _ -> failwith "no matchcase for input isDone"
*)
let rec edges qI qF e = 
    match e with
    | Ass(x,a)              -> set [qI, runStat (Ass(x,a)) , qF]
    | ArrayAss(x,a1,a2)     -> set [qI, runStat(ArrayAss(x,a1,a2)), qF] // not sure how to include array assign otherwise?

    | Stats(S1,S2)          -> fresh <- fresh + 1
                               let q = string fresh
                               let E1 = edges qI q S1
                               let E2 = edges q qF S2 
                               Set.union E1 E2
    | IfElseStat(b, s1, s2) -> fresh <- fresh + 1
                               let q1 = string fresh             
                               let e1 = edges q1 qF s1
                               let E1 = Set.union (set [qI, runBool b, q1]) e1

                               fresh <- fresh + 1
                               let q2 = string fresh
                               let e2 = edges q2 qF s2
                               let E2 = Set.union (set [qI, runBool (NegExpr b), q2]) e2    

                               Set.union E1 E2  

    | IfStat(b,s)           -> fresh <- fresh + 1
                               let q = string fresh
                               let E = edges q qF s
                               Set.union (set [qI, runBool b, q]) E

    | WhileStat(b,s)        -> let E = edges qI qI (IfStat(b,s))
                               Set.union E (set [qI, runBool (NegExpr b), qF]) 
    | _ -> failwith "no matchcase for edges input"

// test: 
// edges "▷" "◀" (IfElseStat((GrThan(Variable "x", Num 0.0)), (Ass(Variable "x", Num 2.0)),(Ass(Variable "y", Num 2.0)))) 
// edges "▷" "◀" (Stats((Ass(Variable "x", Num 2.0)),(Ass(Variable "y", Num 2.0))))
 

// not done !!!
let rec fva a =
    match a with
    | Variable(x)            -> Set [x]
    | Num(n)                 -> Set.empty
    //| Array(a1,a2)           -> 
    | PlusExpr(a1,a2)         
    | MinusExpr(a1,a2)        
    | ModExpr(a1,a2) 
    | TimesExpr(a1,a2)       
    | DivExpr(a1,a2)         -> Set.union (fva a1) (fva a2)  
    | UMinusExpr(a0)         -> fva a0
and fvb b =    
    match b with
    | True 
    | False                  -> Set.empty
    | AndExpr(b1,b2)          
    | OrExpr(b1,b2)          -> Set.union (fvb b1) (fvb b2) 
    | NegExpr(b0)            -> fvb b0
    | Equals(a1,a2)       
    | NotEquals(a1,a2)     
    | GrThan(a1,a2)       
    | GrEqThan(a1,a2)  
    | LeThan(a1,a2)         
    | LeEqThan(a1,a2)        -> Set.union (fva a1) (fva a2)   
    |_ -> failwith "error in fv"

//fvb (LeThan (Variable "y", Variable "x"))

// not done see p 27
let killLV e =
    match e with 
    | Ass(x,a) -> fva x 
    |_ -> Set.empty

// not done see p 27
let rec genLV e =
    match e with 
    | Ass(x,a) -> fva a 
and genLVb e = 
    match e with
    | OrExpr(a1,a2) -> fvb (OrExpr(a1,a2))

    //| _ -> Set.empty
 

killLV (Ass (Variable "r", MinusExpr (Variable "r", Variable "y")))
genLV (Ass (Variable "r", MinusExpr (Variable "r", Variable "y")))

genLVb  (OrExpr (True, True))



//function that takes input from user and prints corresponding graphviz file if the given string has valid GCL syntax
// and gets an error otherwise
let rec compute n =
    if n = 0 then
        printfn "Bye bye"
    else
        try
        printfn "Enter a GCL code: "
        // parse the input string (program)
        let ast = parse (Console.ReadLine())
        printfn "AST:\n%A" ast

        let pg = (edges "▷" "◀" ast)
        printfn "PG:\n%A" pg
        printGraphviz pg

        fresh <- 0
        compute n 
        with err -> printfn "Input is not a valid MicroC program.\n"
                    compute (n-1) 
compute 3

