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

// Parser function for user input string, accepting only GCL valid syntax
let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = GCLParser.start GCLLexer.tokenize lexbuf
    // return the result of parsing
    res

//label function used to create strings compatible with the graphviz format
//let rec edges start stop label = start + " -> " + stop + " [label = \"" + label + "\"];\n"

let mutable fresh = 0
let mutable flag = "" //untill I know what to do with it..



//Functions for making appropriate strings of arithmetic and boolean expressions
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
    | Ass(x,a) -> runExpr(x) + " := " + runExpr(a)

//runExpr (Variable "x")


let rec isDone e =
    match e with 
    | WhileStat(b,s) -> (NegExpr b)
    //| ElseIfExpr(GC1,GC2) -> (AndExpr (isDone GC1, isDone GC2))
    | _ -> failwith "no matchcase for input isDone"

let rec edges qI qF e = 

    match e with
    | Ass(x,a)       -> set [qI, runStat (Ass(x,a)) , qF]
    | Stats(S1,S2)   -> fresh <- fresh + 1
                        let q = fresh
                        let E1 = edges qI q S1
                        let E2 = edges q qF S2 
                        Set.union E1 E2
    | IfElseStat(b, s1, s2) -> fresh <- fresh + 1
                               let q1 = fresh
                          
                               let e1 = edges q1 qF s1
                               let E1 = Set.union (set [qI, runBool b, q1]) e1

                               fresh <- fresh + 1
                               let q2 = fresh
                               let e2 = edges q2 qF s2
                               let E2 = Set.union (set [qI, runBool (NegExpr b), q2]) e2    

                               Set.union E1 E2  

    | IfStat(b,s)           -> fresh <- fresh + 1
                               let q = fresh
                               let E = edges q qF s
                               Set.union (set [qI, runBool b, q]) E

    | WhileStat(b,s)        -> let negb = isDone (WhileStat(b,s))
                               let E = edges qI qI (IfStat(b,s))
                               Set.union E (set [qI, runBool negb, qF])
    | _ -> failwith "no matchcase for input edges"

//edges 8 9 (IfElseStat((GrThan(Variable "x", Num 0.0)), (Ass(Variable "x", Num 2.0)),(Ass(Variable "y", Num 2.0))))
//edges 0 1 (Stats((Ass(Variable "x", Num 2.0)),(Ass(Variable "y", Num 2.0))))
 
(*    and runGC gc q0 q1 =
        match gc with
        | Func(b,c) ->  let qe = a <- a + 1
                                 "q" + string a
                        edges q0 qe (runBool b) + (runthr c qe q1)
        | ElseStat(gc1,gc2) -> runGC gc1 q0 q1 + runGC gc2 q0 q1 
    "digraph program_graph {rankdir=LR;
    node [shape = circle]; q▷;
    node [shape = doublecircle]; q◀; 
    node [shape = circle]\n" + edges comm "q▷" "q◀" + "}"*)


//function that takes input from user and prints corresponding graphviz file if the given string has valid GCL syntax
// and gets an error otherwise
let rec compute n =
    if n = 0 then
        printfn "Bye bye"
    else
        try
        printfn "Enter a GCL code: "
        //try
        // We parse the input string
        let e = parse (Console.ReadLine())
        
        // prints "OK" if given input can be succesfully parsed
        let qI = fresh
        fresh <- fresh + 1
        let qF = fresh
        let res = (edges qI qF e)
        printfn "%A" res
        fresh <- 0
        compute n 
        with err -> compute (n-1)
        //if an error occurs (meaning input was not succesfully parsed), the function prints "KO"
        //with err -> e

compute 3

