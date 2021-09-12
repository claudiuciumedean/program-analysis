﻿//path to run fsLexYacc
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

let rec isDone e =
    match e with 
    | ImplyExpr(b,C) -> (NegExpr b)
    | ElseIfExpr(GC1,GC2) -> (AndExpr (isDone GC1, isDone GC2))
    | _ -> failwith "no matchcase for input isDone"

let rec edges qI qF e = 
    match e with
    | Ass(x,a)       -> set [qI, runStat (Ass(x,a)) , qF]
    | Stats(S1,S2)   -> fresh <- fresh + 1
                        let q = fresh
                        let E1 = edges qI q S1
                        let E2 = edges q qF S2 
                        Set.union E1 E2
    | IfElseStat(b, s1, s2) ->  let q1 = fresh + 1
                                let q2 = fresh + 2
                                fresh <- fresh + 2
                                let E11 = edges qI q1 b
                                let E12 = edges q1 qF s1
                                let E21 = edges qI q2 (runBool NegExpr b) 
                                let E22 = edges q2 qF s2
                                Set.union E22 (Set.union E21 (Set.union E11 E12))
    | IfExpr(GC)            -> edges qI qF GC 
    | DoExpr(GC)            -> let b = isDone GC
                               let E = edges qI qI GC
                               Set.union E (set [qI, (expr b), qF])
    | ImplyExpr(b,C)        -> fresh <- fresh + 1
                               let q = fresh
                               let E = edges q qF C
                               Set.union (set [qI, expr b, q]) E
    | ElseIfExpr(GC1,GC2)   -> let E1 = edges qI qF GC1
                               let E2 = edges qI qF GC2
                               Set.union E1 E2 
    | _ -> failwith "no matchcase for input edges"





(*
//the done function as described in the textbook
let rec finished gc =
    match gc with
    | Func(b,c) -> NegExpr(b)
    | ElseStat(gc1,gc2) -> AndExpr(finished gc1, finished gc2)

let graph comm =
    //mutable variable used to increment node numbers in graph
    let mutable a = -1

    //function handling all command types
    let rec runthr e q0 q1 =
        match e with
        | Ass(s,ex) -> edges q0 q1 (s + ":=" + runExpr ex)
        | Skip      -> ""
        | Stats(s1,s2)      ->  let qe = a <- a + 1
                                         "q" + string a
                                runthr s1 q0 qe + runthr s2 qe q1
        | IfElseStat(b, s1, s2) -> runGC gc q0 q1
        | DoStat(gc) -> runGC gc q0 q0 + edges q0 q1 (runBool (finished gc))
    
    *)
    //function handling guarded command types
 
 
(*    and runGC gc q0 q1 =
        match gc with
        | Func(b,c) ->  let qe = a <- a + 1
                                 "q" + string a
                        edges q0 qe (runBool b) + (runthr c qe q1)
        | ElseStat(gc1,gc2) -> runGC gc1 q0 q1 + runGC gc2 q0 q1 *)
    "digraph program_graph {rankdir=LR;
    node [shape = circle]; q▷;
    node [shape = doublecircle]; q◀; 
    node [shape = circle]\n" + runthr comm "q▷" "q◀" + "}"


//function that takes input from user and prints corresponding graphviz file if the given string has valid GCL syntax
// and gets an error otherwise
let rec compute =
        printfn "Enter a GCL code: "
        //try
        // We parse the input string
        let e = parse (Console.ReadLine())
        
        // prints "OK" if given input can be succesfully parsed

        printfn "%A" e

        let b = graph e
        printfn "%s" b
        b
        //if an error occurs (meaning input was not succesfully parsed), the function prints "KO"
        //with err -> e

compute

