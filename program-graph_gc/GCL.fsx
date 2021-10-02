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

(*
// prints the grafical string representation of a (set) pg
let rec printSet s = Set.iter (fun (qI,st,qF) -> printfn "q%d -> q%d [label = \"%s\"];" qI qF st) s
let printGraphviz s = printfn "\nGraphviz:"
                      printfn  "digraph program_graph {rankdir=LR; \nnode [shape = circle]; q0; \nnode [shape = doublecircle]; q00; \nnode [shape = circle] "
                      printSet s
                      printfn "}"
*)

(*
let rec edges qI qF e = 
    match e with
    // we start building edges using a type of Program
    | Program(d,s)          -> fresh <- fresh + 1
                               let q = fresh
                               let e1 = edgesD qI q d
                               let e2 = edgesS q qF s
                               Set.union e1 e2
and edgesS qI qF e =
    match e with 
    | Assign(l,a)              -> set [qI, e, qF]
    | RecordAssign(x,a1,a2)    -> set [qI, e, qF] 

    | Stats(s1,s2)          -> fresh <- fresh + 1
                               let q = fresh
                               let e1 = edgesS qI q s1
                               let e2 = edgesS q qF s2 
                               Set.union e1 e2
    
    | IfStat(b,s0)          -> fresh <- fresh + 1
                               let q = fresh
                               let e = edgesS q qF s0
                               Set.union (set [qI, b, q]) e
    
    | IfElseStat(b, s1, s2) -> fresh <- fresh + 1
                               let q1 = fresh             
                               let e = edgesS q1 qF s1
                               let E1 = Set.union (set [qI, b, q1]) e

                               fresh <- fresh + 1
                               let q2 = fresh
                               let e = edgesS q2 qF s2
                               let E2 = Set.union (set [qI, (NegExpr b), q2]) e    

                               Set.union E1 E2  

    | WhileStat(b,s0)       -> fresh <- fresh + 1
                               let q = fresh
                               let e = edgesS q qI s0 
                               Set.union e (set [(qI, b, q); (qI, (NegExpr b), qF)])
    | Read(l)               -> set [qI, e, qF]
    | Write(x)              -> set [qI, e, qF]

and edgesD qI qF e =
    match e with 
    | VarInt(l)       -> set [qI, e, qF]
    | ArrayInt(a,l)   -> set [qI, e, qF]
    | Decl(d1,d2)     -> fresh <- fresh + 1
                         let q = fresh
                         let e1 = edgesD qI q d1
                         let e2 = edgesD q qF d2 
                         Set.union e1 e2
*)

// test: 
 //edges 0 1 (IfElseStat((GrThan(Var "x", Num 0)), (Assign(Var "x", Num 2)),(Assign(Var "y", Num 2)))) 
// edges "▷" "◀" (Stats((Ass(Variable "x", Num 2.0)),(Ass(Variable "y", Num 2.0))))
 
(*
// not done !!!
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
        printfn "HELOOO"
        printfn "AST:\n%A" ast

        //let pg = (edges 0 -1 ast) 
        //printfn "PG:\n%A" pg
        //printGraphviz pg

        //fresh <- 0
        compute n 
        with err -> printfn "Input is not a valid MicroC program.\n"
                    compute (n-1) 
compute 3

