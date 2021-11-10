//path to run fsLexYacc
//should be changed depending on which system the script is run
// Windows (Stina)
//#r "FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"
// Julien 
#r "FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"

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

// Parser function for user input string, accepting only MicroC valid syntax
let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = GCLParser.start GCLLexer.tokenize lexbuf
    // return the result of parsing
    res

// Regroup all the types of action into one
type Action = 
    | Program of program
    | Declaration of decl
    | ExprA of expra
    | ExprL of exprl
    | ExprB of exprb
    | Statement of stat

// Return the edges of the program graph for a program
let rec edges qI qF p = 
    match p with 
    | Program (Prog(d,s))   -> fresh <- fresh
                               let q = fresh
                               let e1 = edgesD qI q d
                               let e2 = edgesS q qF s
                               Set.union e1 e2
    | _                     -> Set.empty

// Return the edges of the program graph for a declaration
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

// Return the edges of the program graph for a statement
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

// Get all the nodes of the program graph
let getNodes edgesSet = 
    let mutable nodes = Set.empty
    for (q1, _, q2) in Set.toList edgesSet do
        nodes <- nodes.Add(q1).Add(q2)
    Set.toList nodes

// Get all the variables used in an action
let rec def alpha (SV, SA, SR) = 
    match alpha with 
    | Declaration(d)    -> match d with 
                            | VariableDeclaration(x)    -> (Set.union (Set[x]) SV, SA, SR)
                            | ArrayDeclaration(_,A)     -> (SV, Set.union (Set[A]) SA, SR)
                            | RecordDeclaration(R)      -> (SV, SA, Set.union (Set[R]) SR)
                            | _                         -> (SV, SA, SR)
    | Statement(s)      -> match s with 
                            | AssX(x, a)                ->  def (ExprA(a)) (Set.union (Set[x]) SV, SA,SR)
                            | Ass(l, a)                 -> match l with 
                                                            | VariableL(x)              -> def (ExprA(a)) (Set.union (Set[x]) SV, SA,SR)
                                                            | ArrayExpressionL(A,a1)    -> let (SV1, SA1, SR1) = def (ExprA(a)) (SV, Set.union (Set[A]) SA, SR)
                                                                                           def (ExprA(a1)) (SV1, SA1, SR1)
                                                            | FirstRecordL(R)           -> def (ExprA(a)) (SV, SA, Set.union (Set[R]) SR)
                                                            | SecondRecordL(R)          -> def (ExprA(a)) (SV, SA, Set.union (Set[R]) SR)
                            | RecordAss(R, a1, a2)      -> let (SV1, SA1, SR1) = def (ExprA(a1)) (SV, SA, Set.union (Set[R]) SR)
                                                           def (ExprA(a2)) (SV1, SA1, SR1)
                            | Read(l)                   -> match l with 
                                                            | VariableL(x)              -> (Set.union (Set[x]) SV, SA, SR)
                                                            | ArrayExpressionL(A,a1)    -> def (ExprA(a1)) (SV, Set.union (Set[A]) SA, SR) 
                                                            | FirstRecordL(R)           -> (SV, SA, Set.union (Set[R]) SR)
                                                            | SecondRecordL(R)          -> (SV, SA, Set.union (Set[R]) SR)
                            | Write(a)          ->  def (ExprA(a)) (SV, SA, SR)
                            | _ -> (SV, SA, SR)
    | ExprA(a)              -> match a with 
                                | VariableA(x)              -> (Set.union (Set[x]) SV, SA, SR)
                                | ArrayExpressionA(A,a1)    -> def (ExprA(a1)) (SV, Set.union (Set[A]) SA, SR)
                                | FirstRecordA(R)           -> (SV, SA, Set.union (Set[R]) SR)
                                | SecondRecordA(R)          -> (SV, SA, Set.union (Set[R]) SR)
                                | PlusExpr(a1, a2)          -> let (SV1, SA1, SR1) = def (ExprA(a1)) (SV, SA, SR)
                                                               def (ExprA(a2)) (SV1, SA1, SR1)
                                | MinusExpr(a1, a2)         -> let (SV1, SA1, SR1) = def (ExprA(a1)) (SV, SA, SR)
                                                               def (ExprA(a2)) (SV1, SA1, SR1)
                                | TimesExpr(a1, a2)         -> let (SV1, SA1, SR1) = def (ExprA(a1)) (SV, SA, SR)
                                                               def (ExprA(a2)) (SV1, SA1, SR1)
                                | DivExpr(a1, a2)           -> let (SV1, SA1, SR1) = def (ExprA(a1)) (SV, SA, SR)
                                                               def (ExprA(a2)) (SV1, SA1, SR1)
                                | ModExpr(a1, a2)           -> let (SV1, SA1, SR1) = def (ExprA(a1)) (SV, SA, SR)
                                                               def (ExprA(a2)) (SV1, SA1, SR1)
                                | _                         -> (SV, SA, SR)
    | ExprB(b)              -> match b with 
                                | LeThan(a1,a2)     -> let (SV1, SA1, SR1) = def (ExprA(a1)) (SV, SA, SR)
                                                       def (ExprA(a2)) (SV1, SA1, SR1)
                                | GrThan(a1,a2)     -> let (SV1, SA1, SR1) = def (ExprA(a1)) (SV, SA, SR)
                                                       def (ExprA(a2)) (SV1, SA1, SR1)
                                | LeEqThan(a1,a2)   -> let (SV1, SA1, SR1) = def (ExprA(a1)) (SV, SA, SR)
                                                       def (ExprA(a2)) (SV1, SA1, SR1)
                                | GrEqThan(a1,a2)   -> let (SV1, SA1, SR1) = def (ExprA(a1)) (SV, SA, SR)
                                                       def (ExprA(a2)) (SV1, SA1, SR1)
                                | Equals(a1,a2)     -> let (SV1, SA1, SR1) = def (ExprA(a1)) (SV, SA, SR)
                                                       def (ExprA(a2)) (SV1, SA1, SR1)
                                | NotEquals(a1,a2)  -> let (SV1, SA1, SR1) = def (ExprA(a1)) (SV, SA, SR)
                                                       def (ExprA(a2)) (SV1, SA1, SR1)
                                | AndExpr(b1,b2)    -> let (SV1, SA1, SR1) = def (ExprB(b1)) (SV, SA, SR)
                                                       def (ExprB(b2)) (SV1, SA1, SR1)
                                | OrExpr(b1,b2)     -> let (SV1, SA1, SR1) = def (ExprB(b1)) (SV, SA, SR)
                                                       def (ExprB(b2)) (SV1, SA1, SR1)
                                | NegExpr(b1)       -> def (ExprB(b1)) (SV, SA, SR)
                                | _                 -> (SV, SA, SR)
    | _ -> (SV, SA, SR)

// Get all the variables used in the program graph
let getVariables edges = 
    let mutable variables = (Set.empty, Set.empty, Set.empty)
    for (_, alpha, _) in edges do
        variables <- def alpha variables
    variables

// Get the variable which get assigned a value in the action "alpha". Returns "" if there is none.
let leftPart alpha = 
    match alpha with 
    | Declaration(d)    -> match d with 
                            | VariableDeclaration(x)    -> x
                            | ArrayDeclaration(_,A)     -> A
                            | RecordDeclaration(R)      -> R
                            | _                         -> "" 
    | Statement(s)      -> match s with 
                            | AssX(x, _)    -> x
                            | Ass(l, _)     -> match l with 
                                                | VariableL(x) -> x
                                                | ArrayExpressionL(A,_) -> A
                                                | FirstRecordL(R) -> R
                                                | SecondRecordL(R) -> R
                            | RecordAss(R, _, _) -> R
                            | Read(l)            -> match l with 
                                                    | VariableL(x) -> x
                                                    | ArrayExpressionL(A,_) -> A
                                                    | FirstRecordL(R) -> R
                                                    | SecondRecordL(R) -> R
                            | _         -> ""
    | _                 -> ""

// Return the reaching definition analysis of the program graph
let reachingDefinitions edges = 
    let nodes = getNodes edges
    let (SV, SA, SR) = getVariables edges
    let variables = Set.union (SV) (Set.union SA SR)
    let rd = Array.create (nodes.Length) (Set.empty)
    for variable in variables do 
        if (variable <> "") then 
            rd.[0] <- rd.[0].Add(variable, -1, 0)
    let mutable over = false
    while not over do 
        let mutable newRd = false
        for (q1, alpha, q2) in (Set.toList edges) do
            let variable =  leftPart alpha
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

// Return the faint variables of an action
let rec fv a =
    match a with
    | ExprA(a)  -> match a with
                    | VariableA(x) -> Set [x]
                    | ArrayExpressionA(A, a) -> Set.union (Set [A]) (fv(ExprA(a)))
                    | FirstRecordA(R) -> Set [R]
                    | SecondRecordA(R) -> Set [R]
                    | PlusExpr(a1, a2) -> Set.union (fv(ExprA(a1))) (fv(ExprA(a2)))
                    | MinusExpr(a1, a2) -> Set.union (fv(ExprA(a1))) (fv(ExprA(a2)))
                    | TimesExpr(a1, a2) -> Set.union (fv(ExprA(a1))) (fv(ExprA(a2)))  
                    | DivExpr(a1, a2) -> Set.union (fv(ExprA(a1))) (fv(ExprA(a2)))
                    | ModExpr(a1, a2) -> Set.union (fv(ExprA(a1))) (fv(ExprA(a2)))
                    | _ -> Set.empty
    | ExprB(b)  -> match b with 
                    | LeThan(a1, a2) -> Set.union (fv(ExprA(a1))) (fv(ExprA(a2)))
                    | GrThan(a1, a2) -> Set.union (fv(ExprA(a1))) (fv(ExprA(a2)))
                    | LeEqThan(a1, a2) -> Set.union (fv(ExprA(a1))) (fv(ExprA(a2)))
                    | GrEqThan(a1, a2) -> Set.union (fv(ExprA(a1))) (fv(ExprA(a2)))
                    | Equals(a1, a2) -> Set.union (fv(ExprA(a1))) (fv(ExprA(a2)))
                    | NotEquals(a1, a2) -> Set.union (fv(ExprA(a1))) (fv(ExprA(a2)))
                    | AndExpr(b1, b2) -> Set.union (fv(ExprB(b1))) (fv(ExprB(b2)))
                    | OrExpr(b1, b2) -> Set.union (fv(ExprB(b1))) (fv(ExprB(b2)))
                    | NegExpr(b) -> fv(ExprB(b))
                    | _ -> Set.empty
    | _         ->  Set.empty

// Return the kill set of an action for live variables
let kill_lv alpha = 
    match alpha with 
    | Declaration(d)    -> match d with 
                            | VariableDeclaration(x)    -> Set [x]
                            | ArrayDeclaration(_,A)     -> Set [A]
                            | RecordDeclaration(R)      -> Set [R]
                            | _                         -> Set.empty
    | Statement(s)      -> match s with 
                            | AssX(x, _)    -> Set [x]
                            | Ass(l, _)     -> match l with 
                                                | VariableL(x) -> Set [x]
                                                | _ -> Set.empty
                            | RecordAss(R, _, _) -> Set [R]
                            | Read(l)            -> match l with 
                                                    | VariableL(x) -> Set [x]
                                                    | _             -> Set.empty
                            | _         -> Set.empty
    | _                 -> Set.empty
 
 // Return the gen set of an action for live variables
let gen_lv alpha = 
    match alpha with 
    | Statement(s)      -> match s with 
                            | AssX(_, a)    -> fv(ExprA(a))
                            | Ass(l, a)     -> match l with 
                                                | VariableL(_) -> fv(ExprA(a))
                                                | ArrayExpressionL(_, a2) -> Set.union (fv(ExprA(a))) (fv(ExprA(a2)))
                                                | FirstRecordL(_)       -> fv(ExprA(a))
                                                | SecondRecordL(_) -> fv(ExprA(a))
                            | RecordAss(R, a1, a2) -> Set.union (fv(ExprA(a1))) (fv(ExprA(a2)))
                            | Write(a)            -> fv(ExprA(a))
                            | _         -> Set.empty
    | ExprB(b) -> fv(ExprB(b))
    | _                 -> Set.empty

// Return the live variables analysis of the program graph
let liveVariables edges =
    let nodes = getNodes edges
    let liveVariablesArr = Array.create (nodes.Length) (Set.empty)
    let mutable terminate = false
    while not terminate do
        let mutable new_lv = false
        for (q1, alpha, q2) in (Set.toList edges) do
            let kill = kill_lv alpha
            let gen = gen_lv alpha
            if not (Set.isSubset (Set.union (Set.difference liveVariablesArr.[q2] kill) gen) liveVariablesArr.[q1]) then
                new_lv <- true
                liveVariablesArr.[q1] <- (Set.union liveVariablesArr.[q1] (Set.union (Set.difference liveVariablesArr.[q2] kill) gen))
        if not new_lv then
            terminate <- true
    liveVariablesArr
 
// Analysis function for the dangerous variable analysis
let SDV alpha DV = match alpha with
                    | Declaration(a) -> match a with
                                        | VariableDeclaration(x) -> Set.difference DV (Set[x])
                                        | ArrayDeclaration(_, A) -> Set.difference DV (Set[A])
                                        | RecordDeclaration(R)   -> Set.difference DV (Set[R])
                                        | _ -> DV
                    | Statement(s)  -> match s with
                                       | AssX(x,a) -> if Set.isEmpty (Set.intersect (fv (ExprA(a))) (DV)) then (Set.difference DV (Set[x])) else (Set.union DV (Set[x]))
                                       | Ass(l,a) -> match l with 
                                                      | VariableL(x) -> if Set.isEmpty (Set.intersect (fv (ExprA(a))) (DV)) then (Set.difference DV (Set[x])) else (Set.union DV (Set[x]))
                                                      | ArrayExpressionL(A,a1) -> if Set.isEmpty (Set.intersect (Set.union (fv (ExprA(a))) (fv (ExprA(a1)))) (DV)) then DV else (Set.union DV (Set[A]))
                                                      | FirstRecordL(R) ->  if Set.isEmpty (Set.intersect (fv (ExprA(a))) (DV)) then DV else (Set.union DV (Set[R]))
                                                      | SecondRecordL(R) -> if Set.isEmpty (Set.intersect (fv (ExprA(a))) (DV)) then DV else (Set.union DV (Set[R]))
                                       | RecordAss(R, a1, a2) -> if Set.isEmpty (Set.intersect (Set.union (fv (ExprA(a1))) (fv (ExprA(a2)))) (DV)) then (Set.difference DV (Set[R])) else (Set.union DV (Set[R]))
                                       | Read(l) -> match l with 
                                                     | VariableL(x) -> Set.union DV (Set[x])
                                                     | ArrayExpressionL(A,_) -> Set.union DV (Set[A])
                                                     | FirstRecordL(R) -> Set.union DV (Set[R])
                                                     | SecondRecordL(R) -> Set.union DV (Set[R])
                                       | _ -> DV
                    | _ -> DV

// Return the dangerous variables analysis of the program graph
let dangerousVariables edges = 
    let nodes = getNodes edges
    let (SV, SA, SR) = getVariables edges
    let variables = Set.union SV (Set.union SA SR)
    let dv = Array.create (nodes.Length) (Set.empty)
    for variable in variables do 
        if (variable <> "") then 
            dv.[0] <- dv.[0].Add(variable)
    let mutable over = false
    while not over do 
        let mutable newDV = false
        for (q1, alpha, q2) in (Set.toList edges) do
            if not (Set.isSubset (SDV alpha (dv.[q1])) (dv.[q2])) then
                newDV <- true
                dv.[q2] <- (Set.union dv.[q2] (SDV alpha (dv.[q1])))
        if not newDV then 
            over <- true
    dv 

// Analysis function for the faint variables analysis
let SFV alpha FV = match alpha with
                    | Declaration(a) -> match a with
                                        | VariableDeclaration(x) -> Set.difference FV (Set[x])
                                        | ArrayDeclaration(_, A) -> Set.difference FV (Set[A])
                                        | RecordDeclaration(R)   -> Set.difference FV (Set[R])
                                        | _ -> FV
                    | Statement(s)  -> match s with
                                       | AssX(x,a) -> if Set.contains x (FV) then Set.union (Set.difference FV (Set[x])) (fv (ExprA(a))) else FV
                                       | Ass(l,a) -> match l with 
                                                      | VariableL(x) -> if Set.contains x (FV) then Set.union (Set.difference FV (Set[x])) (fv (ExprA(a))) else FV
                                                      | ArrayExpressionL(A,a1) -> if Set.contains A (FV) then Set.union (Set.union (fv (ExprA(a))) (fv (ExprA(a1)))) (FV) else FV
                                                      | FirstRecordL(R) ->  if Set.contains R (FV) then Set.union (fv (ExprA(a))) (FV) else FV
                                                      | SecondRecordL(R) -> if Set.contains R (FV) then Set.union (fv (ExprA(a))) (FV) else FV
                                       | RecordAss(R, a1, a2) -> if Set.contains R (FV) then Set.union (Set.union (fv (ExprA(a1))) (fv (ExprA(a2)))) (FV) else FV
                                       | Read(l) -> match l with 
                                                     | VariableL(x) -> Set.difference FV (Set[x])
                                                     | ArrayExpressionL(A,_) -> Set.difference FV (Set[A])
                                                     | FirstRecordL(R) -> Set.difference FV (Set[R])
                                                     | SecondRecordL(R) -> Set.difference FV (Set[R])
                                       | Write(a) -> Set.union (fv (ExprA(a))) (FV)
                                       | _ -> FV
                    | ExprB(b) -> Set.union (fv (ExprB(b))) (FV)
                    | _ -> FV

// Return the faint variables analysis of the program graph 
let faintVariables edges = 
    let nodes = getNodes edges
    let fv = Array.create (nodes.Length) (Set.empty)
    let mutable over = false
    while not over do 
        let mutable newFV = false
        for (q1, alpha, q2) in (Set.toList edges) do
            if not (Set.isSubset (SFV alpha (fv.[q2])) (fv.[q1])) then
                newFV <- true
                fv.[q1] <- (Set.union fv.[q1] (SFV alpha (fv.[q2])))
        if not newFV then
            over <- true
    fv

let format (arr: Array) =     
    let mutable map = Map.empty
    let mutable key = 0
    let size = arr.Length - 1
    for set in arr do
        map <- map.Add(key, set)
        key <- key + 1
    map

let rec Basic map sets variables=
    if Map.isEmpty map then 
        sets
    else 
        let mutable key = ""
        for v in variables do
            if Map.containsKey v map then 
                key <- v
        let signSet = map.[key]
        printfn "Key:\n%A" key
        printfn "SignSet:\n%A" signSet
        let mutable newSets =  Set.empty
        if Set.isEmpty sets then 
            for sign in signSet do
                printfn "Sign:\n%A" sign
                newSets <- Set.add (Map[(key, Set[sign])]) newSets
        else 
            for mapping in sets do 
                for sign in signSet do
                    newSets <- Set.add (Map.add key (Set[sign]) mapping) newSets
        Basic (Map.remove key map) newSets variables

printfn "Basic:\n%A" (Basic (Map[("x", Set['+'; '0']); ("y", Set['-'; '2'])]) (Set.empty) (Set["x"; "y"]))

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

        let pg = (edges 0 6 (Program ast))
        printfn "PG:\n%A" pg
        printfn "RD:\n%A" (format (reachingDefinitions pg))
        printfn "LV:\n%A" (format (liveVariables pg))
        printfn "DV:\n%A" (format (dangerousVariables pg))
        printfn "FV:\n%A" (format (faintVariables pg))
        //let pg = (edges 0 -1 ast) 
        //printfn "PG:\n%A" pg
        //printGraphviz pg

        //fresh <- 0
        compute n 
compute 3

//{y:= 1; x:=2;  while (x<=100) { if (y <10) { y := y +1; } else {x := x +10;}}}

// set = (x -> Set('-', '+'), y -> Set('-', '+'))
// sets = sets ((Set x -> Set -) (y -> Set -)), (Set x -> Set -) (y -> Set -)), (Set x -> Set -) (y -> Set -)), (Set x -> Set -) (y -> Set -))

 