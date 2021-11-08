//path to run fsLexYacc

//should be changed depending on which system the script is run
// Windows (Stina)
#r "FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"
// Julien 
//#r "/Users/Julien/F#/FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"

// import of modules, including lexer and parser
open FSharp.Text.Lexing
open System
#load "ParserTypes.fs"
open ParserTypes
#load "GCLParser.fs"
open GCLParser
#load "GCLLexer.fs"
open GCLLexer

#load "GCLSignOperators.fsx"
open GCLSignOperators

let mutable fresh = 0

// Parser function for user input string, accepting only GCL valid syntax
let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = GCLParser.start GCLLexer.tokenize lexbuf
    // return the result of parsing
    res

   
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
    | Prog(d,s) -> fresh <- fresh
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

let def alpha = 
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

let rec defDV alpha = 
    match alpha with 
    | Declaration(d)    -> match d with 
                            | VariableDeclaration(x)    -> Set[x]
                            | ArrayDeclaration(_,A)     -> Set[A]
                            | RecordDeclaration(R)      -> Set[R]
                            | _                         -> Set.empty
    | Statement(s)      -> match s with 
                            | AssX(x, a)    ->  Set.union (Set[x]) (defDV (ExprA(a)))
                            | Ass(l, a)     -> match l with 
                                                | VariableL(x) -> Set.union (Set[x]) (defDV (ExprA(a)))
                                                | ArrayExpressionL(A,a1) -> Set.union (Set[A]) (Set.union (defDV (ExprA(a))) (defDV (ExprA(a1))))
                                                | FirstRecordL(R) -> Set.union (Set[R]) (defDV (ExprA(a)))
                                                | SecondRecordL(R) -> Set.union (Set[R]) (defDV (ExprA(a)))
                            | RecordAss(R, a1, a2) -> Set.union (Set[R]) (Set.union (defDV (ExprA(a1))) (defDV (ExprA(a2))))
                            | Read(l)            -> match l with 
                                                    | VariableL(x) -> Set[x]
                                                    | ArrayExpressionL(A,a1) -> Set.union (Set[A]) (defDV (ExprA(a1)))
                                                    | FirstRecordL(R) -> Set[R]
                                                    | SecondRecordL(R) -> Set[R]
                            | Write(a)          ->  defDV (ExprA(a))
                            | _ -> Set.empty
    | ExprA(a)              -> match a with 
                                | VariableA(x) -> Set[x]
                                | ArrayExpressionA(A,a1) -> Set.union (Set[A]) (defDV (ExprA(a1)))
                                | FirstRecordA(R) -> Set[R]
                                | SecondRecordA(R) -> Set[R]
                                | PlusExpr(a1, a2) -> Set.union (defDV (ExprA(a1))) (defDV (ExprA(a2)))
                                | MinusExpr(a1, a2) -> Set.union (defDV (ExprA(a1))) (defDV (ExprA(a2)))
                                | TimesExpr(a1, a2) -> Set.union (defDV (ExprA(a1))) (defDV (ExprA(a2))) 
                                | DivExpr(a1, a2) -> Set.union (defDV (ExprA(a1))) (defDV (ExprA(a2)))
                                | ModExpr(a1, a2) -> Set.union (defDV (ExprA(a1))) (defDV (ExprA(a2)))
                                | _ -> Set.empty
    | ExprB(b)              -> match b with 
                                | LeThan(a1,a2) -> Set.union (defDV (ExprA(a1))) (defDV (ExprA(a2)))
                                | GrThan(a1,a2) -> Set.union (defDV (ExprA(a1))) (defDV (ExprA(a2)))
                                | LeEqThan(a1,a2) -> Set.union (defDV (ExprA(a1))) (defDV (ExprA(a2)))
                                | GrEqThan(a1,a2) -> Set.union (defDV (ExprA(a1))) (defDV (ExprA(a2)))
                                | Equals(a1,a2) -> Set.union (defDV (ExprA(a1))) (defDV (ExprA(a2)))
                                | NotEquals(a1,a2) -> Set.union (defDV (ExprA(a1))) (defDV (ExprA(a2)))
                                | AndExpr(b1,b2) -> Set.union (defDV (ExprB(b1))) (defDV (ExprB(b2)))
                                | OrExpr(b1,b2) -> Set.union (defDV (ExprB(b1))) (defDV (ExprB(b2)))
                                | NegExpr(b1) -> defDV (ExprB(b1))
                                | _ -> Set.empty
    | _ -> Set.empty

let getNodes edgesSet = 
    let mutable nodes = Set.empty
    for (q1, _, q2) in Set.toList edgesSet do
        nodes <- nodes.Add(q1).Add(q2)
    Set.toList nodes

let getVariables edges = 
    let mutable variables = Set.empty
    for (_, alpha, _) in edges do
        variables <- Set.union (variables) (defDV alpha)
    variables

let reachingDefinitions edges = 
    let nodes = getNodes edges
    let variables = getVariables edges
    let rd = Array.create (nodes.Length) (Set.empty)
    for variable in variables do 
        if (variable <> "") then 
            rd.[0] <- rd.[0].Add(variable, -1, 0)
    printfn "V:\n%A" variables
    printfn "V:\n%A" rd
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

let rec fv a =
    match a with
    | ExprA(a) -> match a with
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
    | ExprB(b) -> match b with 
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

//{y:= 1; x:=2;  while (x<=100) { if (y <10) { y := y +1; } else {x := x +10;}}}
//printfn "%A" (reachingDefinitions (set [(0, "y", 1); (1, "x", 2); (2, "", 3); (3, "", 4); (4, "y", 2); (3, "", 5); (5, "x", 2); (2, "", 6)]))

let sign n = 
    match n with 
    |n when n < 0 -> '-'
    |n when n = 0 -> '0'
    |n when n > 0 -> '+' 
    |_ -> failwith "fail in sign(n) function"
sign -2 = '-'

let op set1 set2 operator = if Set.isEmpty set1 || Set.isEmpty set2 
                            then set [] 
                            // iter through each of the sets and computes the set of signs (see tables in report)
                            else Set.fold(fun acc s1 -> Set.fold (fun acc s2 -> if (operator s1 s2) <> set [] then (operator s1 s2)+acc else set [] ) acc set2 ) (Set.empty) set1 

op (set ['+']) (set ['-';'+']) divideop

let rec AHatDS a (sigmaV, sigmaA, sigmaR) = 
    match a with 
    | Num(n)                  -> set [sign(n)]
    | VariableA(a)            -> Map.find a sigmaV // as defined in book
    //| VariableA(x)          -> if Map.containsKey x sigmaV then Map.find x sigmaV else set []
    | ArrayExpressionA(a1,a2) -> if (Set.intersect (AHatDS a2 (sigmaV, sigmaA, sigmaR)) (set ['0';'+'])) <> set [] then Map.find a1 sigmaA else set []
    | FirstRecordA(a)         -> Map.find a sigmaR 
    | SecondRecordA(a)        -> Map.find a sigmaR 
    | PlusExpr(a1,a2)         -> op (AHatDS a1 (sigmaV, sigmaA, sigmaR)) (AHatDS a2 (sigmaV, sigmaA, sigmaR)) plusop
    | MinusExpr(a1,a2)        -> op (AHatDS a1 (sigmaV, sigmaA, sigmaR)) (AHatDS a2 (sigmaV, sigmaA, sigmaR)) minusop
    | TimesExpr(a1,a2)        -> op (AHatDS a1 (sigmaV, sigmaA, sigmaR)) (AHatDS a2 (sigmaV, sigmaA, sigmaR)) multiplyop
    | DivExpr(a1,a2)          -> op (AHatDS a1 (sigmaV, sigmaA, sigmaR)) (AHatDS a2 (sigmaV, sigmaA, sigmaR)) divideop
    | ModExpr(a1,a2)          -> op (AHatDS a1 (sigmaV, sigmaA, sigmaR)) (AHatDS a2 (sigmaV, sigmaA, sigmaR)) modop
    | UMinusExpr(a)           -> op (AHatDS a (sigmaV, sigmaA, sigmaR)) (set ['-']) uminusop
    | _                       -> failwith "*** UNDEFINED semantic function AHat ***"

// test AHatDS
let memDS = (Map [("x", set ['+'; '0']); ("y", set ['-'; '+']) ], Map [("A", set ['-';'+'])], Map [("R.fst", set ['-']);("R.snd", set ['-'])])

AHatDS (UMinusExpr(VariableA "x")) memDS

let rec BHatDS b (sigmaV, sigmaA, sigmaR) = 
    match b with 
    | True                  -> set ['t'] 
    | False                 -> set ['f'] 
    | AndExpr(b1,b2)        -> op (BHatDS b1 (sigmaV, sigmaA, sigmaR)) (BHatDS b2 (sigmaV, sigmaA, sigmaR)) andop
    | OrExpr(b1,b2)         -> op (BHatDS b1 (sigmaV, sigmaA, sigmaR)) (BHatDS b2 (sigmaV, sigmaA, sigmaR)) orop
    | NegExpr(b)            -> op (BHatDS b (sigmaV, sigmaA, sigmaR)) (BHatDS b (sigmaV, sigmaA, sigmaR)) notop
    | Equals(a1,a2)         -> op (AHatDS a1 (sigmaV, sigmaA, sigmaR)) (AHatDS a2 (sigmaV, sigmaA, sigmaR)) equalop
    | NotEquals(a1,a2)      -> op (AHatDS a1 (sigmaV, sigmaA, sigmaR)) (AHatDS a2 (sigmaV, sigmaA, sigmaR)) notequalop
    | GrThan(a1,a2)         -> op (AHatDS a1 (sigmaV, sigmaA, sigmaR)) (AHatDS a2 (sigmaV, sigmaA, sigmaR)) biggerop
    | GrEqThan(a1,a2)       -> op (AHatDS a1 (sigmaV, sigmaA, sigmaR)) (AHatDS a2 (sigmaV, sigmaA, sigmaR)) biggerequalop
    | LeThan(a1,a2)         -> op (AHatDS a1 (sigmaV, sigmaA, sigmaR)) (AHatDS a2 (sigmaV, sigmaA, sigmaR)) lessop
    | LeEqThan(a1,a2)       -> op (AHatDS a1 (sigmaV, sigmaA, sigmaR)) (AHatDS a2 (sigmaV, sigmaA, sigmaR)) lessequalop
    | _                     -> set ['f'] 

BHatDS (LeThan(VariableA "x", VariableA "y")) memDS

// sigmahat = ((Map [("x", set ['+'; '0']), sigmaA, sigmaR)
// Basic(sigmahat) = ((Map [("x", set [set['0']; set ['+']]), sigmaA, sigmaR)

//let Basic sigmahat =   

let rec SHatDS action (sigmaV, sigmaA, sigmaR) = 
    let isBotDS = (sigmaV.Equals(Map.empty) && sigmaA.Equals(Map.empty) && sigmaR.Equals(Map.empty))
    let botDS = (Map.empty, Map.empty, Map.empty)
    match action with 
    | Statement(s) -> match s with 
                      | AssX(x,a)    -> if AHatDS a (sigmaV, sigmaA, sigmaR) <> set [] 
                                            && not(isBotDS)
                                        then (Map.add x (AHatDS a (sigmaV, sigmaA, sigmaR)) sigmaV, sigmaA, sigmaR) 
                                        else botDS
                      | Ass(l, a2)     -> match l with 
                      
                                          | ArrayExpressionL(A,a1)      -> if (Set.intersect (AHatDS a1 (sigmaV, sigmaA, sigmaR)) (set ['0';'+'])) <> set [] 
                                                                                && AHatDS a2 (sigmaV, sigmaA, sigmaR) <> set [] 
                                                                                && not(isBotDS)
                                                                           then (sigmaV, Map.add A (Set.union (Map.find A sigmaA) (AHatDS a2 (sigmaV, sigmaA, sigmaR))) sigmaA , sigmaR) 
                                                                           else botDS
                                          | FirstRecordL(fst) -> if AHatDS a2 (sigmaV, sigmaA, sigmaR) <> set [] 
                                                                   && not(isBotDS)
                                                                 then (sigmaV, sigmaA, Map.add fst (AHatDS a2 (sigmaV, sigmaA, sigmaR)) sigmaR)
                                                                 else botDS

                                          | SecondRecordL(snd) -> if AHatDS a2 (sigmaV, sigmaA, sigmaR) <> set [] 
                                                                    && not(isBotDS)
                                                                  then (sigmaV, sigmaA, Map.add snd (AHatDS a2 (sigmaV, sigmaA, sigmaR)) sigmaR)
                                                                  else botDS
                                          |_ -> botDS
                      | RecordAss(R, fst, snd) ->  if AHatDS fst (sigmaV, sigmaA, sigmaR) <> set [] 
                                                      && AHatDS snd (sigmaV, sigmaA, sigmaR) <> set [] 
                                                      && not(isBotDS)
                                                   then let (sigmaV, sigmaA, sigmaR) = (sigmaV, sigmaA, Map.add (R+".fst") (AHatDS fst (sigmaV, sigmaA, sigmaR)) sigmaR) // maybe a bit hacky :)
                                                        (sigmaV, sigmaA, Map.add (R+".snd") (AHatDS snd (sigmaV, sigmaA, sigmaR)) sigmaR)                                // maybe a bit hacky :)
                                                   else botDS

                      | Read(l)            -> match l with 
                                              | VariableL(x)           -> if not(isBotDS)
                                                                          then (Map.add x (set ['-';'0';'+']) sigmaV, sigmaA, sigmaR) 
                                                                          else botDS
                                              | ArrayExpressionL(A,a)  -> if (Set.intersect (AHatDS a (sigmaV, sigmaA, sigmaR)) (set ['0';'+'])) <> set [] 
                                                                             && not(isBotDS)
                                                                          then (sigmaV, Map.add A (set ['-';'0';'+']) sigmaA, sigmaR) 
                                                                          else botDS
                                              | FirstRecordL(fst)      -> if not(isBotDS)
                                                                          then (sigmaV, sigmaA, Map.add fst (set ['-';'0';'+']) sigmaR) 
                                                                          else botDS
                                              | SecondRecordL(snd)     -> if not(isBotDS)
                                                                          then (sigmaV, sigmaA, Map.add snd (set ['-';'0';'+']) sigmaR) 
                                                                          else botDS
                                              |_ -> botDS
                      | Write(a)         -> if AHatDS a (sigmaV, sigmaA, sigmaR) <> set [] 
                                            then (sigmaV, sigmaA, sigmaR)
                                            else botDS
                      | _ -> botDS
    | Declaration(d) -> match d with 
                        | VariableDeclaration(x)    -> if not(isBotDS)
                                                       then (Map.add x (set ['0']) sigmaV, sigmaA, sigmaR) 
                                                       else botDS
                        | ArrayDeclaration(_,A)     -> if not(isBotDS)
                                                       then (sigmaV, Map.add A (set ['0']) sigmaA, sigmaR) 
                                                       else botDS
                        | RecordDeclaration(R)      -> if not(isBotDS) 
                                                       then let (sigmaV, sigmaA, sigmaR) = (sigmaV, sigmaA, Map.add (R+".fst") (set ['0']) sigmaR)
                                                            (sigmaV, sigmaA, Map.add (R+".snd") (set ['0']) sigmaR)
                                                       else botDS 
                        |_ ->botDS
    |_ -> botDS

// test SHatDS


SHatDS (Declaration(RecordDeclaration("R"))) memDS

let isSubMap (sV1, sA1, sR1) (sV2, sA2, sR2) variables =
    let mutable isInSet = true
    for v in variables do
        if Map.containsKey v sV1 && Map.containsKey v sV2 then 
            if not(Set.isSubset (Map.find v sV1) (Map.find v sV2) ) then
                isInSet <- false         
    isInSet    

let mapUnion (sV1, sA1, sR1) (sV2, sA2, sR2) variables =
    let mutable (sV, sA, sR) = (Map.empty, Map.empty, Map.empty)
    for v in variables do
        if Map.containsKey v sV1 && Map.containsKey v sV2 then 
            sV <- Map.add v (Set.union (Map.find v sV1) (Map.find v sV2)) sV
    (sV, sA, sR)

         
    

let detectionOfSigns edges = 
    let nodes = getNodes edges
    let variables = getVariables edges
    let res = Array.create (nodes.Length) (Map.empty, Map.empty, Map.empty)
    for variable in variables do 
        if (variable <> "") then 
            let (a, b, c) = res.[0]
            res.[0] <- (Map.add variable (set ['-';'0';'+']) a, b, c)
            for i in 1..nodes.Length-1 do 
                res.[i] <- (Map.add variable (set []) a, b, c)
        printfn "res: \n%A" (res)
        printfn "var: \n%s" variable
    let mutable over = false
    while not over do 
        let mutable newDS = false
        for (q1, alpha, q2) in (Set.toList edges) do
            if not (isSubMap (SHatDS alpha (res.[q1])) (res.[q2]) variables) then
                newDS <- true
                res.[q2] <- (mapUnion res.[q2] (SHatDS alpha (res.[q1])) variables)
        if not newDS then 
            over <- true
    res






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
        printfn "RD:\n%A" (reachingDefinitions pg)
        printfn "LV:\n%A" (liveVariables pg)
        printfn "DS:\n%A" (detectionOfSigns pg)
        //let pg = (edges 0 -1 ast) 
        //printfn "PG:\n%A" pg
        //printGraphviz pg

        fresh <- 0
        compute n 
compute 3

// {int x; int[3] A; {int fst; int snd} R; while (not x == 3) {x := 3 + 5;}}