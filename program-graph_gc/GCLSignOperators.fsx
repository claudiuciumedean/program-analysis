module GCLSignOperators

let plusop s1 s2 = 
    match s1, s2 with
    | ('-', '-') -> set ['-']
    | ('-', '0') -> set ['-']
    | ('-', '+') -> set ['-'; '0'; '+']
    | ('0', '-') -> set ['-']
    | ('0', '0') -> set ['0']
    | ('0', '+') -> set ['+']
    | ('+', '-') -> set ['-'; '0'; '+']
    | ('+', '0') -> set ['+']
    | ('+', '+') -> set ['+']

let minusop s1 s2 = 
    match s1, s2 with
    | ('-', '-') -> set ['-'; '0'; '+']
    | ('-', '0') -> set ['-']
    | ('-', '+') -> set ['-']
    | ('0', '-') -> set ['+']
    | ('0', '0') -> set ['0']
    | ('0', '+') -> set ['-']
    | ('+', '-') -> set ['+']
    | ('+', '0') -> set ['+']
    | ('+', '+') -> set ['-'; '0'; '+']

let uminusop s1 s2 = 
    match s1 with
    | '-' -> set ['+']
    | '+' -> set ['-']
    | '0' -> set ['0']

let multiplyop s1 s2 = 
    match s1, s2 with
    | ('-', '-') -> set ['+']
    | ('-', '0') -> set ['0']
    | ('-', '+') -> set ['-']
    | ('0', '-') -> set ['0']
    | ('0', '0') -> set ['0']
    | ('0', '+') -> set ['0']
    | ('+', '-') -> set ['-']
    | ('+', '0') -> set ['0']
    | ('+', '+') -> set ['+']

let divideop s1 s2 = 
    match s1, s2 with
    | ('-', '-') -> set ['0';'+']
    | ('-', '0') -> set [] // error
    | ('-', '+') -> set ['-';'0']
    | ('0', '-') -> set ['0']
    | ('0', '0') -> set [] // error
    | ('0', '+') -> set ['0']
    | ('+', '-') -> set ['-';'0']
    | ('+', '0') -> set [] // error
    | ('+', '+') -> set ['0';'+']

let modop s1 s2 = 
    match s1, s2 with
    | ('-', '-') -> set ['-';'0']
    | ('-', '0') -> set [] // error
    | ('-', '+') -> set ['0';'+']
    | ('0', '-') -> set ['0']
    | ('0', '0') -> set [] // error
    | ('0', '+') -> set ['0']
    | ('+', '-') -> set ['-';'0']
    | ('+', '0') -> set [] // error
    | ('+', '+') -> set ['0';'+']

(*
let powerop s1 s2 = 
    match s1, s2 with
    | ('-', '-') -> set ['-']
    | ('-', '0') -> set ['-'] 
    | ('-', '+') -> set ['-']
    | ('0', '-') -> set ['0']
    | ('0', '0') -> set ['+'] 
    | ('0', '+') -> set ['0']
    | ('+', '-') -> set ['+']
    | ('+', '0') -> set ['+']
    | ('+', '+') -> set ['+']
*)

let equalop s1 s2 = 
    match s1, s2 with
    | ('-', '-') -> set ['t'; 'f']
    | ('-', '0') -> set ['f']
    | ('-', '+') -> set ['f']
    | ('0', '-') -> set ['f']
    | ('0', '0') -> set ['t']
    | ('0', '+') -> set ['f']
    | ('+', '-') -> set ['f']
    | ('+', '0') -> set ['f']
    | ('+', '+') -> set ['t';'f'] 

let notequalop s1 s2 = 
    match s1, s2 with
    | ('-', '-') -> set ['t'; 'f']
    | ('-', '0') -> set ['t']
    | ('-', '+') -> set ['t']
    | ('0', '-') -> set ['t']
    | ('0', '0') -> set ['f']
    | ('0', '+') -> set ['t']
    | ('+', '-') -> set ['t']
    | ('+', '0') -> set ['t']
    | ('+', '+') -> set ['t';'f'] 

let biggerop s1 s2 = 
    match s1, s2 with
    | ('-', '-') -> set ['t'; 'f']
    | ('-', '0') -> set ['f']
    | ('-', '+') -> set ['f']
    | ('0', '-') -> set ['t']
    | ('0', '0') -> set ['f']
    | ('0', '+') -> set ['f']
    | ('+', '-') -> set ['t']
    | ('+', '0') -> set ['t']
    | ('+', '+') -> set ['t';'f'] 

let biggerequalop s1 s2 = 
    match s1, s2 with
    | ('-', '-') -> set ['t'; 'f']
    | ('-', '0') -> set ['f']
    | ('-', '+') -> set ['f']
    | ('0', '-') -> set ['t']
    | ('0', '0') -> set ['t']
    | ('0', '+') -> set ['f']
    | ('+', '-') -> set ['t']
    | ('+', '0') -> set ['t']
    | ('+', '+') -> set ['t';'f']     

let lessop s1 s2 = 
    match s1, s2 with
    | ('-', '-') -> set ['t'; 'f']
    | ('-', '0') -> set ['t']
    | ('-', '+') -> set ['t']
    | ('0', '-') -> set ['f']
    | ('0', '0') -> set ['f']
    | ('0', '+') -> set ['t']
    | ('+', '-') -> set ['f']
    | ('+', '0') -> set ['f']
    | ('+', '+') -> set ['t';'f'] 

let lessequalop s1 s2 = 
    match s1, s2 with
    | ('-', '-') -> set ['t'; 'f']
    | ('-', '0') -> set ['t']
    | ('-', '+') -> set ['t']
    | ('0', '-') -> set ['f']
    | ('0', '0') -> set ['t']
    | ('0', '+') -> set ['t']
    | ('+', '-') -> set ['f']
    | ('+', '0') -> set ['f']
    | ('+', '+') -> set ['t';'f'] 

let andop s1 s2 = 
    match s1, s2 with
    | ('t', 't') -> set ['t']
    | ('t', 'f') -> set ['f']
    | ('f', 't') -> set ['f']
    | ('f', 'f') -> set ['f']

(*
let condandop s1 s2 = 
    match s1, s2 with
    | ('t', 't') -> set ['t']
    | ('t', 'f') -> set ['f']
    | ('f', 't') -> set ['f']
    | ('f', 'f') -> set ['f']
*)

let orop s1 s2 = 
    match s1, s2 with
    | ('t', 't') -> set ['t']
    | ('t', 'f') -> set ['t']
    | ('f', 't') -> set ['t']
    | ('f', 'f') -> set ['f']

(*
let condorop s1 s2 = 
    match s1, s2 with
    | ('t', 't') -> set ['t']
    | ('t', 'f') -> set ['t']
    | ('f', 't') -> set ['t']
    | ('f', 'f') -> set ['f']
*)

let notop s1 s2 = 
    match s1 with
    | 't' -> set ['f']
    | 'f' -> set ['t']

