module Stck

open System
open System.IO
open System.Text.RegularExpressions


type StackItem = int

type Stack = StackItem list

type Op = 
  { op : Stack -> Stack
    minsize : int
    effect : int }

let standard_library = [
    ("2dup", ["over"; "over"]);
    ("rem", ["dup"; "rot"; "swap"; "2dup"; "i/"; "*"; "-"; "1000000"; "*"; "swap"; "i/"]);
    ("/", ["2dup"; "i/"; "rot"; "rot"; "rem"]);
    ("empty", ["len"; "0"; "="]);
    ("max", ["len"; "1"; "="; "not"; "?"; "2dup"; ">"; "?"; "swap"; "."; ":"; "."; ";"; "max"; ":"; ";"]);
    ("min", ["len"; "1"; "="; "not"; "?"; "2dup"; "<"; "?"; "swap"; "."; ":"; "."; ";"; "min"; ":"; ";"])
]

let push e stack  =
    e :: stack

let printerr op = printfn "Cannot %s on the stack" op

let drop stack =
    match stack with
    | tos :: rest -> rest
    | _ -> failwith "drop"

let swap stack =
    match stack with
    | a :: b :: rest -> b :: a :: rest 
    | _ -> failwith "swap"

let dup stack =
    match stack with
    | tos :: rest -> tos :: tos :: rest
    | _ -> failwith "dup"

let over stack =
    match stack with
    | a :: b :: rest -> b :: a :: b :: rest
    | _ -> failwith "over"

let rot stack =
    match stack with
    | a :: b :: c :: rest -> c :: a :: b :: rest
    | _ -> failwith "rot"

let len stack =
    let n = stack |> List.length  
    push n stack

let isInt string =
    let couldParse, value = Int32.TryParse(string)
    couldParse

let math op stack =
    match stack with
    | a :: b :: rest -> 
      let n = (op a b)
      push n rest 
    | _ -> failwith "do math"

let add stack = math (fun a b -> b + a) stack

let zero stack = 
  push 0 stack

let succ stack = 
  push 1 stack |> add

let neg stack = 
  match stack with 
  | a :: rest -> -a :: rest
  | _ -> failwith "do math"

let subt stack = math (fun a b -> b - a) stack

let mult stack = math (fun a b -> b * a) stack

let idiv stack = math (fun a b -> b / a) stack

let modulo stack = math (fun a b -> b % a) stack

let asInt b = if b then 1 else 0

let equal stack = math (fun a b -> asInt(a = b)) stack

let greater stack = math (fun a b -> asInt(a > b)) stack

let less stack = math (fun a b -> asInt(a < b)) stack

let num (n : int) stack = push n stack 

let not stack =
    match stack with
    | tos :: rest -> 
      let h = if (tos <> 0) then 0 else 1
      h :: rest
    | _ -> failwith "not"

let sprint stack =
    printfn "%A" stack
    stack

let rec hprint heap =
    match heap with
    | [] -> printf ""
    | head :: tail ->
        printfn "# %s %A" (fst head) (snd head)
        hprint tail

let print hs =
    let stack = snd hs
    sprint stack |> ignore
    hs

let primitives = 
  [ 
      ("drop", { op = drop;    minsize = 1; effect = -1 })
      ("swap", { op = swap;    minsize = 2; effect =  0 })
      ("dup",  { op = dup;     minsize = 1; effect = +1 })
      ("over", { op = over;    minsize = 2; effect = +1 })
      ("rot",  { op = rot;     minsize = 3; effect =  0 })
      ("len",  { op = len;     minsize = 0; effect = +1 })
      ("plus", { op = add;     minsize = 2; effect = -1 })
      ("zero", { op = zero;    minsize = 0; effect = +1 })
      ("succ", { op = succ;    minsize = 1; effect =  0 })
      ("min",  { op = subt;    minsize = 1; effect =  0 })
      ("neg",  { op = neg;     minsize = 1; effect =  0 })
      ("mult", { op = mult;    minsize = 2; effect = -1 })
      ("idiv", { op = idiv;    minsize = 2; effect = -1 })
      ("mod",  { op = modulo;  minsize = 2; effect = -1 })
      ("eq",   { op = equal;   minsize = 2; effect = -1 })
      ("gt",   { op = greater; minsize = 2; effect = -1 })
      ("lt",   { op = less;    minsize = 2; effect = -1 })
      ("not",  { op = not;     minsize = 1; effect =  0 })
  ]

let compose (op1 : Op) (op2 : Op) : Op = 
  let op = op1.op >> op2.op
  let minsize = max op1.minsize (op2.minsize - op1.effect)
  let effect = op1.effect + op2.effect
  { op = op; minsize = minsize; effect = effect }

let duup = over >> over

let rem = duup >> rot >> swap >> duup >> idiv >> mult >> subt >> num 1000000 >> mult >> swap >> idiv

let div = duup >> idiv >> rot >> rot >> rem

let empty = len >> num 0 >> equal

let composites = 
  [ ("duup", duup)
    ("rem", rem) 
    ("div", div) 
    ("empty", empty) ]

let availableOperations stackSize = 
  primitives |> List.filter (fun (name, op) -> stackSize >= op.minsize)

let availableNames stackSize = 
  let availablePrimitives = availableOperations stackSize
  "num" :: (availablePrimitives |> List.map (fun (name, _) -> name))

(*  ("2dup", ["over"; "over"]);
    ("rem", ["dup"; "rot"; "swap"; "2dup"; "i/"; "*"; "-"; "1000000"; "*"; "swap"; "i/"]);
    ("/", ["2dup"; "i/"; "rot"; "rot"; "rem"]);
    ("empty", ["len"; "0"; "="]);
    ("max", ["len"; "1"; "="; "not"; "?"; "2dup"; ">"; "?"; "swap"; "."; ":"; "."; ";"; "max"; ":"; ";"]);
    ("min", ["len"; "1"; "="; "not"; "?"; "2dup"; "<"; "?"; "swap"; "."; ":"; "."; ";"; "min"; ":"; ";"])*)

let lookup (exp : string) = 
  match primitives |> List.tryFind (fun (name, op) -> name = exp) with
  | Some (n, o) -> o.op
  | None ->
    failwith <| sprintf "Unknown operation %s" exp
    
let exec exp stack =
  stack |> lookup exp

let define s heap =
    s :: heap

let rec find s heap =
    match heap with
    | [] -> []
    | head :: tail ->
        if fst head = s then
            snd head
        else 
            find s tail

let tokens (s:string) =
    s.Split([|' '|]) |> Array.toList

let rec split delim n col exps =
    match exps with
    | [] -> (col |> List.rev, [])
    | head :: tail ->
        match head with
        | "?" -> split delim (n + 1) (head :: col) tail
        | d when d = delim ->
            match n with
            | 0 -> (col |> List.rev, tail)
            | _ -> split delim (n - 1) (head :: col) tail
        | _ -> split delim n (head :: col) tail
            
let cond tos exps =
    let t = split ":" 0 [] exps
    let f = split ";" 0 [] (snd t)

    match tos <> 0 with
    | true -> (fst t) @ (snd f)
    | false -> (fst f) @ (snd f)

let lines (s:string) =
    let trimmed = Regex.Replace(s, @"\s+", " ");
    let lines = trimmed.Split([|'!'|]) |> Array.toList

    lines
    |> List.filter (fun line -> line <> " ")
    |> List.map (fun line -> line.Trim())