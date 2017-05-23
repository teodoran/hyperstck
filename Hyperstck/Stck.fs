module Stck

open System
open System.IO
open System.Text.RegularExpressions


type StackItem = int

type Stack = StackItem list

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

let succ stack = 
  push 1 stack |> add

let substract stack = math (fun a b -> b - a) stack

let multiply stack = math (fun a b -> b * a) stack

let divide stack = math (fun a b -> b / a) stack

let modulo stack = math (fun a b -> b % a) stack

let asInt b = if b then 1 else 0

let equal stack = math (fun a b -> asInt(a = b)) stack

let greater stack = math (fun a b -> asInt(a > b)) stack

let less stack = math (fun a b -> asInt(a < b)) stack

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
  [ ("drop", drop)
    ("swap", swap) 
    ("dup", dup)
    ("over", over) 
    ("rot", rot)
    ("len", len) 
    ("plus", add)
    ("succ", succ)
    ("min", substract) 
    ("mul", multiply) 
    ("idiv", divide) 
    ("mod", modulo) 
    ("eq", equal) 
    ("gt", greater) 
    ("lt", less) 
    ("not", not) ]

let stackOperations = primitives

let composites = 
  [ ("2dup", ["over"; "over"])
    ("rem", ["dup"; "rot"; "swap"; "2dup"; "i/"; "*"; "-"; "1000000"; "*"; "swap"; "i/"]) 
    ("/", ["2dup"; "idiv"; "rot"; "rot"; "rem"]) 
    ("empty", ["len"; "0"; "eq"]) ]

(*  ("2dup", ["over"; "over"]);
    ("rem", ["dup"; "rot"; "swap"; "2dup"; "i/"; "*"; "-"; "1000000"; "*"; "swap"; "i/"]);
    ("/", ["2dup"; "i/"; "rot"; "rot"; "rem"]);
    ("empty", ["len"; "0"; "="]);
    ("max", ["len"; "1"; "="; "not"; "?"; "2dup"; ">"; "?"; "swap"; "."; ":"; "."; ";"; "max"; ":"; ";"]);
    ("min", ["len"; "1"; "="; "not"; "?"; "2dup"; "<"; "?"; "swap"; "."; ":"; "."; ";"; "min"; ":"; ";"])*)

let lookup (exp : string) = 
  match stackOperations |> List.tryFind (fun (name, op) -> name = exp) with
  | Some (n, o) -> o
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