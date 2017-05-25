module Stck

open System

type StackItem = int

type Stack = StackItem list

let nop stack = stack

let top = function 
  | [] -> failwith "top"
  | h :: _ -> h

let push e stack  =
    e :: stack

let drop = function
  | tos :: rest -> rest
  | _ -> failwith "drop"

let swap = function 
  | a :: b :: rest -> b :: a :: rest 
  | _ -> failwith "swap"

let dup = function 
  | tos :: rest -> tos :: tos :: rest
  | _ -> failwith "dup"

let over = function
  | a :: b :: rest -> b :: a :: b :: rest
  | _ -> failwith "over"

let rot = function
  | a :: b :: c :: rest -> c :: a :: b :: rest
  | _ -> failwith "rot"

let len stack =
  let n = stack |> List.length  
  push n stack

let isInt string =
  let couldParse, value = Int32.TryParse(string)
  couldParse

let math op = function 
  | a :: b :: rest -> 
    let n = (op a b)
    push n rest 
  | _ -> failwith "do math"

let add = math (fun a b -> b + a)

let subt = math (fun a b -> b - a)

let zero =  push 0

let succ stack = 
  push 1 stack |> add

let prev stack = 
  push 1 stack |> subt

let neg = function
  | a :: rest -> -a :: rest
  | _ -> failwith "do math"

let mult = math (fun a b -> b * a)

let idiv = math (fun a b -> b / a)

let modulo = math (fun a b -> b % a)

let asInt b = if b then 1 else 0

let equal = math (fun a b -> asInt(a = b))

let greater = math (fun a b -> asInt(a > b))

let less = math (fun a b -> asInt(a < b))

let num (n : int) = push n 

let not = function
  | tos :: rest -> 
    let h = if (tos <> 0) then 0 else 1
    h :: rest
  | _ -> failwith "not"

let duup = over >> over

let rem = duup >> rot >> swap >> duup >> idiv >> mult >> subt >> num 1000000 >> mult >> swap >> idiv

let div = duup >> idiv >> rot >> rot >> rem

let empty = len >> zero >> equal
