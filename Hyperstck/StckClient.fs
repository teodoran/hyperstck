module StckClient

open Stck

open System
open System.IO
open System.Text.RegularExpressions

type StackItem = int

type Stack = StackItem list

type Op = 
  { op : Stack -> Stack
    minsize : int
    effect : int }

let nopop  = { op = nop;     minsize = 0; effect =  0 }
let dropop = { op = drop;    minsize = 1; effect = -1 }
let swapop = { op = swap;    minsize = 2; effect =  0 }
let dupop  = { op = dup;     minsize = 1; effect = +1 }
let overop = { op = over;    minsize = 2; effect = +1 }
let rotop  = { op = rot;     minsize = 3; effect =  0 }
let lenop  = { op = len;     minsize = 0; effect = +1 }
let addop  = { op = add;     minsize = 2; effect = -1 }
let zeroop = { op = zero;    minsize = 0; effect = +1 }
let succop = { op = succ;    minsize = 1; effect =  0 }
let prevop = { op = prev;    minsize = 1; effect =  0 }
let minop  = { op = subt;    minsize = 2; effect = -1 }
let negop  = { op = neg;     minsize = 1; effect =  0 }
let multop = { op = mult;    minsize = 2; effect = -1 }
let idivop = { op = idiv;    minsize = 2; effect = -1 }
let modop  = { op = modulo;  minsize = 2; effect = -1 }
let eqop   = { op = equal;   minsize = 2; effect = -1 }
let gtop   = { op = greater; minsize = 2; effect = -1 }
let ltop   = { op = less;    minsize = 2; effect = -1 }
let notop  = { op = not;     minsize = 1; effect =  0 }

let primitives = 
  [ 
      ("drop", dropop)
      ("swap", swapop)
      ("dup",  dupop)
      ("over", overop)
      ("rot",  rotop)
      ("len",  lenop)
      ("plus", addop)
      ("zero", zeroop)
      ("succ", succop)
      ("prev", prevop)
      ("min",  minop)
      ("neg",  negop)
      ("mult", multop)
      ("idiv", idivop)
      ("mod",  modop)
      ("eq",   eqop)
      ("gt",   gtop)
      ("lt",   ltop)
      ("not",  notop)
  ]

let combine (op1 : Op) (op2 : Op) : Op = 
  let op = op1.op >> op2.op
  let minsize = max op1.minsize (op2.minsize - op1.effect)
  let effect = op1.effect + op2.effect
  { op = op; minsize = minsize; effect = effect }

let compose ops =
  ops |> List.fold combine nopop

let duupop = [ overop; overop ] |> compose
let remop  = 
  let millop = { op = num 100000; minsize = 0; effect = +1 }
  [ duupop; rotop; swapop; duupop; duupop; idivop; idivop; multop; minop; millop; multop; swapop; idivop ] |> compose
  
let divop = [ duupop; idivop; rotop; rotop; remop ] |> compose

let emptyop = [ lenop; zeroop; eqop ] |> compose

let composites = 
  [ ("duup", duupop)
    ("rem", remop) 
    ("div", divop) 
    ("empty", emptyop) ]

let allOperations = 
  primitives @ composites

let availableOperations stackSize = 
  allOperations |> List.filter (fun (name, op) -> stackSize >= op.minsize)

let availableNames stackSize = 
  let available = availableOperations stackSize
  "num" :: (available |> List.map (fun (name, _) -> name))

let lookup (exp : string) = 
  match allOperations |> List.tryFind (fun (name, op) -> name = exp) with
  | Some (n, o) -> o.op
  | None ->
    failwith <| sprintf "Unknown operation %s" exp

let lookupOp (exp : string) = 
  match primitives |> List.tryFind (fun (name, op) -> name = exp) with
  | Some (n, o) -> o
  | None ->
    failwith <| sprintf "Unknown operation %s" exp
    
let exec exp stack =
  stack |> lookup exp
