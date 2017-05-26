module Hyperstck.Program

open System

open Suave
open Suave.Response
open Suave.Filters
open Suave.Operators
open Suave.Successful
open Suave.Redirection
open Suave.RequestErrors
open Suave.ServerErrors
open Writers

open Stck
open StckClient

let seeOtherRedirect location = 
  setHeader "Location" location >=> response HTTP_303 [||]

let SEE_OTHER = seeOtherRedirect

let isOpName (s : string) =
  s |> Seq.forall Char.IsLetter

let createOplink (input : string) name = 
  let input' = if input.Length = 0 then "" else "/" + input
  let href = sprintf "%s/%s" input' name
  let result = sprintf "<a href=\"%s\">%s</a>" href name
  result

let toList (s : string) : string list = 
  if s.Length = 0 then []
  else
    s.Split(['/'] |> List.toArray) |> List.ofArray

let recreate (s : string) : string list = 
  s |> toList |> List.rev

let decreate (st : int list) : string = 
  let resultStackStrings = st |> List.map (sprintf "%d")
  let forward = resultStackStrings |> List.rev
  String.concat "/" forward

let handleNumRequestWithStack s = request (fun r ->
  match r.queryParam "n" with
  | Choice1Of2 nstr ->
    let (isNumber, n) = Int32.TryParse nstr
    if isNumber then 
      printfn "My stack is '%s'" s
      let stack = s |> recreate |> List.map Int32.Parse
      let stack' = (num n) stack 
      let s' = decreate stack'
      let location = sprintf "/%s" s'
      printfn "Redirect to %s" location
      FOUND "" >=> setHeader "location" location
    else
      BAD_REQUEST <| sprintf "Not a number: %s" nstr
  | Choice2Of2 x -> 
    let stuff = toList s
    let stackDiv = 
      if stuff.Length = 0 then 
        "<div>[]</div>"
      else 
        stuff |> String.concat " " |> sprintf "<div>[ %s ]</div>"
    let formAction = if s = "" then "/num" else sprintf "/%s/num" s
    let bodyDiv = sprintf "<form action=\"%s\" method=\"get\"><input type=\"text\" name=\"n\" /><input type=\"submit\" value=\"Submit\"></form>" formAction
    let html = sprintf "<html><style>body { font-family: consolas; }</style><body>%s%s</body></html>" stackDiv bodyDiv
    OK html)

let handleDefineRequestWithStack (s : string) = request (fun r ->
  match r.queryParam "name" with
  | Choice1Of2 name ->
    let stack' = if s.Length = 0 then sprintf "define/%s" name else sprintf "%s/define/%s" s name
    let location = sprintf "/%s" stack'
    printfn "Redirect to %s" location
    FOUND "" >=> setHeader "location" location 
  | Choice2Of2 x -> 
    let stuff = recreate s |> List.rev
    let stackDiv = stuff |> String.concat " " |> sprintf "<div>[ %s ]</div>"
    let formAction = if s = "" then "/define" else sprintf "/%s/define" s
    let bodyDiv = sprintf "<form action=\"%s\" method=\"get\">Subroutine name:<br /><input type=\"text\" name=\"name\" /><input type=\"submit\" value=\"Submit\"></form>" formAction
    let html = sprintf "<html><style>body { font-family: consolas; }</style><body>%s%s</body></html>" stackDiv bodyDiv
    OK html)

let handleInsideDefineRequestWithStack (stk, def) = request (fun r ->
  let stack = toList stk
  let definition = toList def
  let stackDiv = stack |> String.concat " " |> sprintf "<div>[ %s ]</div>"
  let stackSize = stack |> List.length
  let fullpath = if stk.Length = 0 then sprintf "define/%s" def else sprintf "%s/define/%s" stk def
  let oplinkList = "end" :: allNames() |> List.map (createOplink fullpath)
  let oplinkListItems = oplinkList |> List.map (fun a -> sprintf "<li>%s</li>" a)
  let oplinksDiv = sprintf "<div><ul>%s</ul></div>" <| String.concat "" oplinkListItems
  let html = sprintf "<html><style>body { font-family: consolas; }</style><body>%s%s</body></html>" stackDiv oplinksDiv
  OK html >=> setHeader "Pragma" "no-cache" >=> setHeader "Content-Type" "text/html; charset=utf-8")

let handleEndDefineRequestWithStack (stk, def) = request (fun r ->
  let stack = toList stk
  let definition = toList def
  match definition with 
  | [] -> BAD_REQUEST "Empty definition"
  | [n] -> BAD_REQUEST <| sprintf "Just the name %s doesn't do anything." n
  | name :: body ->
    defineSubroutine name body
    let stackDiv = stack |> String.concat " " |> sprintf "<div>[ %s ]</div>"
    let stackSize = stack |> List.length
    let oplinkList = "define" :: availableNames stackSize |> List.map (createOplink stk)
    let oplinkListItems = oplinkList |> List.map (fun a -> sprintf "<li>%s</li>" a)
    let oplinksDiv = sprintf "<div><ul>%s</ul></div>" <| String.concat "" oplinkListItems
    let html = sprintf "<html><style>body { font-family: consolas; }</style><body>%s%s</body></html>" stackDiv oplinksDiv
    let location = sprintf "/%s" stk
    printfn "Redirect to %s" location
    SEE_OTHER "" >=> setHeader "location" location)

let handleRequest (input : string) = request (fun r ->
  printfn "handleRequest '%s'" input
  let backwards = recreate input
  match backwards with
  | [] ->
    BAD_REQUEST "nothing" 
  | opname :: stackStrings when isOpName opname ->
    let stack = stackStrings |> List.map Int32.Parse
    printfn "do %s on %s" opname (stackStrings |> String.concat "/")
    try 
      let stack' = exec opname stack
      let resultStackStrings = stack' |> List.map (sprintf "%d")
      let forward = resultStackStrings |> List.rev
      let s = String.concat "/" forward
      let location = sprintf "/%s" s
      printfn "Redirect to %s" location
      FOUND "" >=> setHeader "location" location 
    with 
      | Failure(msg) -> BAD_REQUEST <| sprintf "Can't do %s on stack" msg
  | stuff ->
    let stackDiv = 
      if stuff.Length = 0 then 
        "<div>[]</div>"
      else 
        stuff |> List.rev |> String.concat " " |> sprintf "<div>[ %s ]</div>"
    let stackSize = stuff |> List.length
    let oplinkList = "define" :: availableNames stackSize |> List.map (createOplink input)
    let oplinkListItems = oplinkList |> List.map (fun a -> sprintf "<li>%s</li>" a)
    let oplinksDiv = sprintf "<div><ul>%s</ul></div>" <| String.concat "" oplinkListItems
    let html = sprintf "<html><style>body { font-family: consolas; }</style><body>%s%s</body></html>" stackDiv oplinksDiv
    OK html >=> setHeader "Pragma" "no-cache" >=> setHeader "Content-Type" "text/html; charset=utf-8")

let handleEmptyRequest = request (fun r ->
  let oplinkList = "define" :: availableNames 0 |> List.map (createOplink "")
  let oplinkListItems = oplinkList |> List.map (fun a -> sprintf "<li>%s</li>" a)
  let oplinksDiv = sprintf "<div><ul>%s</ul></div>" <| String.concat "" oplinkListItems
  let stackDiv = "<div>[]</div>"
  let html = sprintf "<html><style>body { font-family: consolas; }</style><body>%s%s</body></html>" stackDiv oplinksDiv
  OK html >=> setHeader "Pragma" "no-cache" >=> setHeader "Content-Type" "text/html; charset=utf-8")

let handleShowOpRequest s = request (fun r ->
  let op = lookupOp s
  let nameStr = sprintf "Subroutine: %s" s
  let minstackStr = sprintf "Required stack size: %d" op.minsize
  let effectStr = 
    if op.effect = 0 then
      "Does not change the size of the stack"
    else if op.effect > 0 then
      sprintf "Increases the size of the stack by %d" op.effect
    else 
      sprintf "Reduces the size of the stack by %d" (0 - op.effect)
  let inDiv = sprintf "<div>%s</div>"
  let html = sprintf "<html><style>body { font-family: consolas; }</style><body>%s%s%s</body></html>" (inDiv nameStr) (inDiv minstackStr) (inDiv effectStr)
  OK html)

let app : WebPart = 
  choose [ 
      GET >=> pathScan "/docs/%s" handleShowOpRequest
      GET >=> pathScan "/define/%s/end" (fun def -> handleEndDefineRequestWithStack ("", def))
      GET >=> pathScan "/%s/define/%s/end" handleEndDefineRequestWithStack
      GET >=> pathScan "/define/%s" (fun def -> handleInsideDefineRequestWithStack ("", def))
      GET >=> pathScan "/%s/define/%s" handleInsideDefineRequestWithStack
      GET >=> path "/define" >=> handleDefineRequestWithStack ""
      GET >=> pathScan "/%s/define" handleDefineRequestWithStack
      GET >=> path "/num" >=> handleNumRequestWithStack ""
      GET >=> pathScan "/%s/num" handleNumRequestWithStack
      GET >=> path "/" >=> handleEmptyRequest
      GET >=> pathScan "/%s" handleRequest 
  ]

[<EntryPoint>]
let main argv =
  let port = match argv with [| p |] -> (uint16 p) | _ -> uint16 8282
  let config =
    { defaultConfig with
        bindings = [ HttpBinding.create HTTP Net.IPAddress.Loopback port ]
        listenTimeout = TimeSpan.FromMilliseconds 3000. }
  startWebServer config app
  0