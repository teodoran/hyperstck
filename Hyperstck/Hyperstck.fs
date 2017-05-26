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

let setLocationHeader = setHeader "location"

let inDiv = sprintf "<div>%s</div>"

let isOpName (s : string) =
  s |> Seq.forall Char.IsLetter

let createOplink (input : string) name = 
  let input' = if input.Length = 0 then "" else "/" + input
  let href = sprintf "%s/%s" input' name
  let result = sprintf "<a href=\"%s\">%s</a>" href name
  result

let createDoclink (input : string) name = 
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

let handleNumRequest s = request (fun r ->
  printfn "handleNumRequest %s" s
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

let handleIfRequest (stk : string) = request (fun r ->
  let stack = stk |> toList
  let stackSize = stack |> List.length
  let fullpath = if stk.Length = 0 then "if" else sprintf "%s/if" stk  
  let linkList = availableNames stackSize |> List.map (createOplink fullpath)
  let linkListItems = linkList |> List.map (fun a -> sprintf "<li>%s</li>" a)
  let linksDiv = sprintf "<div><ul>%s</ul></div>" <| String.concat "" linkListItems
  let headerDiv = "Conditional" |> inDiv
  let ifDiv = inDiv "[] ?"
  let html = sprintf "<html><style>body { font-family: consolas; }</style><body>%s%s%s</body></html>" headerDiv ifDiv linksDiv
  OK html >=> setHeader "Pragma" "no-cache" >=> setHeader "Content-Type" "text/html; charset=utf-8")

let handleInsideIfRequest (stk : string, cond : string) = request (fun r ->
  let stack = stk |> toList
  let stackSize = stack |> List.length
  let path = if stk.Length = 0 then "if" else sprintf "%s/if" stk  
  let fullpath = sprintf "%s/%s" path cond
  let linkList = "then" :: availableNames stackSize |> List.map (createOplink fullpath)
  let linkListItems = linkList |> List.map (fun a -> sprintf "<li>%s</li>" a)
  let linksDiv = sprintf "<div><ul>%s</ul></div>" <| String.concat "" linkListItems
  let headerDiv = "Conditional" |> inDiv
  let ifDiv = cond |> toList |> String.concat " " |> sprintf "[ %s ] ?" |> inDiv
  let html = sprintf "<html><style>body { font-family: consolas; }</style><body>%s%s%s</body></html>" headerDiv ifDiv linksDiv
  OK html >=> setHeader "Pragma" "no-cache" >=> setHeader "Content-Type" "text/html; charset=utf-8")

let handleThenRequest (stk : string, cond : string) = request (fun r ->
  let stack = stk |> toList
  let stackSize = stack |> List.length
  let pathstart = if stk.Length = 0 then "" else sprintf "%s/" stk 
  let fullpath = sprintf "%sif/%s/then" pathstart cond  
  let linkList = availableNames stackSize |> List.map (createOplink fullpath)
  let linkListItems = linkList |> List.map (fun a -> sprintf "<li>%s</li>" a)
  let linksDiv = sprintf "<div><ul>%s</ul></div>" <| String.concat "" linkListItems
  let headerDiv = "Conditional" |> inDiv
  let ifDiv = cond |> toList |> String.concat " " |> sprintf "[ %s ] ?" |> inDiv
  let thenDiv = "[] :" |> inDiv
  let html = sprintf "<html><style>body { font-family: consolas; }</style><body>%s%s%s%s</body></html>" headerDiv ifDiv thenDiv linksDiv
  OK html >=> setHeader "Pragma" "no-cache" >=> setHeader "Content-Type" "text/html; charset=utf-8")

let handleInsideThenRequest (stk : string, cond : string, aye : string) = request (fun r ->
  let stack = stk |> toList
  let stackSize = stack |> List.length
  let path = if stk.Length = 0 then "if" else sprintf "%s/if" stk  
  let fullpath = sprintf "%s/%s/then/%s" path cond aye
  let linkList = "else" :: availableNames stackSize |> List.map (createOplink fullpath)
  let linkListItems = linkList |> List.map (fun a -> sprintf "<li>%s</li>" a)
  let linksDiv = sprintf "<div><ul>%s</ul></div>" <| String.concat "" linkListItems
  let headerDiv = "Conditional" |> inDiv
  let ifDiv = cond |> toList |> String.concat " " |> sprintf "[ %s ] ?" |> inDiv
  let thenDiv = aye |> toList |> String.concat " " |> sprintf "[ %s ] :" |> inDiv
  let html = sprintf "<html><style>body { font-family: consolas; }</style><body>%s%s%s%s</body></html>" headerDiv ifDiv thenDiv linksDiv
  OK html >=> setHeader "Pragma" "no-cache" >=> setHeader "Content-Type" "text/html; charset=utf-8")

let handleElseRequest (stk : string, cond : string, aye : string) = request (fun r ->
  let stack = stk |> toList
  let stackSize = stack |> List.length
  let path = if stk.Length = 0 then "if" else sprintf "%s/if" stk  
  let fullpath = sprintf "%s/%s/then/%s/else" path cond aye
  let linkList = "end" :: availableNames stackSize |> List.map (createOplink fullpath)
  let linkListItems = linkList |> List.map (fun a -> sprintf "<li>%s</li>" a)
  let linksDiv = sprintf "<div><ul>%s</ul></div>" <| String.concat "" linkListItems
  let headerDiv = "Conditional" |> inDiv
  let ifDiv = cond |> toList |> String.concat " " |> sprintf "[ %s ] ?" |> inDiv
  let thenDiv = aye |> toList |> String.concat " " |> sprintf "[ %s ] :" |> inDiv
  let elseDiv = inDiv "[]"
  let html = sprintf "<html><style>body { font-family: consolas; }</style><body>%s%s%s%s%s</body></html>" headerDiv ifDiv thenDiv elseDiv linksDiv
  OK html >=> setHeader "Pragma" "no-cache" >=> setHeader "Content-Type" "text/html; charset=utf-8")

let handleDefineRequest (s : string) = request (fun r ->
  match r.queryParam "name" with
  | Choice1Of2 name ->
    let stack' = if s.Length = 0 then sprintf "define/%s" name else sprintf "%s/define/%s" s name
    let location = sprintf "/%s" stack'
    printfn "Redirect to %s" location
    FOUND "" >=> setHeader "location" location 
  | Choice2Of2 x -> 
    let stuff = recreate s |> List.rev
    let formAction = if s = "" then "/define" else sprintf "/%s/define" s
    let bodyDiv = sprintf "<form action=\"%s\" method=\"get\">Subroutine name:<br /><input type=\"text\" name=\"name\" /><input type=\"submit\" value=\"Submit\"></form>" formAction
    let html = sprintf "<html><style>body { font-family: consolas; }</style><body>%s</body></html>" bodyDiv
    OK html)

let handleInsideDefineRequest (stk, def) = request (fun r ->
  printfn "handleInsideDefineRequest %s %s" stk def
  let stack = toList stk
  let definition = toList def
  let stackDiv = stack |> String.concat " " |> sprintf "<div>[ %s ]</div>"
  let stackSize = stack |> List.length
  let fullpath = if stk.Length = 0 then sprintf "define/%s" def else sprintf "%s/define/%s" stk def
  let oplinkList = "cancel" :: "if" :: allNames() |> List.map (createOplink fullpath)
  let oplinkListItems = oplinkList |> List.map (fun a -> sprintf "<li>%s</li>" a)
  let oplinksDiv = sprintf "<div><ul>%s</ul></div>" <| String.concat "" oplinkListItems
  let html = sprintf "<html><style>body { font-family: consolas; }</style><body>%s%s</body></html>" stackDiv oplinksDiv
  let nameDiv = sprintf "<div>Defining subroutine: %s</div>" <| List.head definition
  let bodyStr = List.tail definition |> String.concat " "
  let bodyDiv = sprintf "<div>[ %s ]</div>" bodyStr
  let formAction = if stk = "" then sprintf "/define/%s" def else sprintf "/%s/define/%s" stk def
  printfn "form action: %s" formAction
  let formDiv = sprintf "<form action=\"%s\" method=\"post\"><input type=\"submit\" value=\"Complete\"></form>" formAction
  let html = sprintf "<html><style>body { font-family: consolas; }</style><body>%s%s%s%s</body></html>" nameDiv bodyDiv formDiv oplinksDiv
  OK html >=> setHeader "Pragma" "no-cache" >=> setHeader "Content-Type" "text/html; charset=utf-8")

let handleNumInsideDefineRequest (stk : string, def : string) = request (fun r ->
  printfn "handleNumInsideDefineRequest %s %s" stk def
  let start = if stk.Length = 0 then "" else sprintf "/%s" stk
  match r.queryParam "n" with
  | Choice1Of2 nstr ->
    let (isNumber, n) = Int32.TryParse nstr
    if isNumber then 
      printfn "Got number '%d'" n
      let location = sprintf "%s/define/%s/num/%d" start def n 
      printfn "Redirect to %s" location
      FOUND "" >=> setHeader "location" location
    else
      BAD_REQUEST <| sprintf "Not a number: %s" nstr
  | Choice2Of2 x -> 
    let stuff = toList stk
    let stackDiv = 
      if stuff.Length = 0 then 
        "<div>[]</div>"
      else 
        stuff |> String.concat " " |> sprintf "<div>[ %s ]</div>"
    let formAction = sprintf "%s/define/%s/num" start def
    printfn "formAction? %s" formAction
    let bodyDiv = sprintf "<form action=\"%s\" method=\"get\"><input type=\"text\" name=\"n\" /><input type=\"submit\" value=\"Submit\"></form>" formAction
    let html = sprintf "<html><style>body { font-family: consolas; }</style><body>%s</body></html>" bodyDiv
    OK html)

let handleCancelDefineRequest (stk : string, def : string) = request (fun r ->
  let location = sprintf "/%s" stk
  FOUND "" >=> setLocationHeader location)

let handlePostSubroutineDefinionRequest (stk, def) = request (fun r ->
  printfn "handlePostSubroutineDefinionRequest %s %s" stk def
  let stack = toList stk
  let definition = toList def
  match definition with 
  | [] -> BAD_REQUEST "Empty definition"
  | [n] -> BAD_REQUEST <| sprintf "Just the name %s doesn't do anything." n
  | name :: body ->
    defineSubroutine name body
    let stackDiv = stack |> String.concat " " |> sprintf "<div>[ %s ]</div>"
    let stackSize = stack |> List.length
    let oplinkList = "define" :: "if" :: availableNames stackSize |> List.map (createOplink stk)
    let oplinkListItems = oplinkList |> List.map (fun a -> sprintf "<li>%s</li>" a)
    let oplinksDiv = sprintf "<div><ul>%s</ul></div>" <| String.concat "" oplinkListItems
    let html = sprintf "<html><style>body { font-family: consolas; }</style><body>%s%s</body></html>" stackDiv oplinksDiv
    let location = sprintf "/%s" stk
    printfn "Redirect to %s" location
    SEE_OTHER "" >=> setHeader "location" location)

let handleRequest (stk : string) = request (fun r ->
  printfn "handleRequest '%s'" stk
  let start = if stk.Length = 0 then "" else sprintf "/%s" stk
  let backwards = recreate stk
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
    let docsHref = sprintf "%s/docs" start
    let docsLink = sprintf "<a href=\"%s\">docs</a>" docsHref
    let oplinkList = "define" :: "if" :: availableNames stackSize |> List.map (createOplink stk)
    let linkList = docsLink :: oplinkList 
    let linkListItems = linkList |> List.map (fun a -> sprintf "<li>%s</li>" a)
    let linksDiv = sprintf "<div><ul>%s</ul></div>" <| String.concat "" linkListItems
    let start = if stk.Length = 0 then "" else sprintf "/%s" stk
    let html = sprintf "<html><style>body { font-family: consolas; }</style><body>%s%s</body></html>" stackDiv linksDiv
    OK html >=> setHeader "Pragma" "no-cache" >=> setHeader "Content-Type" "text/html; charset=utf-8")

let handleEmptyRequest = request (fun r ->
  let docsHref = "/docs"
  let docsLink = sprintf "<a href=\"%s\">docs</a>" docsHref
  let oplinkList = "define" :: "if" :: availableNames 0 |> List.map (createOplink "")
  let linkList = docsLink :: oplinkList 
  let linkListItems = linkList |> List.map (fun a -> sprintf "<li>%s</li>" a)
  let linksDiv = sprintf "<div><ul>%s</ul></div>" <| String.concat "" linkListItems
  let stackDiv = "<div>[]</div>"
  let html = sprintf "<html><style>body { font-family: consolas; }</style><body>%s%s</body></html>" stackDiv linksDiv
  OK html >=> setHeader "Pragma" "no-cache" >=> setHeader "Content-Type" "text/html; charset=utf-8")

let handleOpDocRequest stk opname minsize effect = 
  let nameStr = sprintf "Subroutine: %s" opname
  let minstackStr = sprintf "Required stack size: %d" minsize
  let effectStr = 
    if effect = 0 then
      "Does not change the size of the stack"
    else if effect > 0 then
      sprintf "Increases the size of the stack by %d" effect
    else 
      sprintf "Reduces the size of the stack by %d" (0 - effect)
  let stackHref = sprintf "/%s" stk
  let stackLink = sprintf "<a href=\"%s\">stack</a>" stackHref
  let start = if stk.Length = 0 then "" else sprintf "/%s" stk
  let docsHref = sprintf "%s/docs" start
  let docsLink = sprintf "<a href=\"%s\">docs</a>" docsHref
  let html = sprintf "<html><style>body { font-family: consolas; }</style><body>%s%s%s%s%s</body></html>" (inDiv nameStr) (inDiv minstackStr) (inDiv effectStr) (inDiv docsLink) (inDiv stackLink)
  OK html

let handleShowOpRequest (stk, opname) = request (fun r ->
  if opname = "num" then 
    handleOpDocRequest stk "num" 0 1  
  else
    let op = lookupOp opname
    handleOpDocRequest stk opname op.minsize op.effect)

let handleDocsRequest (stk : string) = request (fun r -> 
  let fullpath = if stk.Length = 0 then "docs" else sprintf "%s/docs" stk  
  let doclinks = allNames() |> List.map (createDoclink fullpath)
  let doclinkListItems = doclinks |> List.map (fun a -> sprintf "<li>%s</li>" a)
  let doclinksDiv = sprintf "<div><ul>%s</ul></div>" <| String.concat "" doclinkListItems
  let html = sprintf "<html><style>body { font-family: consolas; }</style><body>%s</body></html>" doclinksDiv
  printfn "links %A" doclinks
  OK html)

let app : WebPart = 
  choose [ 
      GET >=> path "/docs" >=> handleDocsRequest ""
      GET >=> pathScan "/%s/docs" handleDocsRequest 
      GET >=> pathScan "/docs/%s" (fun opname -> handleShowOpRequest ("", opname))
      GET >=> pathScan "/%s/docs/%s" handleShowOpRequest
      GET >=> pathScan "/define/%s/cancel" (fun def -> handleCancelDefineRequest ("", def))
      GET >=> pathScan "/%s/define/%s/cancel" handleCancelDefineRequest
      GET >=> pathScan "/define/%s/num" (fun def -> handleNumInsideDefineRequest ("", def))
      GET >=> pathScan "/%s/define/%s/num" handleNumInsideDefineRequest
      GET >=> pathScan "/define/%s" (fun def -> handleInsideDefineRequest ("", def))
      GET >=> pathScan "/%s/define/%s" handleInsideDefineRequest
      GET >=> path "/define" >=> handleDefineRequest ""
      GET >=> pathScan "/%s/define" handleDefineRequest
      GET >=> pathScan "/if/%s/then/%s/else" (fun (cond, aye) -> handleElseRequest ("", cond, aye))
      GET >=> pathScan "/%s/if/%s/then/%s/else" handleElseRequest
      GET >=> pathScan "/if/%s/then/%s" (fun (cond, aye) -> handleInsideThenRequest ("", cond, aye))
      GET >=> pathScan "/%s/if/%s/then/%s" handleInsideThenRequest
      GET >=> pathScan "/if/%s/then" (fun cond -> handleThenRequest ("", cond))
      GET >=> pathScan "/%s/if/%s/then" handleThenRequest
      GET >=> pathScan "/if/%s" (fun cond -> handleInsideIfRequest ("", cond))
      GET >=> pathScan "/%s/if/%s" handleInsideIfRequest
      GET >=> path "/if" >=> handleIfRequest ""
      GET >=> pathScan "/%s/if" handleIfRequest
      GET >=> path "/num" >=> handleNumRequest ""
      GET >=> pathScan "/%s/num" handleNumRequest
      GET >=> path "/" >=> handleEmptyRequest
      GET >=> pathScan "/%s" handleRequest 
      POST >=> pathScan "/define/%s" (fun def -> handlePostSubroutineDefinionRequest ("", def))
      POST >=> pathScan "/%s/define/%s" handlePostSubroutineDefinionRequest
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