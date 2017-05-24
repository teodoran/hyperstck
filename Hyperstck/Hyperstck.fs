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

open Hyperhelp
open Stck

let isOpName (s : string) =
  s |> Seq.forall Char.IsLetter

let createOplink input name = 
  sprintf "<a href=\"/%s/%s\">%s</a>" input name name

let recreate (s : string) : string list = 
  let things = s.Split(['/'] |> List.toArray) |> List.ofArray
  things |> List.rev

let decreate (st : int list) : string = 
  let resultStackStrings = st |> List.map (sprintf "%d")
  let forward = resultStackStrings |> List.rev
  String.concat "/" forward

let handleNumRequestWithStack s = request (fun r ->
  match r.queryParam "n" with
  | Choice1Of2 nstr ->
    let (isNumber, n) = Int32.TryParse nstr
    if isNumber then 
      let stack = s |> recreate |> List.map Int32.Parse
      let stack' = (num n) stack 
      let s' = decreate stack'
      let location = sprintf "/%s" s'
      printfn "Redirect to %s" location
      FOUND "" >=> setHeader "location" location
    else
      BAD_REQUEST <| sprintf "Not a number: %s" nstr
  | Choice2Of2 x -> 
    let bodyDiv = sprintf "<form action=\"/%s/num\" method=\"get\"><input type=\"text\" name=\"n\" /><input type=\"submit\" value=\"Submit\"></form>" s
    let html = sprintf "<html><style>body { font-family: consolas; }</style><body>%s</body></html>" bodyDiv
    OK html
  )

let handleNumRequestWithoutStack = handleNumRequestWithStack ""

let handleRequest (input : string) = request (fun r ->
  printfn "handleRequest %s" input
  let things = input.Split(['/'] |> List.toArray) |> List.ofArray
  let backwards = things |> List.rev
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
    printfn "just stuff %s" <| (stuff |> String.concat "/")
    let oplinkList = availableOperations |> List.map (createOplink input)
    let oplinkListItems = oplinkList |> List.map (fun a -> sprintf "<li>%s</li>" a)
    let oplinksDiv = sprintf "<div><ul>%s</ul></div>" <| String.concat "" oplinkListItems
    let html = sprintf "<html><style>body { font-family: consolas; }</style><body>%s</body></html>" oplinksDiv
    OK html >=> setHeader "Pragma" "no-cache" >=> setHeader "Content-Type" "text/html; charset=utf-8")

let app : WebPart = 
  choose [ 
      GET >=> path "/num" >=> handleNumRequestWithoutStack
      GET >=> pathScan "/%s/num" handleNumRequestWithStack
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