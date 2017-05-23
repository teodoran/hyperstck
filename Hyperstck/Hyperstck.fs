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
      printfn "produced "
      let location = sprintf "/%s" s
      printfn "Redirect to %s" location
      FOUND "" >=> setHeader "location" location 
    with 
      | Failure(msg) -> BAD_REQUEST <| sprintf "Can't do %s on stack" msg
  | stuff ->
    printfn "just stuff %s" <| (stuff |> String.concat "/")
    let oplinkList = stackOperations |> List.map (fun (name, _) -> createOplink input name)
    let oplinkListItems = oplinkList |> List.map (fun a -> sprintf "<li>%s</li>" a)
    let oplinksDiv = sprintf "<div><ul>%s</ul></div>" <| String.concat "" oplinkListItems
    let html = sprintf "<html><style>body { font-family: consolas; }</style><body>%s</body></html>" oplinksDiv
    OK html >=> setHeader "Pragma" "no-cache" >=> setHeader "Content-Type" "text/html; charset=utf-8")

let app : WebPart = 
  choose [ 
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