module StckTests.TopTests

open Expecto
open FsCheck

open Stck

[<Tests>]
let tests =
  testList "top tests" [
    testProperty "top returns the top element on the stack" <| fun (stack : int list) ->
      match stack with 
      | [] -> ()
      | h :: _ ->
        Expect.equal (top stack) h "top should return the top element on the stack" 
  ]
