module StckTests.AddTests

open Expecto
open FsCheck

open Stck

[<Tests>]
let tests =
  testList "add tests" [
    testProperty "add effect is -1" <| fun (stack : int list) ->
      match stack with 
      | a :: b :: t -> 
        let stack' = add stack
        Expect.equal stack'.Length (stack.Length - 1) "add should decrease stack size by 1"
      | _ -> ()

    testProperty "add performs addition on the two first values from the stack" <| fun (stack : int list) ->
      match stack with 
      | a :: b :: t -> 
        let topval = add stack |> List.head
        Expect.equal topval (a + b) "add should add the two first values on the stack"
      | _ -> ()
  ]