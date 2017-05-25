module StckTests.DropTests

open Expecto
open FsCheck

open Stck

[<Tests>]
let tests =
  testList "drop tests" [
    testProperty "drop effect is -1" <| fun (stack : int list) ->
      match stack with 
      | [] -> ()
      | h :: t -> 
        let stack' = drop stack
        Expect.equal stack'.Length (stack.Length - 1) "drop should decrease stack size by 1"

    testProperty "drop drops the first value from the stack" <| fun (stack : int list) ->
      match stack with 
      | [] -> ()
      | h :: t -> 
        let stack' = drop stack
        Expect.equal stack' t "drop should drop the first element from the stack"
      
    testCase "cannot drop with empty stack" <| fun () -> 
      let f = fun () -> drop [] |> ignore
      Expect.throws f "drop should throw when the stack is empty"

    testCase "cannot drop with stack size 1" <| fun () -> 
      let f = fun () -> drop [1] |> ignore
      Expect.throws f "drop should throw when the stack has one element"
  ]