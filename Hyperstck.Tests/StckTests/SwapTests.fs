module StckTests.SwapTests

open Expecto
open FsCheck

open Stck

[<Tests>]
let tests =
  testList "swap tests" [
    testProperty "swap effect is 0" <| fun (stack : int list) ->
      match stack with 
      | a :: b :: t -> 
        let stack' = swap stack
        Expect.equal stack'.Length stack.Length "swap should not change stack size"
      | _ -> ()

    testProperty "swap should swap the two first elements on the stack" <| fun (stack : int list) ->
      match stack with 
      | a :: b :: _ -> 
        match swap stack with 
        | x :: y :: _ ->
          Expect.isTrue (a = y && b = x) "swap should swap the two first elements on the stack"
        | _ ->
          Tests.failtest "swap shouldn't consume anything from the stack"
      | _ -> ()

    testCase "cannot swap with empty stack" <| fun () -> 
      let f = fun () -> swap [] |> ignore
      Expect.throws f "swap should throw when the stack is empty"

    testCase "cannot swap with stack size 1" <| fun () -> 
      let f = fun () -> swap [1] |> ignore
      Expect.throws f "swap should throw when the stack has one element"
  ]
