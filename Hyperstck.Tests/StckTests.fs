module LolStckTests

open Expecto

open FsCheck

open Stck

[<Tests>]
let tests =
  testList "Stck tests" [

    testList "top tests" [
      testProperty "top returns the top element on the stack" <| fun (stack : int list) ->
        match stack with 
        | [] -> ()
        | h :: _ ->
          Expect.equal (top stack) h "top should  return the top element on the stack" 
    ]

    testList "push tests" [
      testProperty "push effect is +1" <| fun (num : int) (stack : int list) ->
        let stack' = push num stack
        Expect.equal stack'.Length (stack.Length + 1) "push should increase stack size by 1"

      testProperty "push puts the new value on top of the stack" <| fun (num : int) (stack : int list) ->
        let stack' = push num stack
        Expect.equal stack'.Head num  "push should put the new value on top of the stack"
    ]

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

    testList "swap tests" [
      testProperty "swap effect is 0" <| fun (stack : int list) ->
        match stack with 
        | [] -> ()
        | h :: t -> 
          let stack' = swap stack
          Expect.equal stack'.Length stack.Length "swap should not change stack size"

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

]