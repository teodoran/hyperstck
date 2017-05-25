module StckTests

open Expecto

open FsCheck

open Stck

//type X = X of int 
//type A() = 
//  static member M() = gen { let! i = Arb.value in if tst i then yield X i } 
// testPWC {cfg w arbs=[tof<A>]} (fun (MyT i) -> ..


[<Tests>]
let tests =
  testList "Stck tests" [

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

      testProperty "drops drop the first value from the stack" <| fun (stack : int list) ->
        match stack with 
        | [] -> ()
        | h :: t -> 
          let stack' = drop stack
          Expect.equal stack' t "drop should drop the first element from the stack"
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