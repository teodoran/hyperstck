module StckTests.PushTests

open Expecto
open FsCheck

open Stck

[<Tests>]
let tests =
  testList "push tests" [
    testProperty "push effect is +1" <| fun (num : int) (stack : int list) ->
      let stack' = push num stack
      Expect.equal stack'.Length (stack.Length + 1) "push should increase stack size by 1"

    testProperty "push puts the new value on top of the stack" <| fun (num : int) (stack : int list) ->
      let stack' = push num stack
      Expect.equal stack'.Head num  "push should put the new value on top of the stack"
  ]
