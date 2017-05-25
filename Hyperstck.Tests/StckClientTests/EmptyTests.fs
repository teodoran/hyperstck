module StckClientTests.EmptyTests

open Expecto
open FsCheck

open StckClient

[<Tests>]
let tests =
  testList "empty tests" [

    testCase "simple empty test (empty stack)" <| fun _ ->
      let stack = []
      let stackop = lookupOp "empty" |> fun op -> op.op
      let stack' = stackop stack
      Expect.equal stack' [1] "empty should push 1 onto the stack if the stack is empty"

    testCase "simple empty test (non-empty stack)" <| fun _ ->
      let stack = [3; 2]
      let stackop = lookupOp "empty" |> fun op -> op.op
      let stack' = stackop stack
      Expect.equal stack' [0; 3; 2] "empty should push 0 onto the stack if the stack is empty"

    testCase "stated empty minsize is 0" <| fun _ ->
      let stackop = lookupOp "empty"
      Expect.equal stackop.minsize 0 "the stated minimum stack size for empty should be 0"

    testCase "stated empty effect is +1" <| fun _ ->
      let stackop = lookupOp "empty"
      Expect.equal stackop.effect 1 "the stated effect of empty should be +1 elements on the stack"

  ]
