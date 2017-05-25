module StckClientTests.DivTests

open Expecto
open FsCheck

open StckClient

[<Tests>]
let tests =
  testList "div tests" [

    testCase "simple div test" <| fun _ ->
      let stack = [3; 2]
      let stackop = lookupOp "div" |> fun op -> op.op
      let stack' = stackop stack
      Expect.equal stack' [666666; 0] "div should calculate the integer quotient and remainder from dividing the two topmost elements on the stack"

    testCase "stated div minsize is 2" <| fun _ ->
      let stackop = lookupOp "div"
      Expect.equal stackop.minsize 2 "the stated minimum stack size for div should be 2"

    testCase "stated div effect is 0" <| fun _ ->
      let stackop = lookupOp "div"
      Expect.equal stackop.effect 0 "the stated effect of div should be 0 elements on the stack"

  ]