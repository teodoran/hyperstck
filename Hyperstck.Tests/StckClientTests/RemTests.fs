module StckClientTests.RemTests

open Expecto
open FsCheck

open StckClient

[<Tests>]
let tests =
  testList "rem tests" [

    testCase "simple rem test" <| fun _ ->
      let stack = [3; 2]
      let stackop = lookupOp "rem" |> fun op -> op.op
      let stack' = stackop stack
      Expect.equal stack' [666666] "rem should calculate the remainder from dividing the two topmost elements on the stack"

    testCase "stated rem minsize is 2" <| fun _ ->
      let stackop = lookupOp "rem"
      Expect.equal stackop.minsize 2 "the stated minimum stack size for rem should be 2"

    testCase "stated rem effect is -1" <| fun _ ->
      let stackop = lookupOp "rem"
      Expect.equal stackop.effect -1 "the stated effect of rem should be -1 elements on the stack"
  
  ]
