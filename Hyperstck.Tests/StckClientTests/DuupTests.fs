module StckClientTests.DuupTests

open Expecto

open FsCheck

open StckClient

[<Tests>]
let tests =
  testList "duup tests" [

    testCase "simple duup test" <| fun _ ->
      let stack = [1; 2; 3]
      let duup = lookupOp "duup" |> fun op -> op.op
      let stack' = duup stack
      Expect.equal stack' [1; 2; 1; 2; 3] "duup should duplicate the two topmost elements on the stack"

    testCase "stated duup minsize is 0" <| fun _ ->
      let stackop = lookupOp "duup"
      Expect.equal stackop.minsize 2 "the stated minimum stack size for duup should be 2"

    testCase "stated duup effect is +2" <| fun _ ->
      let stackop = lookupOp "duup"
      Expect.equal stackop.effect 2 "the stated effect of duup should be +2 elements on the stack"
  
  ]