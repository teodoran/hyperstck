module StckClientTests

open Expecto

open FsCheck

open StckClient


[<Tests>]
let tests =
  testList "StckClient tests" [

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

    testList "div tests" [

      testCase "simple div test" <| fun _ ->
        let stack = [3; 2]
        let stackop = lookupOp "div" |> fun op -> op.op
        let stack' = stackop stack
        Expect.equal stack' [0; 666666] "div should calculate the integer quotient and remainder from dividing the two topmost elements on the stack"

      testCase "stated div minsize is 2" <| fun _ ->
        let stackop = lookupOp "div"
        Expect.equal stackop.minsize 2 "the stated minimum stack size for div should be 2"

      testCase "stated div effect is -1" <| fun _ ->
        let stackop = lookupOp "div"
        Expect.equal stackop.effect -1 "the stated effect of div should be -1 elements on the stack"
    ]

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



]