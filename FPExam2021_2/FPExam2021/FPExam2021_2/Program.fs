open System
open Exam2021_2

let testQ1 () =
    (* Testsfor Q1.1 *)
    printfn "Testing Question 1"

    printfn "%A" (length Nil)
    printfn "%A" (length (Cons1(3, Cons2(true, Cons1(4, Cons2(false, Nil))))))


    ()

let testQ2 () =
    printfn "Testing Question 2"
    // place debug prints for Q2 here
    ()

let testQ3 () =
    printfn "Testing Question 3"
    printfn ""
    printfn "%A" (solveQuadratic "-4x^2 - 5x + 6 = 0" 5)
    printfn "%A" (solveQuadratic "-4x^2 - 5x+ 6= 0" 5)
    printfn "%A" (solveQuadratic "-4x^2-5x+6=0" 5)
    printfn "%A" (solveQuadratic "-4x^3 - 5x + 6 = 0" 5)
    printfn "%A" (solveQuadratic "-4x^2 - 5x + 6 = 0 Hello World" 5)
    ()

let testQ4 () =
    printfn "Testing Question 4"
    // place debug prints for Q4 here
    ()

[<EntryPoint>]
let main argv =
    testQ3 ()
    0 // return an integer exit code
