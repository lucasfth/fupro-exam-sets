module Exam2022
(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but 
   it does allow you to work in interactive mode and you can just remove the '=' 
   to make the project compile again.

   You will also need to load JParsec.fs. Do this by typing
   #load "JParsec.fs" 
   in the interactive environment. You may need the entire path.

   Do not remove the module declaration (even though that does work) because you may inadvertently
   introduce indentation errors in your code that may be hard to find if you want
   to switch back to project mode. 

   Alternative, keep the module declaration as is, but load ExamInteractive.fsx into the interactive environment
   *)
(*
 module Exam2022 = 
 *)

(* 1: Grayscale images *)

type grayscale =
    | Square of uint8
    | Quad of grayscale * grayscale * grayscale * grayscale

let img =
    Quad(Square 255uy, Square 128uy, Quad(Square 255uy, Square 128uy, Square 192uy, Square 64uy), Square 0uy)

(* Question 1.1 *)
let rec countWhite img =
    match img with
    | Square sq -> if sq = 255uy then 1 else 0
    | Quad(a, b, c, d) -> countWhite a + countWhite b + countWhite c + countWhite d

(* Question 1.2 *)
let rec rotateRight img =
    match img with
    | Square sq -> Square sq
    | Quad(a, b, c, d) -> Quad(rotateRight d, rotateRight a, rotateRight b, rotateRight c)

(* Question 1.3 *)
let rec map mapper img =
    match img with
    | Square sq -> mapper sq
    | Quad(a, b, c, d) ->
        let mapA = map mapper a
        let mapB = map mapper b
        let mapC = map mapper c
        let mapD = map mapper d
        Quad(mapA, mapB, mapC, mapD)

let bitmap img =
    map (fun x -> if x <= 127uy then Square 0uy else Square 255uy) img

(* Question 1.4 *)

let rec fold folder acc img =
    match img with
    | Square sq -> folder acc sq
    | Quad(a, b, c, d) ->
        let foldA = fold folder acc a
        let foldB = fold folder acc b
        let foldC = fold folder acc c
        let foldD = fold folder acc d
        foldA + foldB + foldC + foldD

let countWhite2 img =
    fold (fun acc sq -> if sq = 255uy then acc + 1 else acc) 0 img

(* 2: Code Comprehension *)
let rec foo =
    function
    | 0 -> ""
    | x when x % 2 = 0 -> foo (x / 2) + "0"
    | x when x % 2 = 1 -> foo (x / 2) + "1"

let rec bar =
    function
    | [] -> []
    | x :: xs -> (foo x) :: (bar xs)

(* Question 2.1 *)

(* 
    
    Q: What are the types of functions foo and bar?

    A: <Your answer goes here>
    The type of foo is int -> string
    The type of bar is list<int> -> list<string>


    Q: What does the function bar do.
       Focus on what it does rather than how it does it.

    A: <Your answer goes here>
    foo converts a decimal represented by an int to its binary representation as a string
    bar convests a list of decimals represented by ints to a list of their binary representations as strings
    
    Q: What would be appropriate names for functions 
       foo and bar?

    A: <Your answer goes here>
    An appropriate name for foo would be intToBinary
    An appropriate name for bar would be intListToBinaryList or listToBinary
        
    Q: The function foo does not return reasonable results for all possible inputs.
       What requirements must we have on the input to foo in order to get reasonable results?
    
    A: <Your answer goes here>
    The int can neither a negative number nor zero.
    Therefore it has to be a real number.
    *)


(* Question 2.2 *)


(* 
    The function foo compiles with a warning. 
    
    Q: What warning and why?

    A: <Your answer goes here>
    The warnings says:
        Incomplete pattern matches on this expression. 
        For example, the value '1' may indicate a case not covered by the pattern(s). 
        However, a pattern rule with a 'when' clause might successfully match this value.F# Compiler25
    
    This is due to the compiler not checking the boolean conditions and therefore does not know if it is a complete pattern match.

    *)

let rec foo2 =
    function
    | 0 -> ""
    | x when x % 2 = 0 -> foo (x / 2) + "0"
    | x -> foo (x / 2) + "1" // This could potentially be problematic since it would also catch the negative numbers
// Another possible solutions would be to have the same code as foo and then have another match on '_' and then failwith "Negative number"


(* Question 2.3 *)

let bar2 lst = List.map (fun elem -> foo2 elem) lst

(* Question 2.4 *)

(*

    Q: Neither foo nor bar is tail recursive. Pick one (not both) of them and explain why.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation.
       You need to make clear what aspects of the evaluation tell you that the function is not tail recursive.
       Keep in mind that all steps in an evaluation chain must evaluate to the same value
       ((5 + 4) * 3 --> 9 * 3 --> 27, for instance).

    A: <Your answer goes here>
    I am going to explain foo.
    I am going to call foo with 5
    = foo 5
    = (foo (5/2)) + 1
    = (foo (2)) + 1
    = ((foo (2/2)) + 0) + 1
    = ((foo (1)) + 0) + 1
    = (((foo (1/2)) + 1) + 0) + 1
    = ((("") + 1) + 0) + 1
    = (("" + 1) + 0) + 1
    = (1 + 0) + 1
    = 10 + 1
    = 101
    
    Q: Even though neither `foo` nor `bar` is tail recursive only one of them runs the risk of overflowing the stack.
       Which one and why does  the other one not risk overflowing the stack?

    A: <Your answer goes here>
    Since foo takes a single int (which a 32 bit int) it will only at maximum be called with 2^32 big number.
    Then foo will make at most log(2^32) which evaluates to about 10 calls.
    This is far below the stack size and it will therefore never run the risk of stack overflow.

    Bar does run the risk of stack overflow. 
    This is due to every element in the list being appended to the stack (since bar is not tailrecursive).
    It can therefore given a big enough list get a stack overflow.

    *)
(* Question 2.5 *)

let fooTail x =
    let rec aux x acc =
        match x with
        | 0 -> acc
        | x when x % 2 = 0 -> aux (x / 2) ("0" + acc)
        | x -> aux (x / 2) ("1" + acc)

    aux x ""

(* Question 2.6 *)
let barTail lst =
    let rec aux x c =
        match x with
        | [] -> c []
        | x :: xs -> aux xs (fun r -> c (fooTail x :: r))

    aux lst id


(* 3: Matrix operations *)

type matrix = int[,]

let init f rows cols = Array2D.init rows cols f

let numRows (m: matrix) = Array2D.length1 m
let numCols (m: matrix) = Array2D.length2 m

let get (m: matrix) row col = m.[row, col]
let set (m: matrix) row col v = m.[row, col] <- v

let print (m: matrix) =
    for row in 0 .. numRows m - 1 do
        for col in 0 .. numCols m - 1 do
            printf "%d\t" (get m row col)

        printfn ""

(* Question 3.1 *)

let failDimensions m1 m2 =
    failwith (
        sprintf
            "Invalid matrix dimensions: m1 rows = %A, m1 columns = %A, m2 rows = %A, m2 columns = %A"
            (numRows m1)
            (numCols m1)
            (numRows m2)
            (numCols m2)
    )

(* Question 3.2 *)

let add m1 m2 =
    if (numRows m1 <> numRows m2 || numCols m1 <> numCols m2) then
        failDimensions m1 m2
    else
        init (fun x y -> get m1 x y + get m2 x y) (numRows m1) (numCols m1)

(* Question 3.3 *)

let m1 = (init (fun i j -> i * 3 + j + 1) 2 3)
let m2 = (init (fun j k -> j * 2 + k + 1) 3 2)


let dotProduct m1 m2 m1Row m2Col =
    let counter = [ 0 .. (numRows m2 - 1) ]
    List.fold (fun acc length -> (get m1 m1Row length * get m2 length m2Col) + acc) 0 counter


let mult m1 m2 =
    if (numRows m1 <> numCols m2 || numCols m1 <> numRows m2) then
        failDimensions m1 m2
    else
        init (fun x y -> dotProduct m1 m2 (x) (y)) (numRows m1) (numCols m2)

(* Question 3.4 *)
let parInit f rows cols =
    let m = Array2D.init rows cols (fun row col -> 0)
    let lst = List.init (rows * cols) (fun index -> (index / cols, index % cols))

    List.map (fun (row, col) -> async { do set m row col (f row col) }) lst
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore

    m


// let init f rows cols = Array2D.init rows cols f

(* 4: Stack machines *)

type cmd =
    | Push of int
    | Add
    | Mult

type stackProgram = cmd list

(* Question 4.1 *)

type stack = int list
let emptyStack () = stack.Empty

(* Question 4.2 *)

let runStackProg prog =
    let rec aux prog acc =
        match prog with
        | [] ->
            match acc with
            | [] -> failwith "empty stack"
            | x :: _ -> x
        | x :: xs ->
            match x with
            | Push i -> aux xs (i :: acc)
            | Add ->
                match acc with
                | a :: b :: rest -> aux xs (a + b :: rest)
                | _ -> failwith "empty stack"
            | Mult ->
                match acc with
                | a :: b :: rest -> aux xs (a * b :: rest)
                | _ -> failwith "empty stack"

    aux prog (emptyStack ())

(* Question 4.3 *)

type StateMonad<'a> = SM of (stack -> ('a * stack) option)

let ret x = SM(fun s -> Some(x, s))
let fail = SM(fun _ -> None)

let bind f (SM a) : StateMonad<'b> =
    SM(fun s ->
        match a s with
        | Some(x, s') ->
            let (SM g) = f x
            g s'
        | None -> None)

let (>>=) x f = bind f x
let (>>>=) x y = x >>= (fun _ -> y)

let evalSM (SM f) = f (emptyStack ())

// -------------- Is correct until here --------------

let push _ = failwith "not implemented"
let pop _ = failwith "not implemented"

(* Question 4.4 *)

type StateBuilder() =

    member this.Bind(f, x) = bind x f
    member this.Return(x) = ret x
    member this.ReturnFrom(x) = x
    member this.Combine(a, b) = a >>= (fun _ -> b)

let state = new StateBuilder()

let runStackProg2 _ = failwith "not implemented"

(* Question 4.5 *)

open JParsec.TextParser

let parseStackProg _ = failwith "not implemented"
