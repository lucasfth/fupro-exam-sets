module Exam2021_2
(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but 
   it does allow you to work in interactive mode and you can just remove the '=' 
   to make the project compile again.

   You will also need to load JParsec.fs. Do this by typing
   #load "JParsec.fs" 
   in the interactive environment. You may need the entire path.

   Do not remove the module declaration (even though that does work) because you may inadvertantly
   introduce indentation errors in your code that may be hard to find if you want
   to switch back to project mode. 

   Alternative, keep the module declaration as is, but load ExamInteractive.fsx into the interactive environment
   *)
(*
 module Exam2021_2 = 
 *)

(* 1: Binary lists *)

(* Question 1.1 *)

type binList<'a, 'b> =
    | Nil
    | Cons1 of 'a * binList<'a, 'b>
    | Cons2 of 'b * binList<'a, 'b>

let rec length lst =
    match lst with
    | Nil -> 0
    | Cons1(_, tail) -> length tail + 1
    | Cons2(_, tail) -> length tail + 1

(* Question 1.2 *)
let split lst =
    let rec aux lst c1acc c2acc =
        match lst with
        | Nil -> (c1acc, c2acc)
        | Cons1(elem, tail) -> aux tail (elem :: c1acc) c2acc
        | Cons2(elem, tail) -> aux tail c1acc (elem :: c2acc)

    aux lst [] []

let length2 lst =
    let (l1, l2) = split lst
    let r1 = List.fold (fun acc _ -> acc + 1) 0 l1
    let r2 = List.fold (fun acc _ -> acc + 1) 0 l2
    (r1, r2)


(* Question 1.3 *)


let rec map f g lst =
    match lst with
    | Nil -> Nil
    | Cons1(elem, tail) -> Cons1(f elem, map f g tail)
    | Cons2(elem, tail) -> Cons2(g elem, map f g tail)

(* Question 1.4 *)

let rec filter f g lst =
    match lst with
    | Nil -> Nil
    | Cons1(elem, tail) ->
        match elem with
        | _ when f elem -> Cons1(elem, filter f g tail)
        | _ -> filter f g tail
    | Cons2(elem, tail) ->
        match elem with
        | _ when g elem -> Cons2(elem, filter f g tail)
        | _ -> filter f g tail

(* Question 1.5 *)

let rec fold f g acc lst =
    match lst with
    | Nil -> acc
    | Cons1(elem, tail) -> fold f g (f acc elem) tail
    | Cons2(elem, tail) -> fold f g (g acc elem) tail


(* 2: Code Comprehension *)
let rec foo xs ys =
    match xs, ys with
    | [], ys -> ys
    | xs, [] -> xs
    | x :: xs, y :: ys when x < y -> x :: (foo xs (y :: ys))
    | x :: xs, y :: ys -> y :: (foo (x :: xs) ys)

and bar =
    function
    | [] -> []
    | [ x ] -> [ x ]
    | xs ->
        let (a, b) = List.splitAt (List.length xs / 2) xs
        foo (bar a) (bar b)

(* Question 2.1 *)

(* 
    Q: What are the types of functions foo and bar?

    A: <Your answer goes here>
        foo is of type list<'a> -> list<'a> -> list<'a>
        bar is of type list<'a> -> list<'a>


    Q: What does the function bar do.
       Focus on what it does rather than how it does it.

    A: <Your answer goes here>
        foo merges and sorts two seperate sorted lists
        bar sorts a list

    Q: What would be appropriate names for functions 
       foo and bar?

    A: <Your answer goes here>
        An appropriate name for foo would be merge
        An appropriate name for bar would be mergeSort
    
    Q: What would be appropriate names of the values a and b in bar.
    
    A: <Your answer goes here>
        An appropriate name for a would be leftList or list1
        An appropriate name for b would be rightList or list2
    *)


(* Question 2.2 *)

(* 
    The code includes the keyword "and".
    
    Q: What function does this keyword serve in general
       (why would you use "and" when writing any program)?

    A: <Your answer goes here>
        The and keywords allows for mutual recursive functions.
        So it will allow the first function to call the second function and the second function call the first function.


    Q: What would happen if you removed it from this particular program and
       replaced it with a standard "let"
       (change the line "and bar = " to "let rec bar = ")?
       Explain why the program either does or does not work.

    A: <Your answer goes here>
        In this instance the and keyword can be replaced with the let rec keywords.
        This is due to foo only calling foo and bar calling foo and bar.
        If foo were also going to call bar it would need to have used the and keyword, to allow mutual recursion.
    *)

(* Question 2.3 *)
let foo2 xs ys =
    List.unfold
        (fun (l1, l2) ->
            match (l1, l2) with
            | [], y :: ys -> Some(y, ([], ys))
            | x :: xs, [] -> Some(x, (xs, []))
            | x :: xs, y :: _ when x < y -> Some(x, (xs, l2))
            | _, y :: ys -> Some(y, (l1, ys))
            | _ -> None)
        (xs, ys)

(* use the following code as a starting template
        let foo2 xs ys = List.unfold <a function goes here> (xs, ys)
    *)

(* Question 2.4 *)

(*

    Q: Neither foo nor bar is tail recursive. Pick one (not both) of them and explain why.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation.
       You need to make clear what aspects of the evaluation tell you that the function is not tail recursive.
       Keep in mind that all steps in an evaluation chain must evaluate to the same value
       ((5 + 4) * 3 --> 9 * 3 --> 27, for instance).

    A: <Your answer goes here>
        I choose foo.
        I call foo with foo [1;6] [5;7]
        = foo [1;6] [5;7]
        = 1 :: (foo [6] (5 :: [7]))
        = 1 :: (foo [6] [5;7])
        = 1 :: (5 :: (foo ([6] :: []) [7]))
        = 1 :: (5 :: (foo [6] [7]))
        = 1 :: (5 :: (6 :: (foo [] ([7] :: []))))
        = 1 :: (5 :: (6 :: (foo [] [7])))
        = 1 :: (5 :: (6 :: (7)))
        = 1 :: (5 :: ([6;7]))
        = 1 :: ([5;6;7])
        = [1;5;6;7]

        The reason for foo not being tail recursive is that it tries to append the recursive calls.
        This results in that the recursive calls have to be evaluated first for then to return the elements
        that needs to be appended to the list.
    *)
(* Question 2.5 *)

let fooTail l1 l2 =
    let rec aux l1 l2 acc =
        match l1, l2 with
        | [], ys -> acc @ ys
        | xs, [] -> acc @ xs
        | x :: xs, y :: _ when x < y -> aux xs l2 (acc @ [ x ])
        | _, y :: ys -> aux l1 ys (acc @ [ y ])

    aux l1 l2 []

(* Question 2.6 *)

let barTail lst =
    failwith "does not seem to be needed to make"
// let aux l1 l2 c =
//     match lst with
//     | [], [] -> c [] []
//     | [elem] -> c [elem]
//     | elem  ->
//         let (a, b) = List.splitAt (List.length elem / 2) elem
//         aux [] (fun id ->  )

(* 3: Approximating square roots *)

(* Question 3.1 *)

let approxSquare x num : float =
    let rec aux prev step =

        match step with
        | 0 -> prev
        | _ -> aux ((float x / prev + prev) / 2.0) (step - 1)
    // | _ -> aux ((float x / prev + prev) / (2: float)) (step - 1)

    if num = 0 then
        float (x / 2)
    else
        // printfn "half - %A and whole %A" (float (x / 2 * 2)) x
        aux (sqrt (float (x / 2 * 2))) num


(* Question 3.2 *)

let quadratic a b c num =
    let af = float a
    let bf = float b

    let preSquare = (b * b - 4 * a * c)
    let square = approxSquare (b * b - 4 * a * c) num
    printfn "%A - %A" preSquare square

    ((-bf + square) / (2.0 * af)), ((-bf - square) / (2.0 * af))

// (approxSquare (int ((-bf + sqrt (bf * bf - 4.0 * af * cf)) / (2.0 * af))) num,
//  approxSquare (int ((-bf - sqrt (bf * bf - 4.0 * af * cf)) / (2.0 * af))) num)

(* Question 3.3 *)

let parQuadratic _ = failwith "not implemented"

(* Question 3.4 *)

let solveQuadratic _ = failwith "not implemented"

(* 4: Rational numbers *)

(* Question 4.1 *)

type rat = unit (* replace this entire type with your own *)

(* Question 4.2 *)

let mkRat _ = failwith "not implemented"
let ratToString _ = failwith "not implemented"

(* Question 4.3 *)

let plus _ = failwith "not implemented"
let minus _ = failwith "not implemented"
let mult _ = failwith "not implemented"
let div _ = failwith "not implemented"

(* Question 4.4 *)

type SM<'a> = SM of (rat -> ('a * rat) option)
let ret x = SM(fun st -> Some(x, st))

let bind (SM m) f =
    SM(fun st ->
        match m st with
        | None -> None
        | Some(x, st') ->
            let (SM g) = f x
            g st')

let (>>=) m f = bind m f
let (>>>=) m n = m >>= (fun () -> n)
let evalSM (SM f) s = f s

let smPlus _ = failwith "not implemented"
let smMinus _ = failwith "not implemented"
let smMult _ = failwith "not implemented"
let smDiv _ = failwith "not implemented"

(* Question 4.5 *)

(* You may solve this exercise either using monadic operators or 
        using computational expressions. *)

type StateBuilder() =

    member this.Bind(x, f) = bind x f
    member this.Zero() = ret ()
    member this.Return(x) = ret x
    member this.ReturnFrom(x) = x
    member this.Combine(a, b) = a >>= (fun _ -> b)

let state = new StateBuilder()

let calculate _ = failwith "not implemented"
