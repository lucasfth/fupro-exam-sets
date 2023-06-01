module Exam2021

open JParsec.TextParser
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
 module Exam2021 = 
 *)

(* 1: Dungeon crawler *)

(* Question 1.1 *)

type direction =
    | North
    | East
    | South
    | West

type coord = C of int * int

let move dist (dir: direction) (C(x, y)) =
    match dir with
    | North -> C(x, y - dist)
    | South -> C(x, y + dist)
    | West -> C(x - dist, y)
    | East -> C(x + dist, y)

let turnRight dir =
    match dir with
    | North -> East
    | East -> South
    | South -> West
    | West -> North

let turnLeft dir = turnRight dir |> turnRight |> turnRight

(* Question 1.2 *)

type position = P of (coord * direction)

type move =
    | TurnLeft
    | TurnRight
    | Forward of int

let step (P(C(x, y), dir)) m =
    match m with
    | TurnLeft -> P(C(x, y), turnLeft dir)
    | TurnRight -> P(C(x, y), turnRight dir)
    | Forward dis -> P(move dis dir (C(x, y)), dir)

(* Question 1.3 *)

let rec walk pos ms =
    match ms with
    | move :: tail -> walk (step pos move) tail
    | _ -> pos

let walk2 pos ms =
    List.fold (fun pos m -> step pos m) pos ms

(* Question 1.4 *)

let rec path (pos: position) (moves: move list) : coord list =
    match moves with
    | [] -> [ getCoord pos ]
    | move :: tail ->
        let nextPos = step pos move

        match move with
        | Forward _ -> getCoord pos :: path nextPos tail
        | _ -> path nextPos tail

and getCoord (P(coord, _)) = coord

(* Question 1.5 *)

let path2 (pos: position) (moves: move list) : coord list =
    let rec aux pos moves acc =
        match moves with
        | [] -> acc @ [ getCoord pos ]
        | move :: tail ->
            let nextPos = step pos move

            match move with
            | Forward _ -> aux nextPos tail (acc @ [ getCoord pos ])
            | _ -> aux nextPos tail acc

    aux pos moves []

(* Question 1.6 *)

(* Q: Your solution for `path` is not tail recursive. Why? To make a compelling
      argument you should evaluate a function call of the function, similarly to
      what is done in Chapter 1.4 of HR, and reason about that evaluation.
      You need to make clear what aspects of the evaluation tell you that the
      function is not tail recursive. Keep in mind that all steps in an evaluation
      chain must evaluate to the same value
      (```(5 + 4) * 3 --> 9 * 3 --> 27```, for instance).

   A: <Your answer goes here>
    I will call path with (P (C (0, 0), North)) and [TurnRight; Forward 10; TurnLeft]
    = path (P (C (0, 0), North)) [TurnRight; Forward 10; TurnLeft]
    = path (P (C (0, 0), East)) [Forward 10; TurnLeft]
    = C (0, 0) :: (path (P (C (10, 0), East)) [TurnLeft])
    = C (0, 0) :: (path (P (C (10, 0), North)) [])
    = C (0, 0) :: ([C (10, 0)])
    = C (0, 0) :: [C (10, 0)]
    = [C (0, 0); C(10, 0)]

    As seen it is not tail recursive since it has to evaluate its
    tail before appending the results.
*)

let path3 (pos: position) (moves: move list) : coord list =
    let rec aux pos moves c =
        match moves with
        | [] -> c [ getCoord pos ]
        | move :: tail ->
            let nextPos = step pos move

            match move with
            | Forward _ -> aux nextPos tail (fun r -> c ((getCoord pos) :: r))
            | _ -> aux nextPos tail c

    aux pos moves id

(* 2: Code Comprehension *)
let foo f =
    let mutable m = Map.empty

    let aux x =
        match Map.tryFind x m with
        | Some y when Map.containsKey x m -> y
        | None ->
            m <- Map.add x (f x) m
            f x

    aux

let rec bar x =
    match x with
    | 0 -> 0
    | 1 -> 1
    | y -> baz (y - 1) + baz (y - 2)

and baz = foo bar

(* Question 2.1 *)

(* 
    Q: What are the types of functions foo, bar, and baz?

    A: <Your answer goes here>
        foo: ('a -> 'b) -> ('a -> 'b)
        bar: int -> int
        baz: int -> int

    Q: What do functions foo and baz do (skip bar)?
        Focus on what they do rather than how they do it.

    A: <Your answer goes here>
        foo takes a function and looks up the value.
        If it does not contain the value it will calculate it
        baz will find fibonacci numbers with bar.
        If they have already been calculated it will be taken from foo
        otherwise it will be stored.

    The function foo uses a mutable variable.

    Q: What function does it serve (why is it there)?

    A: <Your answer goes here>
        It is to update the map when a new value has been calculated.


    Q: What would happen if you removed the mutable keyword from the line
       let mutable m = Map.empty? Would the function foo still work?
       If yes, why; if no, why not?

    A: <Your answer goes here>
        Then the map can not be dynamically changed.

    Q: What would be appropriate names for functions 
       foo, bar, and baz?

    A: <Your answer goes here>
        Appropriate name for foo could be cache
        Appropriate name for bar fib or fibAux
        Appropriate name for baz would be cachedFib or fib
    *)

(* Question 2.2 *)

(* 
    The code includes the keyword "and".

    Q: What function does this keyword serve in general
       (why would you use "and" when writing any program)?

    A: <Your answer goes here>
        `and` is used to mutual recursion.
        So if a calls b and b also calls a then defining b with and would allow this

    Q: What would happen if you removed it from this particular program and
       replaced it with a standard "let"
       (change the line "and baz = foo bar" to "let baz = foo bar")?

    A: <Your answer goes here>
        Since foo nor bar calls baz and baz does not call itself it works fine.
    *)

(* Question 2.3 *)

(* 
    The function foo generates a warning during compilation:
    "Warning: Incomplete pattern matches on this expression.".

    Q: Why does this happen, and where? 

    A: <Your answer goes here>
        Since there is a when statement on the match on `Some` the compiler
        does not know if all match cases will be reached.

    Q: For these particular three functions will this incomplete pattern match
       ever cause problems for any possible execution of baz? If yes, why;
       if no, why not.

    A: <Your answer goes here>
        It will not cause a problem.
        This is due to the match on `Some` only being reached when the key
        `x` can be found in the map.
        Then the when statement says that it can only execute if the map
        contains the key which will always be true since it was just found. 

    Q: The function foo has two redundant computations and is hence not as
       efficient as it could be. What are these two computations and why
       are they redundant?

    A: <Your answer goes here>
        The when statement on the match statement on `Some` can be removed.
        f x is calculated twice on the `None` match.
        It would therefore had been more effective to throw the evaluated value
        into a variable.
    *)

let foo2 f =
    let mutable m = Map.empty

    let aux x =
        match Map.tryFind x m with
        | Some y -> y
        | None ->
            let cal = f x
            m <- Map.add x cal m
            cal

    aux

(* Question 2.4 *)

let rec barbaz x =
    let baz = foo barbaz

    match x with
    | 0 -> 0
    | 1 -> 1
    | y -> baz (y - 1) + baz (y - 2)

(*

    Q: Without explicitly timing the execution times, compare the execution
       times of baz and barbaz. One is slower than the other.
       Why? You do not have to give exact times, just spot which one is
       slower and explain why.

    A: <Your answer goes here>
        barbaz is slower as it initializes foo at each recursive call. 
        Therefore each fibanacci number has to be recomputed.
    *)
(* Question 2.5 *)

let bazSeq: int seq = Seq.initInfinite baz

(* 3: Guess the next sequence element *)

(* Question 3.1 *)

type element = char list

(* Question 3.2 *)

let elToString el =
    List.fold (fun acc elem -> acc + (string elem)) "" el

let elFromString s = List.ofSeq s
(* Question 3.3 *)

let countElements elem =
    let startElement = List.item 0 elem

    let rec aux lst count restOfList =
        match lst with
        | [] -> ((count, startElement), restOfList)
        | x :: xs when x = startElement -> aux xs (count + 1) xs
        | _ :: _ -> ((count, startElement), restOfList)

    aux elem 0 []

let nextElement elem =
    let rec aux lst result =
        match lst with
        | [] -> elFromString result
        | xs ->
            let ((count, element), restOfList) = countElements xs
            aux restOfList (result + string count + string element)

    aux elem ""

(* Question 3.4 *)

let elSeq el =
    Seq.unfold (fun e -> Some(e, nextElement e)) el

let elSeq2 el =
    failwith "Seq.initInfinite (el, nextElement el) not even close"

(*

    Q: Why would Seq.initInfinite not be an appropriate choice to
       write a function like elSeq?

    A: <Your answer goes here>

    *)

(* Question 3.5 *)

let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
let pletter = satisfy System.Char.IsLetter <?> "letter"
let palphanumeric = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"
let pdigit = satisfy System.Char.IsDigit <?> "digit"
let spaces = many (whitespaceChar) <?> "space"
let spaces1 = many1 (whitespaceChar) <?> "space1"
let newLine = pstring "\\n"
let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2
let (.>*>) p1 p2 = p1 .>> spaces .>> p2
let (>*>.) p1 p2 = p1 >>. spaces >>. p2

let myParse = spaces >*>. many pint32 .>*> spaces

let compress _ = failwith "not implemented"

(* Question 3.6 *)

let elParse _ = failwith "not implemented"
let elFromString2 _ = failwith "not implemented"

(* 4: Rings *)

(* Question 4.1 *)

type ring<'a> = 'a list * 'a list

(* Question 4.2 *)

let length (l1, l2) =
    let aLength = List.fold (fun acc _ -> acc + 1) 0 l1
    let bLength = List.fold (fun acc _ -> acc + 1) 0 l2
    aLength + bLength


let ringFromList lst : ring<'a> = (List<'a>.Empty, lst)

let ringToList (l1, l2) =
    let alist = List.foldBack (fun acc elem -> acc :: elem) [] l1
    let blist = List.foldBack (fun acc elem -> acc :: elem) [] l2
    alist @ blist

(* Question 4.3 *)
let empty: ring<'a> = (List.empty, List.empty)

let push elem (l1, l2) = (l1, elem :: l2)

let peek (ring: ring<'a>) =
    match ring with
    | ([], []) -> None
    | (a, []) -> Some(a |> List.rev |> List.head)
    | (_ :: _, b :: _) -> Some(b)
    | _ -> None

let pop (ring: ring<'a>) : (ring<'a>) option =
    match ring with
    | ([], []) -> None
    | (a, []) -> Some(List.empty, a |> List.rev |> List.tail)
    | (a, _ :: bx) -> Some(a, bx)

let cw (ring: 'a ring) : 'a ring = // Something wrong
    match ring with
    | ([], []) -> empty
    | ([], b) ->
        let revved = b |> List.rev
        (revved.Tail, [ revved.Head ])
    | (a :: ax, b) -> (ax, a :: b)

let ccw (ring: 'a ring) : 'a ring = // Something wrong
    match ring with
    | ([], []) -> empty
    | (a, []) ->
        let revved = a |> List.rev
        ([ List.head revved ], List.tail revved)
    | (a, b :: bx) -> (b :: a, bx)

(* Question 4.4 *)

type StateMonad<'a, 'b> = SM of ('b ring -> ('a * 'b ring) option)
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

let smLength _ = failwith "not implemented"
let smPush _ = failwith "not implemented"
let smPop _ = failwith "not implemented"
let smCW _ = failwith "not implemented"
let smCCW _ = failwith "not implemented"

(* Question 4.4 *)

(* You may solve this exercise either using monadic operators or 
        using computational expressions. *)

type StateBuilder() =

    member this.Bind(x, f) = bind x f
    member this.Zero() = ret ()
    member this.Return(x) = ret x
    member this.ReturnFrom(x) = x
    member this.Combine(a, b) = a >>= (fun _ -> b)

let state = new StateBuilder()

let ringStep _ = failwith "not implemented"
let iterRemoveSumEven _ = failwith "not implemented"
