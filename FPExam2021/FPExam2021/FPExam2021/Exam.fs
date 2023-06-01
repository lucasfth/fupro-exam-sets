module Exam2021
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

    type direction = North | East | South | West
    type coord     = C of int * int

    let move _ = failwith "not implemented"
    let turnRight _ = failwith "not implemented"
    let turnLeft _ = failwith "not implemented"
    
(* Question 1.2 *)

    type position = P of (coord * direction)
    type move     = TurnLeft | TurnRight | Forward of int

    let step _ = failwith "not implemented"

(* Question 1.3 *)


    let walk _ = failwith "not implemented"

(* Question 1.4 *)

    let path _ = failwith "not implemented"

(* Question 1.5 *)

    let path2 _ = failwith "not implemented"

(* Question 1.6 *)

(* Q: Your solution for `path` is not tail recursive. Why? To make a compelling
      argument you should evaluate a function call of the function, similarly to
      what is done in Chapter 1.4 of HR, and reason about that evaluation.
      You need to make clear what aspects of the evaluation tell you that the
      function is not tail recursive. Keep in mind that all steps in an evaluation
      chain must evaluate to the same value
      (```(5 + 4) * 3 --> 9 * 3 --> 27```, for instance).

   A: <Your answer goes here>
*)

    let path3 _ = failwith "not implemented"

(* 2: Code Comprehension *)
    let foo f =
        let mutable m = Map.empty
        let aux x =
            match Map.tryFind x m with
            | Some y when Map.containsKey x m -> y
            | None   -> 
            m <- Map.add x (f x) m; f x

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


    Q: What do functions foo and baz do (skip bar)?
       Focus on what they do rather than how they do it.

    A: <Your answer goes here>

    The function foo uses a mutable variable.

    Q: What function does it serve (why is it there)?

    A: <Your answer goes here>

    Q: What would happen if you removed the mutable keyword from the line
       let mutable m = Map.empty? Would the function foo still work?
       If yes, why; if no, why not?

    A: <Your answer goes here>

    Q: What would be appropriate names for functions 
       foo, bar, and baz?

    A: <Your answer goes here>
    
    *)
        

(* Question 2.2 *)

 
    (* 
    The code includes the keyword "and".

    
    Q: What function does this keyword serve in general
       (why would you use "and" when writing any program)?

    A: <Your answer goes here>


    Q: What would happen if you removed it from this particular program and
       replaced it with a standard "let"
       (change the line "and baz = foo bar" to "let baz = foo bar")?

    A: <Your answer goes here>

    *)

(* Question 2.3 *) 

    (* 
    The function foo generates a warning during compilation:
    "Warning: Incomplete pattern matches on this expression.".

    Q: Why does this happen, and where? 

    A: <Your answer goes here>


    Q: For these particular three functions will this incomplete pattern match
       ever cause problems for any possible execution of baz? If yes, why;
       if no, why not.

    A: <Your answer goes here>

    Q: The function foo has two redundant computations and is hence not as
       efficient as it could be. What are these two computations and why
       are they redundant?

    A: <Your answer goes here>

    *)

    let foo2 _ = failwith "not implemented"

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

    *)
(* Question 2.5 *)

    let bazSeq _ = failwith "not implemented"

(* 3: Guess the next sequence element *)

(* Question 3.1 *)

    type element = unit (* Your type goes here in stead of unit *)

(* Question 3.2 *)

    let elToString _ = failwith "not implemented"
    let elFromString _ = failwith "not implemented"

(* Question 3.3 *)

    let nextElement _ = failwith "not implemented"

(* Question 3.4 *)

    let elSeq _ = failwith "not implemented"
    let elSeq2 _ = failwith "not implemented"

    (*

    Q: Why would Seq.initInfinite not be an appropriate choice to
       write a function like elSeq?

    A: <Your answer goes here>

    *)

(* Question 3.5 *)

    let compress _ = failwith "not implemented"

(* Question 3.6 *)

    let elParse _ = failwith "not implemented"
    let elFromString2 _ = failwith "not implemented"

(* 4: Rings *)

(* Question 4.1 *)

    type 'a ring = RemoveThisConstructor of 'a (* replace this entire type with your own *)

(* Question 4.2 *)

    let length _ = failwith "not implemented"
    let ringFromList _ = failwith "not implemented"
    let ringToList _ = failwith "not implemented"

(* Question 4.3 *)

    let empty _ = failwith "not implemented"
    let push _ = failwith "not implemented"
    let peek _ = failwith "not implemented"
    let pop _ = failwith "not implemented"
    let cw _ = failwith "not implemented"
    let ccw _ = failwith "not implemented"

(* Question 4.4 *)

    type StateMonad<'a, 'b> = SM of ('b ring -> ('a * 'b ring) option)
    let ret x = SM (fun st -> Some (x, st))
    let bind (SM m) f =
        SM (fun st ->
            match m st with
            | None -> None
            | Some (x, st') ->
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

        member this.Bind(x, f)    = bind x f
        member this.Zero ()       = ret ()
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = new StateBuilder()

    let ringStep _ = failwith "not implemented"
    let iterRemoveSumEven _ = failwith "not implemented"