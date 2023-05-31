module Exam2020_2
(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but 
   it does allow you to work in interactive mode and you can just remove the '=' 
   to make the project compile work again.

   Do not remove the line (even though that does work) because you may inadvertantly
   introduce indentation errors in your code that may be hard to find if you want
   to switch back to project mode. 

   Alternative, keep the line as is, but load ExamInteractive.fsx into the interactive environment
   *)
(* module Exam2020_2 = *)

(* 1: Binary search trees *)

type 'a bintree =
    | Leaf
    | Node of 'a bintree * 'a * 'a bintree

(* Question 1.1 *)

let rec insert x t =
    match t with
    | Leaf -> Node(Leaf, x, Leaf)
    | Node(l, y, r) when x <= y -> Node(insert x l, y, r)
    | Node(l, y, r) -> Node(l, y, insert x r)

(* Question 1.2 *)

let fromList lst =
    let rec aux lst acc =
        match lst with
        | x :: xs -> aux xs (insert x acc)
        | _ -> acc

    aux lst Leaf
(* Question 1.3 *)

let rec fold f acc t =
    match t with
    | Leaf -> acc
    | Node(l, y, r) -> fold f (f (fold f acc l) y) r

let rec foldBack f acc t =
    match t with
    | Leaf -> acc
    | Node(l, y, r) -> foldBack f (f (fold f acc r) y) l

let inOrder t =
    foldBack (fun acc elem -> elem :: acc) [] t

(* Question 1.4 *)

(* 

    Q: Consider the following map function

    *)

let rec badMap f =
    function
    | Leaf -> Leaf
    | Node(l, y, r) -> Node(badMap f l, f y, badMap f r)

(*
    Even though the type of this function is `('a -> 'b) -> 'a bintree -> 'b bintree` 
    as we would expect from a map function, this  function does not do what
    we want it to do. What is the problem? Provide an example to demonstrate the problem.

    A: <Your answer goes here>
        The reason for why it is not a good mapper is due to it not reorganizing the nodes
        after applying the functions.

    *)

let rec map f t = inOrder t |> List.map f |> fromList


(* 2: Code Comprehension *)
let rec foo =
    function
    | [ x ] -> [ x ]
    | x :: y :: xs when x > y -> y :: (foo (x :: xs))
    | x :: xs -> x :: foo xs

let rec bar =
    function
    | [ x ] -> true
    | x :: y :: xs -> x <= y && bar (y :: xs)

let rec baz =
    function
    | [] -> []
    | lst when bar lst -> lst
    | lst -> baz (foo lst)


(* Question 2.1 *)

(* 
    
    Q: What are the types of functions foo,  bar, and baz?

    A: <Your answer goes here>
        The type of foo is list<'a> -> list<'a>
        The type of bar is list<'a> -> list<'a>
        The type of baz is list<'a> -> list<'a>

    Q: What do functions ```bar```, and ```baz``` do 
       (not `foo`, we admit that it is a bit contrived)? 
       Focus on what they do rather than how they do it.

    A: <Your answer goes here>
        bar says if a list is sorted correctly (true) or not sorted (false)
        baz sorts the list

    Q: What would be appropriate names for functions 
       foo, bar, and baz?

    A: <Your answer goes here>
        foo could be called sort singleSort since it only sorts a single 
        element in a list.
        bar could be called isSorted
        baz could be called sort
    *)

(* Question 2.2 *)

(* 
    The functions foo and bar generate a warning during compilation: 
    'Warning: Incomplete pattern matches on this expression.' 
    
    Q: Why does this happen, and where? 

    A: <Your answer goes here>
        The warning says:
            Incomplete pattern matches on this expression. 
            For example, the value '[]' may indicate a case 
            not covered by the pattern(s).
        This is due to the match cases only being for list with elements.
        If the functions were to recieve empty lists they will crash.

    Q: For these particular three functions will this incomplete 
       pattern match ever cause problems for any possible execution of baz? 
       If yes, why; if no, why not.

    A: <Your answer goes here>
        Since baz uses foo and foo does not have a match case for an empty
        list it will throw an error if given an empty list.
    *)

let rec foo2 (lst: list<int>) =
    match lst with
    | [] -> []
    | x :: y :: xs when x > y -> y :: (foo2 (x :: xs))
    | x :: xs -> x :: foo2 xs

let rec bar2 lst =
    match lst with
    | x :: y :: xs -> x <= y && bar (y :: xs)
    | _ -> true

(* Uncomment code to run after you have written foo2 and bar2 *)

let rec baz2 =
    function
    | lst when bar2 lst -> lst
    | lst -> baz2 (foo2 lst)

(* Question 2.3 *)

(* Consider this alternative definition of *)

let rec foo3 =
    function
    | [ x ] -> [ x ]
    | x :: xs -> x :: foo3 xs
    | x :: y :: xs when x > y -> y :: (foo3 (x :: xs))

(*

    Q: Do the functions `foo` and `foo3` produce the same output for all possible inputs? 
       If yes, why; if no why not and provide a counter example.

    A: <Your answer goes here>
        Due to the match case x::xs being before x::y::xs the later will never be reached.
        Therefore foo3 will never make any single sorts. 
        This is because x::xs will always match list with a single element (though only from
        two elements and up here due to the first match case).
        So no it will not produce the same outputs.
    *)

(* Question 2.4 *)

let bar3 lst =
    let sorted = List.sort lst
    lst = sorted

(* Question 2.5 *)

(*
    Q: The function foo or baz is not tail recursive. Which one and why?
    
    A: <Your answer goes here>
        foo is not tail recursive
        I will call foo with [1;3;2]
        = foo [1;3;2]
        = 1 :: (foo [3;2])
        = 1 :: (2 :: (foo (3 :: [])))
        = 1 :: (2 :: (foo [3]))
        = 1 :: (2 :: ([3]))
        = 1 :: (2 :: [3])
        = 1 :: ([2;3])
        = 1 :: [2;3]
        = [1;2;3]

        As seen foo is not tail recursive since it has to make its
        recursive calls and then append the evaluations afterwards. 
    *)

//     let rec foo =
//     function
//     | [ x ] -> [ x ]
//     | x :: y :: xs when x > y -> y :: (foo (x :: xs))
//     | x :: xs -> x :: foo xs

// let rec baz =
//     function
//     | [] -> []
//     | lst when bar lst -> lst
//     | lst -> baz (foo lst)


(* ONLY implement the one that is NOT already tail recursive *)

let fooTail lst =
    let rec aux lst c =
        match lst with
        | [ x ] -> c [ x ]
        | x :: y :: xs when x > y -> aux (x :: xs) (fun r -> c (y :: r))
        | x :: xs -> aux (xs) (fun r -> c (x :: r))

    aux lst id

let bazTail _ = failwith "not implemented"

(* 3: Big Integers *)

(* Question 3.1 *)

type bigInt = int list

let fromString (nums: string) : bigInt =
    List.foldBack (fun elem acc -> (int elem - int '0') :: acc) (Seq.toList nums) []

let toString (x: bigInt) =
    List.foldBack (fun elem acc -> string elem + acc) x ""

(* Question 3.2 *)

let add (x: bigInt) (y: bigInt) : bigInt =
    let num = (List.length x) - (List.length y)
    let zList = [ for i in 1 .. (abs num) -> 0 ]

    let x' = List.rev x
    let y' = List.rev y

    let rec aux (x: bigInt) (y: bigInt) (acc: string) (extra: int) =
        match x, y with
        | [], [] ->
            if extra <> 0 then
                [ 1 ] @ fromString acc
            else
                fromString acc
        | a :: ax, b :: ys ->
            if (a + extra) + b >= 10 then
                aux ax ys (string (((a + extra) + b) % 10) + acc) 1
            else
                aux ax ys (string ((a + extra) + b) + acc) 0

    if num > 0 then
        aux x' (List.rev (zList @ y)) "" 0
    else
        aux (List.rev (zList @ x)) y' "" 0

(* Question 3.3 *)

let multSingle (x: bigInt) (y: int) : bigInt =
    if x = [ 0 ] || y = 0 then
        fromString "0"
    else
        let rec aux counter acc =
            match counter with
            | count when count = y -> acc
            | _ -> aux (counter + 1) (add acc x)

        aux 0 [ 0 ]

(* Question 3.4 *)

let mult _ = failwith "not implemented"

(* Question 3.5 *)

let fact _ = failwith "not implemented"

(* 4: Lazy lists *)

type 'a llist = Cons of (unit -> ('a * 'a llist))

let rec llzero = Cons(fun () -> (0, llzero))

(* Question 4.1 *)

let step (ll: 'a llist) =
    match ll with
    | Cons f -> f ()

let cons (x: 'a) (ll: 'a llist) : 'a llist = Cons(fun () -> (x, ll))


(* Question 4.2 *)

let init _ = failwith "not implemented"

(* Question 4.3 *)

let llmap _ = failwith "not implemented"

(* Question 4.4 *)

let filter _ = failwith "not implemented"

(* Question 4.5 *)

let takeFirst _ = failwith "not implemented"

(* Question 4.6 *)

let unfold _ = failwith "not implemented"

(* Consider the following two implementations of Fibonacci sequences fibll1 and fibll2: *)

let fib x =
    let rec aux acc1 acc2 =
        function
        | 0 -> acc1
        | x -> aux acc2 (acc1 + acc2) (x - 1)

    aux 0 1 x

(* Uncomment after you have implemented init and unfold *)

(*
    let fibll1 = init fib
    let fibll2 = unfold (fun (acc1, acc2) -> (acc1, (acc2, acc1 + acc2))) (0, 1)
  *)
(* 

    Q: Both fibll1 and fibll2 correctly calculate a lazy list of Fibonacci numbers. 
       Which of these two lazy lists is the most efficient implementation and why?
    
    A: <Your answer goes here>
    
    *)
