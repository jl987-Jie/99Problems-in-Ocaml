let (@) xs ys = List.rev_append (List.rev xs) ys

(* 
 *  Write a function 
 * map : ('a -> 'b) -> 'a list -> 'b list 
 * that is an implementation of the List.map 
 * function using only List.fold_left or List.fold_right.
 *)
let map f lst =
    let h accu elm =
        accu @ [f elm] in
    List.fold_left h [] lst

(* Write a function 
 * last : 'a list -> 'a option 
 * that returns the last element of a list. *)
let rec last lst =
    match lst with
    | [] -> None
    | hd :: [] -> Some hd
    | hd :: tl -> last tl

(*
 * Find the last but one (last and penultimate) 
 * elements of a list. 
 *)
let last_two lst = failwith "implement me"

(* 
 * Find the k'th element of a list. 
 *)
let at kth lst =
    let h (acc, pos) elm =
        if pos = kth then (Some elm, pos + 1) else (acc, pos + 1) in
    fst (List.fold_left h (None, 1) lst)

(*
 * Find the number of elements of a list. 
 *)
let length lst =
    let h acc elm =
        acc + 1 in
    List.fold_left h 0 lst

(*
 * Reverse a list. 
 *)
let rev lst =
    let h acc elm =
        elm :: acc in
    List.fold_left h [] lst

(*
 * Find out whether a list is a palindrome. 
 *)
let is_palindrome lst = lst = rev lst

(*
 * Flatten a nested list structure.
 * There is no nested list type in OCaml, so we need to define one
 * first. A node of a nested list is either an element, or a list of
 * nodes. 
 * ex:
 * # flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ];;
 * - : string list = ["a"; "b"; "c"; "d"; "e"]
 *)
type 'a node = One of 'a | Many of 'a node list

let rec flatten lst =
    let h accu elm =
        match elm with
        | One e -> accu @ [e]
        | Many l -> accu @ flatten l in
    List.fold_left h [] lst

(*
 * Eliminate consecutive duplicates of list elements.
 * eg: 
 * # compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
 * - : string list = ["a"; "b"; "c"; "a"; "d"; "e"]
 *)
let compress lst =
    match lst with
    | [] -> []
    | hd :: tl ->
        let h (prev, accu) elm =
            if elm = prev then (prev, accu) else (elm, accu @ [elm]) in
        snd(List.fold_left h (hd, [hd]) tl)