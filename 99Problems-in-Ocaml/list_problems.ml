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


