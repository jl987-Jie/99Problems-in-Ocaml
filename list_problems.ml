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
let last_two lst = 
    let len = List.length lst in
    if len < 2 then
        None
    else
        let h (cur_len, result) elm =
            if cur_len = len - 1 || cur_len = len then
                (cur_len + 1, elm :: result)
            else
                (cur_len + 1, result) in 
        let res = snd (List.fold_left h (1, []) lst) in
        match res with
        | [] -> None
        | hd :: tl -> Some (hd, List.hd tl)



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

(*
 * Pack consecutive duplicates of list elements into sublists. (medium)
 * eg:
 * # pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"];;
   - : string list list =
   [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
   ["e"; "e"; "e"; "e"]]
 *)
let pack lst = 
    let len = List.length lst in
    match lst with
    | [] -> []
    | hd :: tl ->
        let h (prev, accu, same_lst, cur_pos) elm =
            (* when element is same as previous element, add to same_lst *)
            if elm = prev then 
                if cur_pos = len - 1 then
                    (* append the last element *)
                    (elm, accu @ [same_lst @ [elm]], same_lst, cur_pos)
                else
                    (* add to same_lst *)
                    (elm, accu, same_lst @ [elm], cur_pos + 1) 
            else 
            (* element is not the same as the previous, so add the same_lst to accu *)
                (elm, accu @ [same_lst], [elm], cur_pos + 1) in
        match List.fold_left h (hd, [], [], 0) lst with
        | (_, s, _, _) -> s

(* 
 * Run-length encoding of a list. (easy)
 * # encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
    - : (int * string) list =
    [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
 *)
let encode lst = 
    let len = List.length lst in
    match lst with
    | [] -> []
    | hd :: tl ->
        (* prev: previous element 
         * accu: stores encoding list
         * same_lst: list with consecutive same elements
         * cur_pos: current position in the fold
         * count: count of consecutive same elements *)
        let h (prev, accu, same_lst, cur_pos, count) elm =
            (* when element is same as previous element, add to same_lst *)
            if elm = prev then 
                if cur_pos = len - 1 then
                    (* append the last element *)
                    (elm, accu @ [(count + 2, prev)], same_lst, cur_pos, count)
                else
                    (* add to same_lst *)
                    (elm, accu, same_lst @ [elm], cur_pos + 1, count + 1) 
            else 
            (* element is not the same as the previous, so add the same_lst to accu *)
                (elm, accu @ [(count + 1, prev)], [elm], cur_pos + 1, 0) in
        match List.fold_left h (hd, [], [], 0, -1) lst with
        | (_, s, _, _, _) -> s

(* 
 Modify the result of the previous problem in such a way that if an element 
 has no duplicates it is simply copied into the result list. Only elements with 
 duplicates are transferred as (N E) lists.
*)

type 'a rle = | One of 'a | Many of int * 'a

(* modified_encoding ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
- : string rle list =
[Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 Many (4, "e")] 
 *)

let modified_encoding lst = 
    (* use the encode function and check if the count is 1 or many *)
    let encoded_lst = encode lst in
    let h accu elm = 
        match elm with
        | (count, c) -> 
            if count = 1 then
                accu @ [One c]
            else
                accu @ [Many (count, c)] in
    List.fold_left h [] encoded_lst

(* Decode a run-length encoded list. (medium)
 Given a run-length code list generated as specified in the previous problem, 
 construct its uncompressed version.
# decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")];;
- : string list =
["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] *)

(* return a list containing c #count times *)
let rec dupl count c = 
    if count = 0 then [] else c :: dupl (count - 1) c

let decode lst =
    let h accu elm =
        match elm with 
        | One e -> accu @ [e]
        | Many (count, e) -> accu @ dupl count e in
    List.fold_left h [] lst

(* Duplicate the elements of a list. (easy) *)
(* # duplicate ["a";"b";"c";"c";"d"];;
- : string list = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"] *)
let duplicate lst = 
    let h accu elm =
        accu @ [elm] @ [elm] in
    List.fold_left h [] lst

(* Replicate the elements of a list a given number of times. (medium) 
# replicate ["a";"b";"c"] 3;;
- : string list = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]
 *)
let replicate lst count = 
    let h accu elm =
        accu @ dupl count elm in
    List.fold_left h [] lst

(* Drop every N'th element from a list. (medium) 
# drop_nth_elm ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
- : string list = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]
 *)

let drop_nth_elm lst n = 
    let h (accu, count) elm =
        if count = n then
            (accu, 1)
        else
            (accu @ [elm], count + 1) in
    fst(List.fold_left h ([], 1) lst)

(* Split a list into two parts; the length of the first part is given. (easy)
If the length of the first part is longer than the entire list, then the 
first part is the list and the second part is empty. 

# split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
- : string list * string list =
(["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])
# split ["a";"b";"c";"d"] 5;;
- : string list * string list = (["a"; "b"; "c"; "d"], []) *)

let split lst n =
    let len = List.length lst in
    if n >= len then
        (lst, [])
    else
    (* keep a first part and second part and when count reaches n, append
        to the second list *)
        let h (fst_part, snd_part, count) elm =
            if count <= n then
                (fst_part @ [elm], snd_part, count + 1)
            else
                (fst_part, snd_part @[elm], count + 1) in
        let result = List.fold_left h ([], [], 1) lst in
        match result with
        | (f, s, _) -> (f, s)

