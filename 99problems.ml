(* Problem 1 *) 
let rec last (xs: 'a list): 'a option =
  match xs with
  | [] -> None
  | [x] -> Some x
  | _  :: tl -> last tl;;
last ["a" ; "b" ; "c" ; "d"];;


(* Problem 2 *)
module Problem02 = struct
    let rec last_two (xs: 'a list): ('a * 'a) option =
        match xs with
        | [] -> None
        | [_] -> None
        | [x; y] -> Some (x, y)
        | _ :: rest -> last_two rest
    let () =
      print_endline "Testing Problem 02";
      assert (last_two [1;2;3;4] = Some (3,4));
end

(* Problem 3 -- Fine the 'kth' element of a list*)
module Problem03 = struct
  let rec at (xs: 'a list) (k: int): 'a option =
    match xs with
    | x :: _ when k == 1 -> Some x
    | _ :: tl when k > 1 -> at tl (k - 1)
    | _ -> None

  let () =
    print_endline "Testing Problem 03";
    assert (at ["a"; "b"; "c"; "d"; "e"] 3 = Some "c");
end

(*Problem 4 -- Find the number of elements in a list (length of list) *)
module Problem04 = struct
  let rec length (xs: 'a list): int =
    match xs with
    | [] -> 0
    | hd :: tl -> 1 + length tl
  let length_tail_recursive (xs: 'a list): int =
    let rec aux (xs: 'a list) (acc: int): int =
      match xs with
      | [] -> acc
      | _ :: tl -> aux tl (acc + 1)
  in aux xs 0

  let () =
    print_endline "Testing Problem 04";
    assert (length [1;2;3;4] = 4);
    assert (length_tail_recursive [1;2;3;4] = 4);
end

(* Problem 5 -- Reverse a list*)
module Problem05 = struct
  let rev (xs: 'a list): 'a list =
    let rec aux (xs: 'alist) (acc: 'a list): 'a list =
      match xs with
      | [] -> acc
      | hd :: tl -> aux tl (hd :: acc)
  in aux xs []

  let () =
    print_endline "Testing Problem 05";
    assert (rev [1;2;3;4] = [4;3;2;1]);
  end

(* Problem 6 -- Find out if a list is a palindrome *)
module Problem06 = struct
  let is_palindrome (xs: 'a list): bool =
    List.rev xs = xs
  let () =
    print_endline "Testing Problem 06";
    assert (is_palindrome ["r" ; "a" ; "c" ; "e" ; "c" ; "a" ; "r"] = true);
    assert (is_palindrome ["a" ; "b" ; "c" ] = false);
  end
(*Problem 7 -- Flatten a List*)
module Problem07 = struct
(* There is no nested list type in OCaml, so we need to define one
     first. A node of a nested list is either an element, or a list of
     nodes. *)
  type 'a node =
    | One of 'a 
    | Many of 'a node list
let flatten (xs: 'a node list): 'a list =
  let rec aux (acc: 'a list)  (xs: 'a node list): 'a list =
    match xs with
      | [] -> acc
      | One x :: t -> aux (x :: acc) t
      | Many l :: t -> aux (aux acc l) t in
    List.rev (aux [] xs)

  let () =
    print_endline "Testing Problem 07";
    assert (flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]] = ["a"; "b"; "c"; "d"; "e"])
end

(* Problem 10 -- Run-length encoding of a list *)  
module Problem10 = struct
  let encode (xs: 'a list): (int * string) list =
    let rec aux (xs: 'a list) (count: int) (acc: (int * string) list): (int * string) list =
      match xs with
      | [] -> []
      | [x] -> ((count + 1, x) :: acc) 
      | x :: (y :: _ as tl) -> if x = y then aux tl (count + 1) acc 
                              else aux tl 0 ((count + 1, x) :: acc) in
    List.rev (aux xs 0 [])

  let () =
    print_endline "Testing Problem 10";
    assert (encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] = [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]);
end

(* Problem 14 -- Duplicate the elements of a list *)
module Problem14 = struct
  let duplicate (xs: 'a list): 'a list =
    let rec aux (xs: 'a list) (acc: 'a list): 'a list =
      match xs with
      |[] -> acc
      | hd :: tl -> aux tl (hd :: hd :: acc) in
    aux xs [] |> List.rev

  let () =
    print_endline "Testing Problem 14";
    assert ( duplicate ["a"; "b"; "c"; "c"; "d"] = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"])
end

(*Problem 17 -- Split a list into two parts; the length of the first part is given *)
module Problem17 = struct
  let split (xs: 'a list) (loc: int): 'a list * 'a list = 
    let rec aux (xs: 'a list) (acc: 'a list) (loc: int): 'a list * 'a list =
      match xs with
      | [] -> List.rev acc, [] (* if loc was equal to or greater than the length of the initial list *)
      | hd :: tl as l -> if loc = 0 then List.rev acc, l
                         else aux tl (hd :: acc) (loc - 1) in
    aux xs [] loc

  let () =
    print_endline "Testing Problem 17";
    assert (split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3 = (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"]));
    assert (split ["a"; "b"; "c"; "d"] 5 = (["a"; "b"; "c"; "d"], []))
 end 

(*Problem 20 -- Remove the kth element from a list*)
module Problem20 = struct
  let remove_at (loc: int) (xs: 'a list): 'a list =
    let rec aux (loc: int) (xs: 'a list) (acc: 'a list): 'a list =
    match xs with
    | [] -> []
    | hd :: tl -> if loc == 0 then acc @ tl
                  else aux (loc - 1) tl (hd :: acc)
  in aux loc xs [] 
  let () =
    print_endline "Testing Problem 20";
    assert(remove_at 1 ["a"; "b"; "c"; "d"] = ["a"; "c"; "d"]);
end

(*Problem 21 -- Insert an Element at the given location*)
module Problem21 = struct
  let rec insert_at (element: 'a) (loc: int) (xs: 'a list): 'a list =
    match xs with
    |[] -> [element]
    | hd :: tl as l-> if loc = 0 then element :: l
                      else hd :: insert_at element (loc - 1) tl
  let () =
    print_endline "Testing Problem 21";
    assert(insert_at "alfa" 1 ["a"; "b"; "c"; "d"] = ["a"; "alfa"; "b"; "c"; "d"]);
    assert(insert_at "alfa" 3 ["a"; "b"; "c"; "d"] = ["a"; "b"; "c"; "alfa"; "d"]);
    assert(insert_at "alfa" 4 ["a"; "b"; "c"; "d"] = ["a"; "b"; "c"; "d"; "alfa"]);
end

(*Problem 22 - Create a list containing all integers within a given range*)
module Problem22 = struct
  let range (starting: int) (ending: int): int list =
    (*Performing tail-recursive so we can use an accumulator*)
    let rec aux (acc: int list) (starting: int) (ending: int): int list =
      (*Catch the case when we need to perform a decrement range ie: range 9 0 *)
      if starting >= ending then
        aux (starting :: acc) (starting - 1) ending
      else acc
    in
    (*Provide count for both cases of starting > ending and starting < ending in the closure*)
    (*If starting < ending, then flip the inputs in to aux to account for the chosen structure*)
    if starting < ending then
      aux [] ending starting
    else List.rev (aux [] starting ending) 

  let () =
    print_endline "Testing Problem 22";
    assert(range 4 9 = [4; 5; 6; 7; 8; 9]);
    assert(range 9 4 = [9; 8; 7; 6; 5; 4]);
end

(* Problem 31 -- Determine whether a given integer is prime*)
module Problem31 = struct
  let is_prime (i: int): bool =
    let n = max i (-i) in
    let rec is_not_divisor (d: int): bool =
      d * d > n || (n mod d <> 0 && is_not_divisor (d + 1))
    in
    is_not_divisor 2
                                         
  let () =
    print_endline "Testing Problem 31";
    assert(is_prime (12) = false);
end

(*Euclid's Algorithm - Find the Greatest Common Divisor*)

module Euclid = struct
  let rec euclid (a: int) (b: int): int =
    match (a,b) with
    | (a,b) when b == 0 -> a
    | (a,b) -> euclid (b) (a mod b)

  let () =
    print_endline "Testing Euclid Algorithm";
    assert (euclid (21) (9) = 3);
end

(*Remove Consecutive Duplicate Entries in a List*)
module CompressList = struct
  let rec compress (xs: 'a list): 'a list =
    match xs with
    (*Match the head with the next element and the tail (combined as "t"*)
    (*If the Head matches the next element, chop off the head and recurr with y :: _ (as t) *)
    |x :: (y :: _ as t) -> if x = y then compress t
                            else x :: compress t 
    (*When we finally get a list with no adjacent repeated value, just return that list*)
    | final_list -> final_list 

  let () =
    print_endline "Testing Compress List";
    assert (compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] = ["a"; "b"; "c"; "a"; "d"; "e"])
    
end
